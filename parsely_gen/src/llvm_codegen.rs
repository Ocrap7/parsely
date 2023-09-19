use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
};

use inkwell::{
    basic_block::BasicBlock,
    builder::{self, Builder},
    context::Context,
    module::{Linkage, Module},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
    },
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{
        AnyValueEnum, ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue,
        FunctionValue, IntValue, PointerValue, StructValue, VectorValue,
    },
    AddressSpace, FloatPredicate, IntPredicate,
};
use parsely_lexer::{tokens, Tok};
use parsely_parser::{
    expression::{self, Expression, ExpressionKind, Literal},
    item,
    program::Program,
    NodeId,
};

use crate::{
    path::Path,
    resolve::{Environment, TypeResolver},
    ty::{Type, TypeKind},
};

const EMPTY_STR: &'static str = "";

pub struct FunctionContext<'ctx> {
    function: FunctionValue<'ctx>,
    alloca_block: BasicBlock<'ctx>,
    basic_block: BasicBlock<'ctx>,
    builder: Builder<'ctx>,
    locals: HashMap<String, PointerValue<'ctx>>,
    id: NodeId,
}

pub struct CommonContext<'ctx> {
    type_hint: Option<(AnyTypeEnum<'ctx>, Type)>,
    size_hint: Option<usize>,
    is_ref: bool,
}

pub fn new_context() -> Context {
    Context::create()
}

pub struct LlvmCodegen<'ctx> {
    env: Environment,

    node_to_path: HashMap<NodeId, Path>,
    path_to_node: HashMap<Path, NodeId>,

    node_to_type: HashMap<NodeId, Type>,
    // node_to_value_type: HashMap<NodeId, Type>,
    node_to_llvm_type: RefCell<HashMap<NodeId, AnyTypeEnum<'ctx>>>,

    context: &'ctx Context,
    module: Module<'ctx>,

    target: Target,
    target_machine: TargetMachine,
    target_data: TargetData,

    /// Context of currently genning functon.
    /// If this is None, we are in global scope
    fn_ctx: RefCell<Option<FunctionContext<'ctx>>>,
    common_ctx: RefCell<CommonContext<'ctx>>,
}

impl<'ctx> LlvmCodegen<'ctx> {
    pub fn new(resolver: TypeResolver, context: &'ctx Context) -> Self {
        let (target, target_machine, target_data) = Self::default_target();

        Self {
            env: resolver.env,

            node_to_path: resolver.node_to_path,
            path_to_node: resolver.path_to_node,

            node_to_type: resolver.node_to_type,
            // node_to_value_type: resolver.node_to_value_type,
            node_to_llvm_type: RefCell::new(HashMap::new()),

            module: context.create_module("mymod"),
            context,

            target,
            target_machine,
            target_data,

            fn_ctx: RefCell::new(None),
            common_ctx: RefCell::new(CommonContext {
                type_hint: None,
                size_hint: None,
                is_ref: false,
            }),
        }
    }
    fn default_target() -> (Target, TargetMachine, TargetData) {
        Target::initialize_all(&InitializationConfig::default());
        let target = Target::from_triple(&TargetMachine::get_default_triple())
            .expect("Unable to get target");

        let target_machine = target
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name().to_str().unwrap(),
                TargetMachine::get_host_cpu_features().to_str().unwrap(),
                inkwell::OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("Unable to create target machine");

        let target_data = target_machine.get_target_data();
        (target, target_machine, target_data)
    }

    fn fold_elements<'a, T: TryFrom<BasicValueEnum<'ctx>, Error = E>, E: std::fmt::Debug>(
        &self,
        elements: impl Iterator<Item = &'a Expression>,
    ) -> Vec<T> {
        elements
            .map(|e| {
                let value = self.gen_expr(e);

                let ty = value.get_type();

                T::try_from(value).expect("Expected value")
            })
            .collect()
    }

    pub fn finish(self, path: impl AsRef<std::path::Path>) -> String {
        let path = path.as_ref();

        self.module
            .verify()
            .inspect_err(|_| {
                println!("{}", self.module.to_string());
            })
            .unwrap();

        // self.target_machine.write_to_file(&self.module, FileType::Object, &path.with_extension("o")).unwrap();
        self.target_machine
            .write_to_file(&self.module, FileType::Assembly, &path.with_extension("s"))
            .unwrap();

        self.module.to_string()
    }
}

// impl <'ctx> Visitor<AnyValueEnum<'ctx>> for LlvmCodegen<'ctx> {
//     fn default_value(&mut self) -> AnyValueEnum<'ctx> {
//         self.context.void_type().
//     }
// }

// impl<'ctx> Visitor for LlvmCodegen<'ctx> {
//     fn default_value(&mut self) -> () {
//         ()
//     }

//     fn visit_type(&mut self, ty: &parsely_parser::types::Type) -> () {

//     }
// }

impl<'ctx> LlvmCodegen<'ctx> {
    fn builder(&self) -> Ref<Builder<'ctx>> {
        Ref::map(self.fn_ctx.borrow(), |ctx| &ctx.as_ref().unwrap().builder)
    }

    fn with_ref<U>(&self, f: impl Fn(&Self) -> U, is_ref: bool) -> U {
        let old = {
            let mut cctx = self.common_ctx.borrow_mut();
            std::mem::replace(&mut cctx.is_ref, is_ref)
        };

        let val = f(self);

        let mut cctx = self.common_ctx.borrow_mut();
        let _ = std::mem::replace(&mut cctx.is_ref, old);

        val
    }

    pub fn gen_program(&self, program: &Program) {
        for (_, ty) in &self.node_to_type {
            match &ty.kind {
                TypeKind::Struct { .. } => {
                    self.gen_ty(&ty);
                }
                _ => (),
            }
        }

        for item in &program.items {
            self.gen_item(item);
        }
    }

    fn gen_items(&self, items: &[item::Item]) -> Option<AnyValueEnum<'ctx>> {
        for item in &items[..items.len() - 1] {
            self.gen_item(item);
        }

        let last = items.last().expect("Expected last");
        self.gen_item(last)
    }

    fn gen_item(&self, item: &item::Item) -> Option<AnyValueEnum<'ctx>> {
        match &item.kind {
            item::ItemKind::Binding(item::Binding::Function(func)) => {
                self.gen_fn(func, item);

                None
            }
            item::ItemKind::Binding(item::Binding::Value(var))
                if self.fn_ctx.borrow().is_some() =>
            {
                self.gen_local(var, item);
                None
            }
            item::ItemKind::Binding(item::Binding::Value(var)) => {
                self.gen_global(var, item);
                None
            }
            item::ItemKind::Expression(box Expression {
                id,
                kind: ExpressionKind::Assign(assign),
            }) => {
                self.gen_assign(assign, id);
                None
            }
            item::ItemKind::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    let ret_ty = {
                        let fn_ctx = self.fn_ctx.borrow();
                        let fn_ctx = fn_ctx.as_ref().unwrap();

                        let Some(Type {kind: TypeKind::Function {return_ty, ..}}) = self.node_to_type.get(&fn_ctx.id) else {
                            panic!("Expected function type")
                        };

                        (
                            fn_ctx
                                .function
                                .get_type()
                                .get_return_type()
                                .unwrap()
                                .as_any_type_enum(),
                            return_ty.as_ref().clone(),
                        )
                    };

                    let llvm_ret_ty = self.common_ctx.borrow_mut().type_hint.replace(ret_ty);

                    let expr = self.gen_expr(expr);

                    let _ =
                        std::mem::replace(&mut self.common_ctx.borrow_mut().type_hint, llvm_ret_ty);

                    self.builder().build_return(Some(&expr)).unwrap();
                }

                // self.gen_assign(assign, id);
                None
            }
            _ => unimplemented!(),
        }
    }

    fn gen_fn(&self, func: &item::FunctionBinding, item: &item::Item) {
        let path = self
            .node_to_path
            .get(&item.id)
            .expect("Expected fuction path");

        let Some(Type {kind: TypeKind::Function{ params, return_ty, ..}}) = self.node_to_type.get(&item.id) else {
            panic!("Expected function type")
        };

        let llvm_name = path.simple_mangle();

        let ret_ty_llvm = self.gen_ty(return_ty);

        let params: Vec<BasicMetadataTypeEnum<'ctx>> = params
            .iter()
            .map(|(_, ty)| {
                self.gen_ty(ty)
                    .try_into()
                    .expect("Expected basic type for parameter")
            })
            .collect();

        let fty = match ret_ty_llvm {
            AnyTypeEnum::VoidType(ty) => ty.fn_type(&params, false),
            _ => {
                let bbty: BasicTypeEnum = ret_ty_llvm
                    .try_into()
                    .expect("Expected return type to be basic type");

                bbty.fn_type(&params, false)
            }
        };

        let linkage = if item.export_tok.is_some() {
            None
        } else {
            Some(Linkage::Private)
        };

        let function = self.module.add_function(&llvm_name, fty, linkage);

        let alloca_bb = self.context.append_basic_block(function, "allocas");
        let bb = self.context.append_basic_block(function, "entry");

        let builder = self.context.create_builder();

        builder.position_at_end(alloca_bb);

        let local_params: HashMap<_, _> = params
            .iter()
            .zip(func.parameters.value.iter())
            .map(|bv| {
                let bv_ty: BasicTypeEnum = (*bv.0).try_into().unwrap();
                let ptr = builder.build_alloca(bv_ty, EMPTY_STR).unwrap();

                (bv.1.pattern.expect_ident().value.to_string(), ptr)
            })
            .collect();

        builder.position_at_end(bb);

        for ((_, ptr), value) in local_params.iter().zip(function.get_param_iter()) {
            builder.build_store(*ptr, value).unwrap();
        }

        let ctx = FunctionContext {
            function,
            alloca_block: alloca_bb,
            basic_block: bb.clone(),
            builder,
            locals: local_params,
            id: item.id.clone(),
        };

        let _ = self.fn_ctx.borrow_mut().insert(ctx);

        let _ = self.gen_items(&func.value);

        let ctx = self.fn_ctx.take().unwrap();

        ctx.builder.position_at_end(ctx.alloca_block);
        ctx.builder.build_unconditional_branch(bb).unwrap();

        if let Some(item::Item {
            kind: item::ItemKind::Return(_),
            ..
        }) = func.value.last()
        {
            
        }

        ctx.builder.position_at_end(ctx.basic_block);
        ctx.builder.build_return(None).unwrap();
    }

    fn gen_local(&self, var: &item::ValueBinding, item: &item::Item) {
        let ty = self
            .node_to_type
            .get(&var.pattern.id)
            .expect("Expected local type");

        let implicit_ty = if let Type {
            kind: TypeKind::Optional { base },
        } = ty
        {
            Some(self.gen_ty(base))
        } else {
            None
        };

        println!("{ty:?}");
        let llvm_ty: BasicTypeEnum = self.gen_ty(ty).try_into().expect("Expected basic type");

        let alloca = {
            let mut fn_ctx = self.fn_ctx.borrow_mut();
            let fn_ctx = fn_ctx.as_mut().unwrap();

            let name = var.pattern.expect_ident();

            fn_ctx.builder.position_at_end(fn_ctx.alloca_block);
            let alloca = fn_ctx
                .builder
                .build_alloca(llvm_ty, &name.value)
                .expect("Unable to build alloca");
            fn_ctx.builder.position_at_end(fn_ctx.basic_block);

            fn_ctx.locals.insert(name.value.clone(), alloca);

            alloca
        };

        {
            let mut cctx = self.common_ctx.borrow_mut();
            cctx.type_hint = Some((llvm_ty.as_any_type_enum(), ty.clone()));
            // cctx.implicit_type = implicit_ty;
        }

        if let Some((_, init)) = &var.init {
            let expr = self.gen_expr(init);

            let mut fn_ctx = self.fn_ctx.borrow_mut();
            let fn_ctx = fn_ctx.as_mut().unwrap();

            fn_ctx.builder.build_store(alloca, expr).unwrap();
        }

        {
            let mut cctx = self.common_ctx.borrow_mut();
            cctx.type_hint = None;
        }
    }

    fn gen_global(&self, var: &item::ValueBinding, item: &item::Item) {
        let ty = self
            .node_to_type
            .get(&var.pattern.id)
            .expect("Expected global type");
        let llvm_ty: BasicTypeEnum = self.gen_ty(ty).try_into().expect("Expected basic type");

        let name = var.pattern.expect_ident();

        let global = self.module.add_global(llvm_ty, None, &name.value);

        // self.module.set

        // global.set_initializer();
    }

    fn gen_expr(&self, expr: &expression::Expression) -> BasicValueEnum<'ctx> {
        let implicit = {
            let cctx = self.common_ctx.borrow();

            // cctx.type_hint.map(||)
            // cctx.implicit_type.map(|ty| {
            //     let cty = cctx
            //         .type_hint
            //         .expect("This should be some if implicit_ty is some");
            //     let full_ty = self.node_to_type.get(&cty.1).expect("Expected full type");

            //     (ty, full_ty.clone())
            // })
        };

        let expr = match &expr.kind {
            ExpressionKind::Literal(lit) => match lit {
                Literal::Bool(b) => self
                    .context
                    .bool_type()
                    .const_int(if b.value { 1 } else { 0 }, false)
                    .into(),
                Literal::Int(b) => {
                    let cctx = self.common_ctx.borrow();

                    let (ty, signed) =
                       cctx
                        .type_hint
                        .as_ref()
                        .and_then(|(llvm_ty, ty)| {
                            let Type { kind: TypeKind::Integer {signed, ..} | TypeKind::Optional { base: box Type {kind: TypeKind::Integer { signed, .. }} } } = ty else {
                                panic!("Expeteced int type")
                            };

                            let nty = (*llvm_ty).try_into().ok();


                            nty.map(|nty| (nty, *signed))
                        })
                        .unwrap_or_else(|| {
                            (self.context.ptr_sized_int_type(&self.target_data, None), true)
                        });

                    ty.const_int(b.value.value, signed).into()
                }
                Literal::Float(float) => {
                    let cctx = self.common_ctx.borrow();

                    let ty = cctx
                        .type_hint
                        .as_ref()
                        .and_then(|(llvm_ty, _)| (*llvm_ty).try_into().ok())
                        .unwrap_or_else(|| self.context.f64_type());

                    ty.const_float(float.value.value).into()
                }
                Literal::String(str) => {
                    let ty = self
                        .context
                        .i8_type()
                        .array_type(str.value.value.len() as _);
                    let global = self.module.add_global(ty, None, "litstr");

                    let array: Vec<_> = str
                        .value
                        .value
                        .chars()
                        .map(|c| self.context.i8_type().const_int(c as u8 as u64, false))
                        .collect();

                    let init = self.context.i8_type().const_array(&array);
                    global.set_initializer(&init);

                    global.as_pointer_value().into()
                }
                Literal::None(_) => {
                    let cctx = self.common_ctx.borrow();

                    let (_, ty) = cctx.type_hint.as_ref().expect("Expected a type hint");

                    let ty @ Type { kind: TypeKind::Optional {base} } = ty else {
                        panic!("Expeteced optional type")
                    };

                    let llvm_base: BasicTypeEnum =
                        self.gen_ty(base).try_into().expect("Expect basic type");

                    return self
                        .context
                        .const_struct(
                            &[
                                self.context.bool_type().const_zero().into(),
                                llvm_base.const_zero(),
                            ],
                            false,
                        )
                        .into();
                }
            },
            ExpressionKind::AddrOf(addrof) => {
                let old_is_ref = {
                    let mut cctx = self.common_ctx.borrow_mut();
                    let old_is_ref = cctx.is_ref;
                    cctx.is_ref = true;

                    old_is_ref
                };

                let value = self.gen_expr(&addrof.expr);

                if !old_is_ref {
                    let mut cctx = self.common_ctx.borrow_mut();
                    cctx.is_ref = old_is_ref;
                }

                value
            }
            ExpressionKind::Path(_) => {
                let path = self.node_to_path.get(&expr.id).expect("Expected path");
                let sym = self.env.follow_absolute(path).expect("Expected symbol");

                let cctx = self.common_ctx.borrow();

                if cctx.is_ref {
                    let fn_ctx = self.fn_ctx.borrow();
                    let fn_ctx = fn_ctx.as_ref().unwrap();

                    let local = *fn_ctx
                        .locals
                        .get(sym.name.as_ref())
                        .expect("Expected local to exist");

                    local.into()
                } else {
                    let fn_ctx = self.fn_ctx.borrow();
                    let fn_ctx = fn_ctx.as_ref().unwrap();

                    let ty = self
                        .node_to_type
                        .get(sym.node_id.as_ref().unwrap())
                        .expect("Unable to get type");
                    let local = fn_ctx
                        .locals
                        .get(sym.name.as_ref())
                        .expect("Expected local to exist");

                    let llvm_ty: BasicTypeEnum = self.gen_ty(ty).try_into().unwrap();

                    fn_ctx
                        .builder
                        .build_load(llvm_ty, *local, EMPTY_STR)
                        .unwrap()
                        .into()
                }
            }
            ExpressionKind::Deref(deref) => {
                if self.common_ctx.borrow().is_ref {
                    let exp = self.gen_expr(&deref.expr);

                    let fn_ctx = self.fn_ctx.borrow();
                    let fn_ctx = fn_ctx.as_ref().unwrap();

                    let ty = self
                        .node_to_type
                        .get(&expr.id)
                        .expect("Expected deref type");
                    let llvm_ty: BasicTypeEnum = self.gen_ty(ty).try_into().unwrap();

                    fn_ctx
                        .builder
                        .build_load(
                            llvm_ty.ptr_type(AddressSpace::default()),
                            exp.into_pointer_value(),
                            EMPTY_STR,
                        )
                        .unwrap()
                        .into()
                } else {
                    let exp = self.gen_expr(&deref.expr);

                    let fn_ctx = self.fn_ctx.borrow();
                    let fn_ctx = fn_ctx.as_ref().unwrap();

                    let ty = self
                        .node_to_type
                        .get(&expr.id)
                        .expect("Expected deref type");
                    let llvm_ty: BasicTypeEnum = self.gen_ty(ty).try_into().unwrap();

                    fn_ctx
                        .builder
                        .build_load(llvm_ty, exp.into_pointer_value(), EMPTY_STR)
                        .unwrap()
                        .into()
                }
            }
            ExpressionKind::ArrayInit(array) => {
                let ty = self
                    .node_to_type
                    .get(&expr.id)
                    .expect("Expected not to type for array inti");
                let base_ty = ty.index_result();

                let llvm_ty: BasicTypeEnum = self
                    .gen_ty(ty)
                    .try_into()
                    .expect("Expected basic type for array init");

                let llvm_base_ty: BasicTypeEnum = self
                    .gen_ty(&base_ty)
                    .try_into()
                    .expect("Expected basic type for array init base");

                let old_ty_hint = {
                    let mut cctx = self.common_ctx.borrow_mut();
                    let old_hint = cctx
                        .type_hint
                        .replace((llvm_base_ty.as_any_type_enum(), base_ty.clone()));

                    old_hint
                };

                let array_value = match llvm_base_ty {
                    BasicTypeEnum::ArrayType(array_ty) => {
                        let elements =
                            self.fold_elements::<ArrayValue<'ctx>, _>(array.elements.value.iter());

                        array_ty.const_array(&elements)
                    }
                    BasicTypeEnum::IntType(int_ty) => {
                        let elements =
                            self.fold_elements::<IntValue<'ctx>, _>(array.elements.value.iter());

                        int_ty.const_array(&elements)
                    }
                    BasicTypeEnum::FloatType(float_ty) => {
                        let elements =
                            self.fold_elements::<FloatValue<'ctx>, _>(array.elements.value.iter());

                        float_ty.const_array(&elements)
                    }
                    BasicTypeEnum::PointerType(ptr_ty) => {
                        let elements = self
                            .fold_elements::<PointerValue<'ctx>, _>(array.elements.value.iter());

                        ptr_ty.const_array(&elements)
                    }
                    BasicTypeEnum::StructType(struct_ty) => {
                        let elements =
                            self.fold_elements::<StructValue<'ctx>, _>(array.elements.value.iter());

                        struct_ty.const_array(&elements)
                    }
                    BasicTypeEnum::VectorType(vctr_ty) => {
                        let elements =
                            self.fold_elements::<VectorValue<'ctx>, _>(array.elements.value.iter());

                        vctr_ty.const_array(&elements)
                    }
                };

                {
                    let mut cctx = self.common_ctx.borrow_mut();
                    cctx.type_hint = old_ty_hint;
                }

                array_value.into()
            }
            ExpressionKind::Index(base, ind) => {
                let ptr_int = self.context.ptr_sized_int_type(&self.target_data, None);

                let llvm_index = if let Some(ty) = self.node_to_type.get(&ind.value.id) {
                    let value = self.with_ref(|res| res.gen_expr(&ind.value), false);

                    match &ty.kind {
                        TypeKind::Integer {
                            signed: false,
                            size: Some(size),
                        } if size.get() < self.target_data.get_pointer_byte_size(None) * 8 => {
                            let value = self
                                .builder()
                                .build_int_z_extend(value.into_int_value(), ptr_int, EMPTY_STR)
                                .unwrap();

                            value
                        }
                        TypeKind::Integer {
                            signed: false,
                            size: Some(size),
                        } if size.get() > self.target_data.get_pointer_byte_size(None) * 8 => {
                            let value = self
                                .builder()
                                .build_int_truncate(value.into_int_value(), ptr_int, EMPTY_STR)
                                .unwrap();

                            value
                        }
                        TypeKind::Integer { signed: false, .. } => value.into_int_value(),
                        _ => panic!("Expected unsigned integral type"),
                    }
                } else {
                    self.gen_expr(&ind.value).into_int_value()
                };

                let ty = self
                    .node_to_type
                    .get(&base.id)
                    .expect("Expected type for node");
                let llvm_ty: BasicTypeEnum =
                    self.gen_ty(ty).try_into().expect("Expected basic type");

                let ptr = self.with_ref(|res| res.gen_expr(base), true);

                let result = unsafe {
                    self.builder().build_in_bounds_gep(
                        llvm_ty,
                        ptr.into_pointer_value(),
                        &[ptr_int.const_zero(), llvm_index],
                        EMPTY_STR,
                    )
                }
                .unwrap();

                if self.common_ctx.borrow().is_ref {
                    result.into()
                } else {
                    let base_ty = self
                        .node_to_type
                        .get(&expr.id)
                        .expect("Expected index type");
                    let llvm_base_ty: BasicTypeEnum = self
                        .gen_ty(base_ty)
                        .try_into()
                        .expect("Expected basic base type");

                    debug_assert_eq!(base_ty, &ty.index_result());

                    self.builder()
                        .build_load(llvm_base_ty, result, EMPTY_STR)
                        .unwrap()
                        .into()
                }
            }
            ExpressionKind::BinOp(binop) => {
                self.gen_binop(&binop.left, &binop.right, &binop.op, &expr.id)
            }
            ExpressionKind::Parens(expr) => self.gen_expr(&expr.value),
            _ => unimplemented!(),
        };

        let cctx = self.common_ctx.borrow();

        if let Some((
            _,
            Type {
                kind: TypeKind::Optional { .. },
            },
        )) = &cctx.type_hint
        {
            self.context
                .const_struct(
                    &[self.context.bool_type().const_all_ones().into(), expr],
                    false,
                )
                .into()
        } else {
            expr
        }
    }

    /// TODO: When doing +=, stop pointer load from being duplicated
    fn gen_assign(&self, assign: &expression::Assign, id: &NodeId) {
        // Load the left value as a pointer
        let left_ptr = {
            self.common_ctx.borrow_mut().is_ref = true;

            let left_ptr = self.gen_expr(&assign.left).into_pointer_value();

            self.common_ctx.borrow_mut().is_ref = false;

            left_ptr
        };

        // Find the base type of the pointer
        let element_ty = self
            .node_to_type
            .get(&assign.left.id)
            .expect("Expeceted deref type");
        let llvm_base = self.gen_ty(&element_ty);

        // For type coercion
        self.common_ctx.borrow_mut().type_hint = Some((llvm_base, element_ty.clone()));

        let new_val = if let Tok![enum =] = &assign.op {
            self.gen_expr(&assign.right)
        } else {
            self.gen_binop(&assign.left, &assign.right, &assign.bin_op_token(), id)
        };

        self.common_ctx.borrow_mut().type_hint = None;

        let fn_ctx = self.fn_ctx.borrow();
        let fn_ctx = fn_ctx.as_ref().expect("Expected to be in function");

        fn_ctx.builder.build_store(left_ptr, new_val).unwrap();
    }

    fn gen_binop(
        &self,
        left: &expression::Expression,
        right: &expression::Expression,
        op: &tokens::Token,
        id: &NodeId,
    ) -> BasicValueEnum<'ctx> {
        let ty = self.node_to_type.get(id).expect("Expected value type");

        let fn_ctx = self.fn_ctx.borrow();
        let fn_ctx = fn_ctx.as_ref().expect("Expected to be in function");

        let lhs = self.gen_expr(left);
        let rhs = self.gen_expr(right);

        match &ty.kind {
            TypeKind::Integer { signed, .. } => {
                let lhs: IntValue = lhs.try_into().unwrap();
                let rhs: IntValue = rhs.try_into().unwrap();

                match op {
                    Tok![enum +] => fn_ctx
                        .builder
                        .build_int_add(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum -] => fn_ctx
                        .builder
                        .build_int_sub(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum *] => fn_ctx
                        .builder
                        .build_int_mul(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum /] if *signed => fn_ctx
                        .builder
                        .build_int_signed_div(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum /] => fn_ctx
                        .builder
                        .build_int_unsigned_div(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum %] if *signed => fn_ctx
                        .builder
                        .build_int_signed_rem(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum %] => fn_ctx
                        .builder
                        .build_int_unsigned_rem(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum &] => fn_ctx
                        .builder
                        .build_and(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum |] => fn_ctx
                        .builder
                        .build_or(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum ^] => fn_ctx
                        .builder
                        .build_xor(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum <<] => fn_ctx
                        .builder
                        .build_left_shift(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum >>] => fn_ctx
                        .builder
                        .build_right_shift(lhs, rhs, *signed, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum >] if *signed => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum >] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::UGT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum >=] if *signed => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum >=] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::UGE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum <] if *signed => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum <] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::ULT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum <=] if *signed => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum <=] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::ULE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum ==] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum !=] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    _ => panic!(),
                }
            }
            TypeKind::Float { .. } => {
                let lhs: FloatValue = lhs.try_into().unwrap();
                let rhs: FloatValue = rhs.try_into().unwrap();

                match op {
                    Tok![enum +] => fn_ctx
                        .builder
                        .build_float_add(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum -] => fn_ctx
                        .builder
                        .build_float_sub(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum *] => fn_ctx
                        .builder
                        .build_float_mul(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum /] => fn_ctx
                        .builder
                        .build_float_div(lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum >] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::OGT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum >=] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::OGE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum <] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::OLT, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum <=] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::OLE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum ==] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::OEQ, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    Tok![enum !=] => fn_ctx
                        .builder
                        .build_float_compare(FloatPredicate::ONE, lhs, rhs, &EMPTY_STR)
                        .unwrap()
                        .into(),

                    _ => panic!(),
                }
            }
            TypeKind::Bool | TypeKind::Char => {
                let lhs: IntValue = lhs.try_into().unwrap();
                let rhs: IntValue = rhs.try_into().unwrap();

                match op {
                    Tok![enum ==] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, EMPTY_STR)
                        .unwrap()
                        .into(),
                    Tok![enum !=] => fn_ctx
                        .builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, EMPTY_STR)
                        .unwrap()
                        .into(),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn gen_ty(&self, ty: &Type) -> AnyTypeEnum<'ctx> {
        match &ty.kind {
            TypeKind::Integer {
                size: Some(size), ..
            } => self.context.custom_width_int_type(size.get()).into(),
            TypeKind::Integer { .. } => self
                .context
                .ptr_sized_int_type(&self.target_data, None)
                .into(),
            TypeKind::Float { size: Some(size) } => {
                let ty = match size.get() {
                    16 => self.context.f16_type(),
                    32 => self.context.f32_type(),
                    64 => self.context.f64_type(),
                    128 => self.context.f128_type(),
                    _ => panic!("Unsupported float size"),
                };

                ty.into()
            }
            TypeKind::Float { .. } => self.context.f64_type().into(),
            TypeKind::Bool => self.context.bool_type().into(),
            TypeKind::Char => self.context.i8_type().into(),
            TypeKind::Struct { id, .. } => {
                if let Some(ty) = self.node_to_llvm_type.borrow().get(id) {
                    ty.clone()
                } else {
                    let Some(Type { kind:TypeKind::Struct { id, fields }, .. }) = self.node_to_type.get(id) else {
                        panic!("This should be a struct types")
                    };

                    let field_tys: Vec<_> = fields
                        .iter()
                        .map(|(name, ty)| {
                            let ty = self.gen_ty(ty);

                            ty.try_into().expect("This should be a basic type")
                        })
                        .collect();

                    let sty: AnyTypeEnum = self.context.struct_type(&field_tys, false).into();

                    self.node_to_llvm_type
                        .borrow_mut()
                        .insert(id.clone(), sty.clone());

                    sty
                }
            }
            TypeKind::Optional { base } => {
                let llvm_base: BasicTypeEnum =
                    self.gen_ty(base).try_into().expect("Expectyed basic type");

                self.context
                    .struct_type(&[self.context.bool_type().into(), llvm_base], false)
                    .into()
            }
            TypeKind::Ref { base, .. } => {
                let llvm_base: BasicTypeEnum =
                    self.gen_ty(base).try_into().expect("Expectyed basic type");

                llvm_base.ptr_type(AddressSpace::default()).into()
            }
            TypeKind::Array { base, size } => {
                let size = size.expect("Size should be known");

                let base_ty: BasicTypeEnum =
                    self.gen_ty(base).try_into().expect("Expected basic type!");

                // base_ty.
                let array_ty = match base_ty {
                    BasicTypeEnum::ArrayType(arr) => arr.array_type(size as u32),
                    BasicTypeEnum::IntType(arr) => arr.array_type(size as u32),
                    BasicTypeEnum::FloatType(arr) => arr.array_type(size as u32),
                    BasicTypeEnum::PointerType(arr) => arr.array_type(size as u32),
                    BasicTypeEnum::StructType(arr) => arr.array_type(size as u32),
                    BasicTypeEnum::VectorType(arr) => arr.array_type(size as u32),
                };

                array_ty.into()
            }
            TypeKind::Unit => self.context.void_type().into(),
            _ => unimplemented!(),
        }
    }
}
