use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, RwLock};

use hover::HoverProvider;
use parsely_diagnostics::{DiagnosticFmt, DiagnosticLevel};
use parsely_gen_asm::module::Module;
use parsely_gen_asm::pack::Pack;
use parsely_lexer::{Lexer, Span};
use parsely_parser::program::Program;
use semantic_tokens::{SemanticGenerator, STOKEN_MODIFIERS, STOKEN_TYPES};

use signature::SignatureProvider;
use tokio::net::TcpListener;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::{lsp_types::*, LanguageServer};
use tower_lsp::{Client, LspService, Server};

mod goto;
mod hover;
mod semantic_tokens;
mod signature;

pub struct Backend {
    documents: RwLock<HashMap<Url, (Module, Program)>>,
    completions: RwLock<HashMap<Url, Vec<(CompletionItem, Span)>>>,

    client: Arc<Client>,
}

const PACK: &str = include_str!("../../../examples/specfile.toml");

fn compile_file(name: &str, input: &str, pack: Arc<Pack>) -> io::Result<(Module, Program)> {
    let tokens = Lexer::run(input.as_bytes());
    let (program, diagnostics) = Program::new(&input, input.to_string(), tokens)
        .parse()
        .unwrap();

    let module = Module::run_new(name, &program, pack, diagnostics).unwrap();

    Ok((module, program))
}

impl Backend {
    fn compile_document(
        pack: Arc<Pack>,
        input: &str,
        path: &Path,
    ) -> (Module, Program, Vec<Diagnostic>) {
        let (module, program) = compile_file(
            path.file_stem().unwrap().to_string_lossy().as_ref(),
            &input,
            pack,
        )
        .unwrap();

        println!("{:?}", program.items);

        let diags = module
            .diagnostics()
            .iter()
            .map(|diag| Diagnostic {
                range: to_rng(&diag.primary_span()),
                severity: {
                    match diag.level() {
                        DiagnosticLevel::Error => Some(DiagnosticSeverity::ERROR),
                        DiagnosticLevel::Warning => Some(DiagnosticSeverity::WARNING),
                        DiagnosticLevel::Info => Some(DiagnosticSeverity::INFORMATION),
                        _ => None,
                    }
                },
                code: None,
                code_description: None,
                source: Some("parsely".to_string()),
                message: DiagnosticFmt(&[diag.clone()]).to_string(),
                related_information: None,
                tags: None,
                data: None,
            })
            .collect::<Vec<_>>();

        println!("{:#?}", diags);

        (module, program, diags)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _p: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: SemanticTokensLegend {
                                token_types: STOKEN_TYPES.to_vec(),
                                token_modifiers: STOKEN_MODIFIERS.to_vec(),
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["=".to_string(), "+".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec!["=".to_string(), "+".to_string()]),

                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: None,
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client
            .log_message(MessageType::INFO, "server semantic!")
            .await;

        let tokens = {
            let document = self.documents.read().unwrap();
            let mut completions = self.completions.write().unwrap();
            let (module, program) = document.get(&params.text_document.uri).unwrap();

            let mut gen = SemanticGenerator::new(module);

            gen.run(&program);

            let (tokens, comps) = gen.finish();

            completions.insert(params.text_document.uri.clone(), comps);

            tokens
        };
        // let tokens = gen.finish();

        self.client
            .log_message(MessageType::INFO, format!("toks: {tokens:?}"))
            .await;

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let Ok(document) = self.documents.read() else {
            return Err(Error::new(ErrorCode::InternalError))
        };

        let Some((module, program)) = document.get(&params.text_document_position_params.text_document.uri) else {
            return Err(Error::new(ErrorCode::InvalidParams))
        };

        let position = parsely_lexer::Position {
            line: params.text_document_position_params.position.line as usize,
            column: params.text_document_position_params.position.character as usize,
        };

        let provder = HoverProvider::new(module);
        let hover = provder.run(program, &position);

        match hover {
            Some(Some(hover)) => Ok(Some(hover)),
            Some(None) => Ok(None), // store in none cache
            None => Ok(None),
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let Ok(document) = self.documents.read() else {
            return Err(Error::new(ErrorCode::InternalError))
        };

        let Some((module, program)) = document.get(&params.text_document_position_params.text_document.uri) else {
            return Err(Error::new(ErrorCode::InvalidParams))
        };

        let position = parsely_lexer::Position {
            line: params.text_document_position_params.position.line as usize,
            column: params.text_document_position_params.position.character as usize,
        };

        let provder = SignatureProvider::new(module);
        let signature = provder.run(program, &position);

        match signature {
            Some(Some(signature)) => Ok(Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: None,
            })),
            Some(None) => Ok(None), // store in none cache
            None => Ok(None),
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let completions = self.completions.read().unwrap();
        let completions = completions
            .get(&params.text_document_position.text_document.uri)
            .unwrap();

        let position = parsely_lexer::Position {
            line: params.text_document_position.position.line as _,
            column: params.text_document_position.position.character as _,
        };

        let vec = completions
            .iter()
            .filter_map(|c| c.1.contains_position(&position).then(|| c.0.clone()))
            .collect();

        Ok(Some(CompletionResponse::Array(vec)))
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn initialized(&self, _p: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "server opwen!")
            .await;

        let pack = parsely_gen_asm::toml::from_str(PACK).unwrap();
        let pack = Arc::new(pack);

        println!("{pack:#?}");

        let (path, diagnostics) = {
            let mut docs = self.documents.write().unwrap();
            let path = params.text_document.uri.clone();

            let path_buf = PathBuf::from_str(path.as_str()).unwrap();

            let (module, program, diagnostics) =
                Backend::compile_document(pack, &params.text_document.text, &path_buf);

            docs.insert(path.clone(), (module, program));

            (path, diagnostics)
        };

        self.client
            .publish_diagnostics(path.clone(), diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "server change!")
            .await;

        let mut diags_accum = Vec::new();

        let path = {
            let mut document = self.documents.write().unwrap();
            let path = params.text_document.uri.clone();

            let (doc_module, doc_program) = document.get_mut(&path).unwrap();
            let path_buf = PathBuf::from_str(path.as_str()).unwrap();

            for change in params.content_changes {
                let (module, program, mut diags) =
                    Backend::compile_document(doc_module.pack.clone(), &change.text, &path_buf);

                diags_accum.append(&mut diags);

                *doc_module = module;
                *doc_program = program;
            }

            path
        };

        self.client
            .publish_diagnostics(path.clone(), diags_accum, None)
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub enum CompletionType {
    Enum(Vec<String>),
    Boolean,
    Symbol(Box<CompletionType>),
    Style,
    Color,
    Rect,
    Unknown,
}

#[tokio::main]
async fn main() {
    let _read = tokio::io::stdin();
    let _write = tokio::io::stdout();

    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let _args = std::env::args();

    let listener = TcpListener::bind("127.0.0.1:5007").await.unwrap();
    println!("Listening");
    let (stream, _) = listener.accept().await.unwrap();
    println!("Connection");

    let (read, write) = tokio::io::split(stream);
    #[cfg(feature = "runtime-agnostic")]
    let (read, write) = (read.compat(), write.compat_write());

    let (service, socket) = LspService::new(|client| {
        let client = Arc::new(client);

        Backend {
            documents: RwLock::new(HashMap::new()),
            completions: RwLock::new(HashMap::new()),
            client,
        }
    });
    Server::new(read, write, socket).serve(service).await;
}

#[inline]
fn to_rng(range: &parsely_lexer::Span) -> Range {
    Range::new(
        Position {
            line: range.start.line as _,
            character: range.start.column as _,
        },
        Position {
            line: range.end.line as _,
            character: range.end.column as _,
        },
    )

    // if range.start == range.end {
    //     Range::new(
    //         Position {
    //             line: range.start.line as _,
    //             character: range.start.column as _,
    //         },
    //         Position {
    //             line: range.start.line as _,
    //             character: range.start.column as u32 + range.len() as u32,
    //         },
    //     )
    // } else {
    //     Range::new(
    //         Position {
    //             line: range.start.line as _,
    //             character: range.start.column as _,
    //         },
    //         Position {
    //             line: range.end.line as _,
    //             character: range.end.column as u32 + range.len() as u32,
    //         },
    //     )
    // }
}

#[inline]
fn range_contains(inner: &Range, outer: &Range) -> bool {
    inner.start.line >= outer.start.line
        && inner.end.line <= outer.end.line
        && inner.start.character >= outer.start.character
        && inner.end.character <= outer.end.character
}
