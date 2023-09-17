#[derive(Debug, Clone, Copy, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    const DUMMY: NodeId = NodeId(u32::MAX);
}
