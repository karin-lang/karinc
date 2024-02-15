use crate::new::data::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub root: AstNode,
}

impl Ast {
    pub fn new(root: AstNode) -> Ast {
        Ast { root }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstChild {
    Node(AstNode),
    Leaf(AstLeaf),
}

impl AstChild {
    pub fn node(name: String, children: Vec<AstChild>) -> AstChild {
        let node = AstNode::new(name, children);
        AstChild::Node(node)
    }

    pub fn leaf(value: Token) -> AstChild {
        let leaf = AstLeaf::new(value);
        AstChild::Leaf(leaf)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstNode {
    pub name: String,
    pub children: Vec<AstChild>,
}

impl AstNode {
    pub fn new(name: String, children: Vec<AstChild>) -> AstNode {
        AstNode { name, children }
    }
}

impl Default for AstNode {
    fn default() -> Self {
        AstNode::new(String::new(), Vec::new())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstLeaf {
    pub value: Token,
}

impl AstLeaf {
    pub fn new(value: Token) -> AstLeaf {
        AstLeaf { value }
    }
}
