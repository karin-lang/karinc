use crate::data::token::Token;

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

    pub fn leaf(name: String, value: Token) -> AstChild {
        let leaf = AstLeaf::new(name, value);
        AstChild::Leaf(leaf)
    }

    pub fn rename(mut self, name: &str) -> AstChild {
        match &mut self {
            AstChild::Node(node) => node.name = name.to_string(),
            AstChild::Leaf(leaf) => leaf.name = name.to_string(),
        }

        self
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
    pub name: String,
    pub value: Token,
}

impl AstLeaf {
    pub fn new(name: String, value: Token) -> AstLeaf {
        AstLeaf { name, value }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AstVisibility {
    Visible,
    Expanded,
    Hidden,
}
