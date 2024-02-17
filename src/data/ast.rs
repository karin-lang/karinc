use crate::data::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub root: AstNode,
}

impl Ast {
    pub fn new(root: AstNode) -> Ast {
        Ast { root }
    }

    pub fn normalize(mut self) -> Ast {
        self.root = self.root.normalize();
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstChild {
    Node(AstNode),
    Leaf(AstLeaf),
}

impl AstChild {
    #[inline]
    pub fn node(name: String, children: Vec<AstChild>) -> AstChild {
        let node = AstNode::new(name, children);
        AstChild::Node(node)
    }

    #[inline]
    pub fn leaf(name: String, value: Token) -> AstChild {
        let leaf = AstLeaf::new(name, value);
        AstChild::Leaf(leaf)
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match self {
            AstChild::Node(node) => &node.name,
            AstChild::Leaf(leaf) => &leaf.name,
        }
    }

    #[inline]
    pub fn rename(mut self, name: &str) -> AstChild {
        match &mut self {
            AstChild::Node(node) => node.name = name.to_string(),
            AstChild::Leaf(leaf) => leaf.name = name.to_string(),
        }

        self
    }

    #[inline]
    pub fn to_node(&self) -> Option<&AstNode> {
        match self {
            AstChild::Node(node) => Some(node),
            _ => None,
        }
    }

    #[inline]
    pub fn to_leaf(&self) -> Option<&AstLeaf> {
        match self {
            AstChild::Leaf(leaf) => Some(leaf),
            _ => None,
        }
    }

    #[inline]
    pub fn expect_node(&self) -> &AstNode {
        self.to_node().unwrap()
    }

    #[inline]
    pub fn expect_leaf(&self) -> &AstLeaf {
        self.to_leaf().unwrap()
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

    pub fn normalize(mut self) -> AstNode {
        let mut normalized_children = Vec::new();

        for each_child in self.children {
            match each_child {
                AstChild::Node(node) => {
                    let mut normalized_node = node.normalize();

                    if normalized_node.name == "" {
                        normalized_children.append(&mut normalized_node.children);
                    } else {
                        normalized_children.push(AstChild::Node(normalized_node));
                    }
                },
                _ => normalized_children.push(each_child),
            }
        }

        self.children = normalized_children;
        self
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&AstChild> {
        self.children.get(index)
    }

    pub fn find(&self, id: &str) -> Option<&AstChild> {
        for each_child in &self.children {
            if id == each_child.get_name() {
                return Some(each_child);
            }
        }

        None
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
