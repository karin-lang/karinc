pub mod token;
pub mod tokenize;

#[derive(Clone, Debug, PartialEq)]
pub struct SourceTree<'a> {
    pub hakos: Vec<HakoSource<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HakoSource<'a> {
    pub mods: Vec<(usize, ModSource<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModSource<'a> {
    pub id: String,
    pub src: &'a str,
    pub submods: Vec<(usize, ModSource<'a>)>,
}
