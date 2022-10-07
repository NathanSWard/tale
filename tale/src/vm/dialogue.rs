use crate::compiler::instruction::Instruction;
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct Node<TString> {
    pub name: TString,
    pub instructions: Vec<Instruction<TString>>,
    pub labels: HashMap<TString, usize>,
    pub tags: Vec<TString>,
    // TODO: children HashMap<String, NodeWithoutChildren>,
}

#[derive(Debug, Default, Clone)]
pub struct Program<TString> {
    pub nodes: HashMap<TString, Node<TString>>,
}
