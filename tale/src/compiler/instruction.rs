use super::value::Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Instruction<TString> {
    Push(Value<TString>),
    Pop,
    Load(TString),
}
