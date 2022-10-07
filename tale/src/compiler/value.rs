use crate::parser::expr::Atom;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value<S = String> {
    Bool(bool),
    Num(f32),
    String(S),
}

impl From<Atom<'_>> for Value {
    fn from(atom: Atom<'_>) -> Self {
        match atom {
            Atom::Bool(b) => Self::Bool(b),
            Atom::Num(n) => Self::Num(n),
            Atom::String(s) => Self::String(s.to_owned()),
            _ => unreachable!("Atom -> Value conversion failure"),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Self {
        Value::Num(f)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}
