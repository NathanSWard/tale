pub mod dialogue;
pub mod storage;

pub struct Line;
pub struct Choice;
pub struct Command;

pub enum Suspend<'vm> {
    Line(Line),
    Choice(&'vm Vec<Choice>),
    Command(Command),
}

pub enum Error {}

pub struct VirtualMachine {}

impl VirtualMachine {
    pub fn continue_dialogue(&mut self) -> Result<Suspend, Error> {
        todo!()
    }

    pub fn make_choice(_choice_index: usize) -> Result<(), Error> {
        todo!()
    }
}
