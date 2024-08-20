use std::sync::mpsc;

#[derive(Clone, Copy)]
pub enum Severity {
  Info,
  Warning,
  Error,
}

impl std::fmt::Display for Severity {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Severity::Info => write!(f, "INFO"),
      Severity::Warning => write!(f, "WARNING"),
      Severity::Error => write!(f, "ERROR"),
    }
  }
}

pub trait Diagnostic {
  fn severity(&self) -> Severity;

  fn message(&self) -> String;

  fn hint(&self) -> Option<String> {
    None
  }
}

#[derive(Clone)]
pub struct Reporter(mpsc::Sender<Box<dyn Diagnostic>>);

impl Reporter {
  pub fn new() -> (Self, mpsc::Receiver<Box<dyn Diagnostic>>) {
    let (tx, rx) = mpsc::channel();
    (Self(tx), rx)
  }

  pub fn report<'a>(&self, diagnostic: impl Diagnostic + 'static) {
    self.0.send(Box::new(diagnostic)).unwrap();
  }

  pub fn to_stdout(receiver: mpsc::Receiver<Box<dyn Diagnostic>>) {
    for diagnostic in receiver.try_iter() {
      println!("[{}]: {}", diagnostic.severity(), diagnostic.message());
      if let Some(hint) = diagnostic.hint() {
        println!("hint:");
        println!("- {}", hint);
      }
    }
  }
}
