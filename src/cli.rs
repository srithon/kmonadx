use structopt::StructOpt;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(name = "kmonadx", about = "The KMonadX CLI for transpiling kbdx to kbd")]
pub struct CLI {
    /// Check for errors without displaying the compiled document
    ///
    /// If any of the input files fail to compile, the program will return with an exit code of "1"
    #[structopt(short, long)]
    pub check: bool,
    /// A list of kbdx files to process.
    ///
    /// Unless the stdout option is specified, the kbd will be written to the same filename except
    /// with the extension replaced with ".kbdx"
    pub filenames: Vec<PathBuf>
}
