use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "kmonadx",
    about = "The KMonadX CLI for transpiling kbdx to kbd"
)]
pub struct CLI {
    /// Check for errors without displaying the compiled document
    ///
    /// If any of the input files fail to compile, the program will return with an exit code of "1"
    #[structopt(short, long)]
    pub check: bool,

    /// Show the raw Pest parser output
    ///
    /// This is useful for debugging changes to the parser
    #[structopt(short, long, conflicts_with = "check")]
    pub debug_output: bool,

    /// A list of kbdx files to process.
    ///
    /// The compiled kbd will be written to the same filename except
    /// with the extension replaced with ".kbd"
    pub filenames: Vec<PathBuf>,
}
