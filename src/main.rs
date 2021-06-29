use kmonadx::cli::CLI;
use structopt::StructOpt;

use kmonadx::kbdx::{ self, ParseError };

fn main() -> Result<(), ParseError> {
    let cli = CLI::from_args();

    for file_name in cli.filenames {
        let file_contents = std::fs::read_to_string(&file_name).unwrap();
        let compiled_string = kbdx::compile_string(&file_contents)?;

        if !cli.check {
            println!("{:#?}", compiled_string);
        }
    }

    Ok(())
}
