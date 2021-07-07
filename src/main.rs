use kmonadx::cli::CLI;
use structopt::StructOpt;

use kmonadx::kbdx::{
    compiler::compile_string, parser::ParseError, parser::Parser
};

fn main() -> Result<(), ParseError> {
    let cli = CLI::from_args();

    for file_name in cli.filenames {
        let file_contents = std::fs::read_to_string(&file_name).unwrap();

        if cli.debug_output {
            println!("{:#?}", Parser::parse_string_raw(&file_contents))
        }
        else {
            let compiled_string = compile_string(&file_contents)?;

            if !cli.check {
                println!("{}", compiled_string);
            }
        }
    }

    Ok(())
}
