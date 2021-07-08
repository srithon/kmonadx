use color_eyre::{eyre::Result, eyre::WrapErr, Section};
use kmonadx::cli::CLI;
use structopt::StructOpt;

use kmonadx::kbdx::{
    compiler::compile_string, parser::Parser
};

fn main() -> Result<()> {
    let cli = CLI::from_args();

    // https://github.com/yaahc/color-eyre/issues/83
    // We do not want the "Backtrace has been omitted" message to display
    color_eyre::config::HookBuilder::default()
        .display_env_section(false)
        .install()?;

    for file_name in cli.filenames {
        let file_contents = std::fs::read_to_string(&file_name)
            .wrap_err(format!("Unable to read file: {:?}", file_name))
            .suggestion("Please specify a file that exists")?;

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
