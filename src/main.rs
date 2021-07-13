use color_eyre::{eyre::eyre, eyre::Result, eyre::WrapErr, Section};

use kmonadx::cli::CLI;
use structopt::StructOpt;

use kmonadx::kbdx::{compiler::compile_string, parser::Parser};

use kmonadx::kbdx::diagnostic::DiagnosticAggregator;

use std::cell::UnsafeCell;

fn main() -> Result<()> {
    let cli = CLI::from_args();

    // https://github.com/yaahc/color-eyre/issues/83
    // We do not want the "Backtrace has been omitted" message to display
    color_eyre::config::HookBuilder::default()
        .display_env_section(false)
        .install()?;

    let mut diagnostics = DiagnosticAggregator::default();

    // We need to use an UnsafeCell to hold our Vec because we need to have immutable references to
    // the contained elements while also being able to append new elements to the list.
    // Since we explicitly created the Vec with the necessary capacity, this will not result in any
    // errors
    let files_content = UnsafeCell::new(Vec::with_capacity(cli.filenames.len()));

    for file_name in cli.filenames {
        let file_contents = std::fs::read_to_string(&file_name)
            .wrap_err(format!("Unable to read file: {:?}", file_name))
            .suggestion("Please specify a file that exists")?;

        let file_contents = {
            let files_content_ref: &mut Vec<_> = unsafe { &mut *files_content.get() };
            files_content_ref.push(file_contents);
            files_content_ref.last().unwrap()
        };

        let file_handle = diagnostics.new_file(
            file_name
                .to_str()
                .ok_or_else(|| eyre!("Filename is not valid unicode!"))?
                .to_owned(),
            file_contents,
        );

        let mut parser = Parser::new(file_contents, file_handle);

        if cli.debug_output {
            println!("{:#?}", parser.parse_string_raw())
        } else {
            match compile_string(parser) {
                Ok(compiled_string) => {
                    if !cli.check {
                        println!("{}", compiled_string);
                    }
                }
                Err(_) => break,
            }
        }
    }

    // https://doc.rust-lang.org/std/process/fn.exit.html
    //
    // QUOTE
    // Note that because this function never returns, and that it terminates the process, no
    // destructors on the current stack or any other thread’s stack will be run. If a clean
    // shutdown is needed it is recommended to only call this function at a known point where there
    // are no more destructors left to run.
    // END QUOTE
    let exit_code: usize = diagnostics.emit_all()?.into();
    eprintln!("Exit code would be {}", exit_code);

    Ok(())
}
