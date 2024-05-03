use color_eyre::{eyre::eyre, eyre::Result, eyre::WrapErr, Section};

use kmonadx::cli::CLI;
use structopt::StructOpt;

use kmonadx::kbdx::{compiler::Compiler, parser::Parser};

use kmonadx::kbdx::diagnostic::DiagnosticAggregator;

use std::cell::UnsafeCell;

// Because std::process::exit terminates the program without running destructors, we need a helper
// function to wrap all the functionality so that there is nothing left to cleanup in the actual
// main function.
// This function returns the exit code.
fn _main() -> Result<usize> {
    let cli = CLI::from_args();

    if cli.filenames.is_empty() {
        CLI::clap().print_long_help()?;
        return Ok(1);
    }

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

    for file_name in &cli.filenames {
        let file_contents = std::fs::read_to_string(&file_name)
            .wrap_err(format!("Unable to read file: {:?}", file_name))
            .suggestion("Please specify a file that exists")?;

        let file_contents = {
            let files_content_ref: &mut Vec<_> = unsafe { &mut *files_content.get() };
            files_content_ref.push(file_contents);
            files_content_ref.last().unwrap()
        };

        let file_id = diagnostics.new_file(
            file_name
                .to_str()
                .ok_or_else(|| eyre!("Filename is not valid unicode!"))?,
            file_contents,
        );

        let file_handle = diagnostics
            .get_file_handle_mut(file_id)
            .expect("Couldn't read newly created file handle!");

        // OPTIMIZE: shouldn't have to read the file before handling this case
        if let Some(ext) = file_name.extension() {
            if ext == "kbd" {
                file_handle.error(format!(
                    "Cannot compile '{:?}'; file already has .kbd extension!",
                    file_name
                ));
                continue;
            }
        }

        let mut parser = Parser::new(file_contents, file_handle);

        if cli.debug_output {
            println!("{:#?}", parser.parse_string_raw())
        } else {
            match Compiler::new(parser) {
                Ok(compiler) => {
                    match compiler.compile_string() {
                        Ok(string) => {
                            let mut file_name = file_name.clone();

                            if !cli.check {
                                // replace kbdx extension with kbd or append .kbd if there is no
                                // extension
                                file_name.set_extension("kbd");
                                std::fs::write(file_name, string)?;
                            }
                        }
                        Err(_) => break,
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
    diagnostics.emit_all_to_stderr()?;

    // now, count number of errors
    let exit_code = if diagnostics.error_count() > 0 { 1 } else { 0 };

    Ok(exit_code)
}

fn main() -> Result<()> {
    let exit_code = _main()?;
    std::process::exit(exit_code as i32)
}
