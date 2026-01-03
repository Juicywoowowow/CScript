//! CScript Compiler CLI
//!
//! Usage: csc <input.csr> [-o output.c]

use clap::Parser;
use colored::Colorize;
use cscript::Driver;
use std::path::PathBuf;
use std::process::ExitCode;

#[derive(Parser, Debug)]
#[command(name = "csc")]
#[command(author = "CScript Team")]
#[command(version = "0.1.0")]
#[command(about = "CScript Compiler - Strongly typed C that transpiles to C", long_about = None)]
struct Args {
    /// Input CScript file (.csr)
    #[arg(value_name = "INPUT")]
    input: PathBuf,

    /// Output C file (defaults to <input>.c)
    #[arg(short, long, value_name = "OUTPUT")]
    output: Option<PathBuf>,

    /// Emit runtime bounds checks in generated C
    #[arg(long, default_value_t = false)]
    bounds_checks: bool,

    /// Print the AST for debugging
    #[arg(long, default_value_t = false)]
    dump_ast: bool,

    /// Print tokens for debugging
    #[arg(long, default_value_t = false)]
    dump_tokens: bool,
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Validate input file extension
    if args.input.extension().map_or(true, |ext| ext != "csr") {
        eprintln!(
            "{}: input file must have .csr extension: {}",
            "error".red().bold(),
            args.input.display()
        );
        return ExitCode::FAILURE;
    }

    // Determine output path
    let output = args.output.unwrap_or_else(|| {
        args.input.with_extension("c")
    });

    // Read source file
    let source = match std::fs::read_to_string(&args.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "{}: could not read file '{}': {}",
                "error".red().bold(),
                args.input.display(),
                e
            );
            return ExitCode::FAILURE;
        }
    };

    // Create driver and compile
    let mut driver = Driver::new(
        args.input.to_string_lossy().to_string(),
        source,
    );

    driver.set_bounds_checks(args.bounds_checks);
    driver.set_dump_ast(args.dump_ast);
    driver.set_dump_tokens(args.dump_tokens);

    match driver.compile() {
        Ok(c_code) => {
            // Write output
            if let Err(e) = std::fs::write(&output, &c_code) {
                eprintln!(
                    "{}: could not write file '{}': {}",
                    "error".red().bold(),
                    output.display(),
                    e
                );
                return ExitCode::FAILURE;
            }

            println!(
                "{}: {} -> {}",
                "compiled".green().bold(),
                args.input.display(),
                output.display()
            );
            ExitCode::SUCCESS
        }
        Err(diagnostics) => {
            // Print all diagnostics
            for diag in &diagnostics {
                eprintln!("{}", diag);
            }

            let error_count = diagnostics.iter()
                .filter(|d| d.is_error())
                .count();

            eprintln!(
                "\n{} {} generated",
                format!("{} error{}", error_count, if error_count == 1 { "" } else { "s" }).red().bold(),
                if diagnostics.len() > error_count {
                    format!(", {} warning{}", 
                        diagnostics.len() - error_count,
                        if diagnostics.len() - error_count == 1 { "" } else { "s" }
                    )
                } else {
                    String::new()
                }
            );

            ExitCode::FAILURE
        }
    }
}
