/// UHAC (v0.0.2)
/// HUM What: Interpreter/Parser runner for GML. Runs file or REPL.
/// HUM LICENCE: MIT.
/// HUM Agent: Aryan Suri.
/// LLM Instructions: Provide functional code; Do not add comments.
/// LLM Vendor: Multiple.
/// LLM Model: GPT-4o; Claude 3.5 Sonnet; Gemini 1.5 Pro.
/// LLM Only: False.
pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod tokens;

use std::env; // To access command line arguments
use std::fs; // To read files
use std::io::{self, Write};
use std::path::Path;
use std::process; // To exit with a status code

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        // No arguments provided (just the program name) -> Run REPL
        1 => {
            run_repl();
        }
        // One argument provided (program name + filename) -> Run File
        2 => {
            let filename = &args[1];
            run_file(filename);
        }
        // Too many arguments -> Show usage
        _ => {
            // Use program name from args[0] if available, otherwise fallback
            let program_name = args
                .get(0)
                .map(Path::new)
                .and_then(Path::file_name)
                .and_then(|s| s.to_str())
                .unwrap_or("gml_interpreter"); // Fallback name

            eprintln!("Usage: {} [filename]", program_name);
            eprintln!("       (run REPL if filename is omitted)");
            process::exit(1); // Exit with error code
        }
    }
}

// --- File Processing Logic ---

/// Reads and parses a GML source file.
fn run_file(filename: &str) {
    match fs::read_to_string(filename) {
        Ok(contents) => {
            // Successfully read the file content
            parse_and_print(&contents, false); // Don't use REPL prefixes for file output
        }
        Err(e) => {
            // Failed to read the file
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1); // Exit with error code
        }
    }
}

// --- REPL Logic ---

/// Runs the interactive Read-Eval-Print Loop.
fn run_repl() {
    // Clear the screen and set the cursor to the top left
    // Comment this out if you prefer not to clear the screen on start
    // println!("\x1b[2J\x1b[H");
    println!("GML REPL started. Type 'help', 'clear', or 'exit'. End expressions with ';;'.");

    let mut input_buffer = String::new();
    let mut is_multiline = false;

    loop {
        if is_multiline {
            print!("... "); // Prompt for continuation
        } else {
            print!(">>> "); // Standard prompt
        }
        io::stdout().flush().unwrap(); // Ensure prompt appears immediately

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            eprintln!("Error reading input. Exiting.");
            break; // Exit on read error
        }

        let trimmed = line.trim();

        // Handle REPL commands
        match trimmed {
            "exit" => {
                println!("Exiting REPL...");
                break;
            }
            "clear" => {
                println!("\x1b[2J\x1b[H"); // ANSI escape code to clear screen
                input_buffer.clear(); // Clear any pending input
                is_multiline = false;
                continue;
            }
            "help" => {
                println!("GML REPL Help:");
                println!("  - Enter GML code ending with ';;'.");
                println!("  - Multiline input is supported, finish with ';;'.");
                println!("  - `help`   : Show this help message.");
                println!("  - `clear`  : Clear the screen.");
                println!("  - `exit`   : Exit the REPL.");
                input_buffer.clear(); // Don't process help as code
                is_multiline = false;
                continue;
            }
            "" if !is_multiline => {
                // Ignore empty lines unless in multiline mode
                continue;
            }
            _ => {} // Process as code input
        }

        // Append input, removing leading/trailing whitespace from the line
        if !input_buffer.is_empty() {
            input_buffer.push(' '); // Separate lines with a space for the parser
        }
        input_buffer.push_str(trimmed);

        // Check if the buffer ends with the termination sequence ;;
        if input_buffer.ends_with(";;") {
            // Parse the complete input
            parse_and_print(&input_buffer, true); // Use REPL prefixes

            input_buffer.clear(); // Reset buffer for next command
            is_multiline = false; // Reset multiline state
        } else if !trimmed.is_empty() {
            // Input doesn't end with ;; and isn't empty, continue multiline input
            is_multiline = true;
        } else {
            // Empty line entered during multiline input - treat as completed?
            // Or just keep waiting? Let's assume keep waiting.
            is_multiline = true;
        }
    }
}

// --- Common Parsing and Printing Logic ---

/// Parses the given source string and prints the AST or errors.
/// `use_repl_prefixes` controls whether ">>>" / "..." are used for output.
fn parse_and_print(source: &str, use_repl_prefixes: bool) {
    if source.trim().is_empty() {
        return; // Ignore empty input
    }

    let l = lexer::Lexer::new(source);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    if !p.errors.is_empty() {
        // Print errors to stderr
        println_output(
            if use_repl_prefixes { "ERR" } else { "" }, // Prefix for errors
            &format!("Parser Errors:\n{:#?}", p.errors),
            use_repl_prefixes,
        );
    } else {
        // Print successful AST to stdout
        println_output(
            if use_repl_prefixes { "AST" } else { "" }, // Prefix for AST
            &format!("{:#?}", program),
            use_repl_prefixes,
        );
    }
}

// --- Helper for conditional REPL-style output ---

/// Prints output, optionally using REPL prefixes.
/// Errors go to stderr, success to stdout.
fn println_output(prefix: &str, text: &str, use_repl_prefixes: bool) {
    let mut lines = text.lines();
    let first_prefix = if use_repl_prefixes {
        format!("{} ", prefix)
    } else {
        "".to_string()
    };
    let subsequent_prefix = if use_repl_prefixes { "... " } else { "" }.to_string();

    // Determine output stream based on prefix (simple heuristic)
    let mut stream: Box<dyn Write> = if prefix == "ERR" {
        Box::new(io::stderr())
    } else {
        Box::new(io::stdout())
    };

    if let Some(first) = lines.next() {
        writeln!(stream, "{}{}", first_prefix, first).ok();
    }
    for line in lines {
        writeln!(stream, "{}{}", subsequent_prefix, line).ok();
    }
    stream.flush().ok();
}
