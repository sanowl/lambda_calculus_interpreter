// src/main.rs

mod ast;
mod parser;

use crate::parser::parse_term;
use crate::ast::Term;
use std::io::{self, Write};

fn main() {
    println!("Lambda Calculus Parser and Evaluator REPL");
    println!("Enter lambda expressions to parse and evaluate them.");
    println!("Type `exit` or `quit` to terminate.\n");

    loop {
        print!("λ> ");
        // Flush stdout to ensure the prompt is displayed
        io::stdout().flush().expect("Failed to flush stdout");

        // Read user input
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                if n == 0 {
                    // EOF (e.g., Ctrl+D)
                    println!("\nExiting.");
                    break;
                }
                let trimmed = input.trim();
                if trimmed.eq_ignore_ascii_case("exit") || trimmed.eq_ignore_ascii_case("quit") {
                    println!("Goodbye!");
                    break;
                }
                if trimmed.is_empty() {
                    continue; // Skip empty input
                }

                // Parse the input
                match parse_term(trimmed) {
                    Ok((remaining, term)) => {
                        if !remaining.trim().is_empty() {
                            println!("Warning: Unparsed input remaining: '{}'", remaining);
                        }

                        println!("Parsed Term: {}", term);

                        // Evaluate the term
                        let (reduced, steps) = term.reduce_full();

                        // Determine if reduction occurred
                        if reduced == term {
                            println!("The term is already in normal form.\n");
                        } else {
                            println!("Reduced Term: {}", reduced);
                            println!("Reduction steps:");
                            for (i, step) in steps.iter().enumerate() {
                                println!("Step {}: {}", i, step);
                            }
                            println!();
                        }
                    }
                    Err(e) => {
                        println!("Error parsing input: {:?}", e);
                    }
                }
            }
            Err(error) => {
                println!("Error reading input: {}", error);
                break;
            }
        }
    }
}
