use crate::ast::Term;
use crate::interpreter::beta_reduce;

pub fn visualize_reduction(term: &Term) {
    let mut current_term = term.clone();
    let mut step = 0;

    println!("Step-by-step reduction:");
    println!("Step {}: {}", step, current_term);

    while let Term::Application(_, _) = current_term {
        current_term = beta_reduce(&current_term);
        step += 1;
        println!("Step {}: {}", step, current_term);
    }
}
