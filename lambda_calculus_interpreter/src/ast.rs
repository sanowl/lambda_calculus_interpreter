// src/ast.rs

use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

/// Represents a lambda calculus term.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Variable(String),
    Abstraction(String, Rc<Term>),
    Application(Rc<Term>, Rc<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(name) => write!(f, "{}", name),
            Term::Abstraction(param, body) => write!(f, "λ{}. {}", param, body),
            Term::Application(func, arg) => {
                let func_str = match **func {
                    Term::Abstraction(_, _) => format!("({})", func),
                    _ => format!("{}", func),
                };
                let arg_str = match **arg {
                    Term::Application(_, _) | Term::Abstraction(_, _) => format!("({})", arg),
                    _ => format!("{}", arg),
                };
                write!(f, "{} {}", func_str, arg_str)
            }
        }
    }
}

impl Term {
    /// Collects all free variables in the term.
    fn free_vars(&self) -> HashSet<String> {
        match self {
            Term::Variable(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Term::Abstraction(param, body) => {
                let mut set = body.free_vars();
                set.remove(param);
                set
            }
            Term::Application(func, arg) => {
                let mut set = func.free_vars();
                set.extend(arg.free_vars());
                set
            }
        }
    }

    /// Substitutes all free occurrences of `var` with `replacement` in the term.
    fn substitute(&self, var: &str, replacement: &Term) -> Term {
        match self {
            Term::Variable(name) => {
                if name == var {
                    replacement.clone()
                } else {
                    Term::Variable(name.clone())
                }
            }
            Term::Abstraction(param, body) => {
                if param == var {
                    // The variable is bound here; do not substitute.
                    Term::Abstraction(param.clone(), body.clone())
                } else if replacement.free_vars().contains(param) {
                    // Alpha conversion needed to avoid capture.
                    let new_param = generate_unique_var(param, &replacement.free_vars().union(&body.free_vars()).cloned().collect());
                    let new_body = body.substitute(param, &Term::Variable(new_param.clone()));
                    Term::Abstraction(new_param, Rc::new(new_body.substitute(var, replacement)))
                } else {
                    Term::Abstraction(param.clone(), Rc::new(body.substitute(var, replacement)))
                }
            }
            Term::Application(func, arg) => {
                Term::Application(
                    Rc::new(func.substitute(var, replacement)),
                    Rc::new(arg.substitute(var, replacement)),
                )
            }
        }
    }

    /// Performs one step of beta reduction if possible.
    fn reduce_once(&self) -> Option<Term> {
        match self {
            Term::Application(func, arg) => {
                match &**func {
                    Term::Abstraction(param, body) => {
                        // Beta reduction: (λx.M) N => M[x := N]
                        Some(body.substitute(param, arg))
                    }
                    _ => {
                        // Try to reduce the function part
                        if let Some(reduced_func) = func.reduce_once() {
                            Some(Term::Application(Rc::new(reduced_func), arg.clone()))
                        }
                        // Else, try to reduce the argument
                        else if let Some(reduced_arg) = arg.reduce_once() {
                            Some(Term::Application(func.clone(), Rc::new(reduced_arg)))
                        }
                        else {
                            None
                        }
                    }
                }
            }
            Term::Abstraction(param, body) => {
                // Try to reduce the body
                if let Some(reduced_body) = body.reduce_once() {
                    Some(Term::Abstraction(param.clone(), Rc::new(reduced_body)))
                } else {
                    None
                }
            }
            Term::Variable(_) => None,
        }
    }

    /// Reduces the term to its normal form, collecting each reduction step.
    /// Limits the number of reduction steps to prevent infinite loops.
    pub fn reduce_full(&self) -> (Term, Vec<Term>) {
        let mut steps = vec![self.clone()];
        let mut current = self.clone();
        let max_steps = 100; // Adjust as needed
        let mut count = 0;
        while let Some(reduced) = current.reduce_once() {
            if reduced == current {
                // No further reduction possible.
                break;
            }
            steps.push(reduced.clone());
            current = reduced;
            count += 1;
            if count >= max_steps {
                println!("Reached maximum reduction steps.");
                break;
            }
        }
        (current, steps)
    }
}

/// Generates a unique variable name not present in the given set.
fn generate_unique_var(base: &str, used_vars: &HashSet<String>) -> String {
    let mut index = 1;
    loop {
        let new_var = format!("{}{}", base, index);
        if !used_vars.contains(&new_var) {
            return new_var;
        }
        index += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substitution_simple() {
        let term = Term::Variable("x".to_string());
        let replacement = Term::Variable("y".to_string());
        let substituted = term.substitute("x", &replacement);
        assert_eq!(substituted, Term::Variable("y".to_string()));
    }

    #[test]
    fn test_substitution_abstraction_no_capture() {
        let term = Term::Abstraction("x".to_string(), Rc::new(Term::Variable("x".to_string())));
        let replacement = Term::Variable("y".to_string());
        let substituted = term.substitute("x", &replacement);
        assert_eq!(substituted, term);
    }

    #[test]
    fn test_substitution_abstraction_with_capture() {
        let term = Term::Abstraction("x".to_string(), Rc::new(Term::Variable("y".to_string())));
        let replacement = Term::Abstraction("y".to_string(), Rc::new(Term::Variable("x".to_string())));
        let substituted = term.substitute("y", &replacement);
        // Expected: λx1. (λy.x)
        let expected = Term::Abstraction(
            "x1".to_string(),
            Rc::new(Term::Abstraction(
                "y".to_string(),
                Rc::new(Term::Variable("x".to_string())),
            )),
        );
        assert_eq!(substituted, expected);
    }

    #[test]
    fn test_beta_reduction_simple() {
        let term = Term::Application(
            Rc::new(Term::Abstraction(
                "x".to_string(),
                Rc::new(Term::Variable("x".to_string())),
            )),
            Rc::new(Term::Variable("y".to_string())),
        );
        let (reduced, steps) = term.reduce_full();
        assert_eq!(reduced, Term::Variable("y".to_string()));
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[0], term);
        assert_eq!(steps[1], Term::Variable("y".to_string()));
    }

    #[test]
    fn test_beta_reduction_nested() {
        // ((λx.λy.x y) (λz.z)) should reduce to λy.y
        let term = Term::Application(
            Rc::new(Term::Abstraction(
                "x".to_string(),
                Rc::new(Term::Abstraction(
                    "y".to_string(),
                    Rc::new(Term::Application(
                        Rc::new(Term::Variable("x".to_string())),
                        Rc::new(Term::Variable("y".to_string())),
                    )),
                )),
            )),
            Rc::new(Term::Abstraction(
                "z".to_string(),
                Rc::new(Term::Variable("z".to_string())),
            )),
        );
        let (reduced, steps) = term.reduce_full();
        let expected = Term::Abstraction(
            "y".to_string(),
            Rc::new(Term::Variable("y".to_string())),
        );
        assert_eq!(reduced, expected);
        assert_eq!(steps.len(), 3);
        // Step 0: ((λx.λy.x y) (λz.z))
        // Step 1: (λy. (λz.z) y)
        // Step 2: (λy. y)
    }

    #[test]
    fn test_no_reduction() {
        let term = Term::Variable("x".to_string());
        let (reduced, steps) = term.reduce_full();
        assert_eq!(reduced, term);
        assert_eq!(steps.len(), 1);
    }

    #[test]
    fn test_infinite_reduction() {
        // (\x. x x) (\x. x x) leads to infinite reduction
        let term = Term::Application(
            Rc::new(Term::Abstraction(
                "x".to_string(),
                Rc::new(Term::Application(
                    Rc::new(Term::Variable("x".to_string())),
                    Rc::new(Term::Variable("x".to_string())),
                )),
            )),
            Rc::new(Term::Abstraction(
                "x".to_string(),
                Rc::new(Term::Application(
                    Rc::new(Term::Variable("x".to_string())),
                    Rc::new(Term::Variable("x".to_string())),
                )),
            )),
        );
        let (reduced, steps) = term.reduce_full();
        // The reducer should stop after reaching max_steps
        assert_eq!(reduced, term); // After max_steps, it remains the same
        assert!(steps.len() >= 2); // At least two steps
    }
}
