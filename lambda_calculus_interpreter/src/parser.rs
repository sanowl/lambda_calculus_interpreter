// src/parser.rs

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0, multispace1},
    combinator::map,
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple, pair},
    IResult, error::VerboseError,
};
use std::rc::Rc;

use crate::ast::Term;

/// Parses a variable (e.g., x, var1).
fn parse_variable(input: &str) -> IResult<&str, Term, VerboseError<&str>> {
    map(
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        |name: &str| Term::Variable(name.to_string()),
    )(input)
}

/// Parses an abstraction with multiple parameters (e.g., λx y. x y).
fn parse_abstraction(input: &str) -> IResult<&str, Term, VerboseError<&str>> {
    let (input, _) = alt((tag("λ"), tag("\\")))(input)?;
    let (input, params) = many1(preceded(multispace0, take_while1(|c: char| c.is_alphanumeric() || c == '_'))))(input)?;
    let (input, _) = preceded(multispace0, char('.'))(input)?;
    let (input, body) = preceded(multispace0, parse_term)(input)?;

    // Nest the abstractions
    let term = params.into_iter().rev().fold(body, |acc, param| {
        Term::Abstraction(param.to_string(), Rc::new(acc))
    });

    Ok((input, term))
}

/// Parses an application (e.g., f x y).
fn parse_application(input: &str) -> IResult<&str, Term, VerboseError<&str>> {
    map(
        pair(parse_atom, many0(preceded(multispace1, parse_atom))),
        |(first, rest)| {
            rest.into_iter().fold(first, |acc, term| {
                Term::Application(Rc::new(acc), Rc::new(term))
            })
        },
    )(input)
}

/// Parses an atomic term: variable, abstraction, or parenthesized term.
fn parse_atom(input: &str) -> IResult<&str, Term, VerboseError<&str>> {
    alt((
        parse_abstraction,
        parse_variable,
        delimited(
            preceded(multispace0, char('(')),
            parse_term,
            preceded(multispace0, char(')')),
        ),
    ))(input)
}

/// Parses a lambda calculus term.
pub fn parse_term(input: &str) -> IResult<&str, Term, VerboseError<&str>> {
    preceded(multispace0, parse_application)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Term;

    #[test]
    fn test_parse_variable() {
        let input = "x";
        let expected = Term::Variable("x".to_string());
        let result = parse_variable(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_abstraction_single_param() {
        let input = "λx.x";
        let expected = Term::Abstraction(
            "x".to_string(),
            Rc::new(Term::Variable("x".to_string())),
        );
        let result = parse_abstraction(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_abstraction_multiple_params() {
        let input = "λx y. x y";
        let expected = Term::Abstraction(
            "x".to_string(),
            Rc::new(Term::Abstraction(
                "y".to_string(),
                Rc::new(Term::Application(
                    Rc::new(Term::Variable("x".to_string())),
                    Rc::new(Term::Variable("y".to_string())),
                )),
            )),
        );
        let result = parse_abstraction(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_abstraction_with_backslash() {
        let input = "\\y. y";
        let expected = Term::Abstraction(
            "y".to_string(),
            Rc::new(Term::Variable("y".to_string())),
        );
        let result = parse_abstraction(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_application() {
        let input = "(λx.x) y";
        let expected = Term::Application(
            Rc::new(Term::Abstraction(
                "x".to_string(),
                Rc::new(Term::Variable("x".to_string())),
            )),
            Rc::new(Term::Variable("y".to_string())),
        );
        let result = parse_term(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_nested_application() {
        let input = "a b c";
        let expected = Term::Application(
            Rc::new(Term::Application(
                Rc::new(Term::Variable("a".to_string())),
                Rc::new(Term::Variable("b".to_string())),
            )),
            Rc::new(Term::Variable("c".to_string())),
        );
        let result = parse_term(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_complex_term() {
        let input = "(λx.λy.x y) (λz.z)";
        let expected = Term::Application(
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
        let result = parse_term(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_display_term() {
        let input = "(λx.λy.x y) (λz.z)";
        let parsed = parse_term(input).expect("Failed to parse term").1;
        let display = format!("{}", parsed);
        assert_eq!(display, "(λx. λy. x y) (λz. z)");
    }
}
