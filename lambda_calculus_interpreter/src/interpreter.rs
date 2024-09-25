use crate::ast::Term;
use std::rc::Rc;

pub fn beta_reduce(term: &Term) -> Term {
    match term {
        Term::Application(func, arg) => {
            if let Term::Abstraction(param, body) = &**func {
                substitute(body, param, arg)
            } else {
                Term::Application(Rc::new(beta_reduce(func)), Rc::new(beta_reduce(arg)))
            }
        }
        Term::Abstraction(param, body) => Term::Abstraction(param.clone(), Rc::new(beta_reduce(body))),
        Term::Variable(_) => term.clone(),
    }
}

fn substitute(term: &Term, param: &str, arg: &Term) -> Term {
    match term {
        Term::Variable(name) if name == param => arg.clone(),
        Term::Variable(_) => term.clone(),
        Term::Abstraction(name, body) if name != param => {
            Term::Abstraction(name.clone(), Rc::new(substitute(body, param, arg)))
        }
        Term::Abstraction(_, _) => term.clone(),
        Term::Application(func, arg2) => Term::Application(
            Rc::new(substitute(func, param, arg)),
            Rc::new(substitute(arg2, param, arg)),
        ),
    }
}
