use super::*;
use crate::frontend::ast::{LitVal, Pattern};

fn remove_nth<T: Clone>(vec: &Vec<T>, j: usize) -> Vec<T> {
    vec[..j]
        .iter()
        .chain(vec[j + 1..].iter())
        .cloned()
        .collect()
}

fn insert_nth<T: Clone>(vec: &Vec<T>, j: usize, vec2: &Vec<T>) -> Vec<T> {
    vec[..j]
        .iter()
        .chain(vec2.into_iter())
        .chain(vec[j + 1..].iter())
        .cloned()
        .collect()
}

#[derive(Debug, Clone)]
pub struct PatnMatrix {
    pub objs: Vec<Ident>,
    pub matrix: Vec<Vec<Pattern>>,
    pub acts: Vec<(Ident, Vec<Ident>)>,
}

impl PatnMatrix {
    pub fn is_empty(&self) -> bool {
        self.matrix.is_empty()
    }
    pub fn first_row_aways_match(&self) -> bool {
        assert!(!self.matrix.is_empty());
        self.matrix[0].iter().all(|p| p.is_wild_or_var())
    }
    pub fn get_row_num(&self) -> usize {
        self.matrix.len()
    }
    pub fn get_col_num(&self) -> usize {
        if self.matrix.is_empty() {
            return 0;
        }
        let res = self.matrix[0].len();
        for row in &self.matrix[1..] {
            assert_eq!(row.len(), res);
        }
        res
    }
    pub fn get_lit_set(&self, j: usize) -> Vec<LitVal> {
        let mut vec = Vec::new();
        for row in self.matrix.iter() {
            match &row[j] {
                Pattern::Lit { lit, .. } => {
                    vec.push(*lit);
                }
                Pattern::Var { .. } => {}
                Pattern::Cons { .. } => {}
                Pattern::Wild { .. } => {}
            }
        }
        vec
    }
    pub fn get_cons_set(&self, j: usize) -> Vec<Ident> {
        let mut vec = Vec::new();
        for row in self.matrix.iter() {
            match &row[j] {
                Pattern::Lit { .. } => {}
                Pattern::Var { .. } => {}
                Pattern::Cons { cons, .. } => {
                    vec.push(*cons);
                }
                Pattern::Wild { .. } => {}
            }
        }
        vec
    }
    // first row always success
    // return (bindings, func, args)
    pub fn success(&self) -> (Vec<(Ident, Ident)>, Ident, Vec<Ident>) {
        let bindings = self.matrix[0]
            .iter()
            .zip(self.objs.iter())
            .flat_map(|(patn, obj)| match patn {
                Pattern::Var { var, .. } => Some((*var, *obj)),
                Pattern::Lit { .. } => unreachable!(),
                Pattern::Cons { .. } => unreachable!(),
                Pattern::Wild { .. } => None,
            })
            .collect();
        let (func, args) = self.acts[0].clone();
        (bindings, func, args)
    }
    // return (new_mat, bindings)
    pub fn default(&self, j: usize) -> (PatnMatrix, Vec<(Ident, Ident)>) {
        let objs = remove_nth(&self.objs, j);
        let mut bindings: Vec<(Ident, Ident)> = Vec::new();
        let (matrix, acts): (Vec<Vec<_>>, Vec<_>) = self
            .matrix
            .iter()
            .zip(self.acts.iter())
            .flat_map(|(row, act)| match &row[j] {
                Pattern::Lit { .. } => None,
                Pattern::Var { var, .. } => {
                    bindings.push((*var, self.objs[j]));
                    let new_row = remove_nth(row, j);
                    Some((new_row, act.clone()))
                }
                Pattern::Cons { .. } => None,
                Pattern::Wild { .. } => {
                    let new_row = remove_nth(row, j);
                    Some((new_row, act.clone()))
                }
            })
            .unzip();
        let new_mat = PatnMatrix { objs, matrix, acts };
        (new_mat, bindings)
    }
    // return (new_mat, bindings)
    pub fn specialize_lit(&self, j: usize, lit: LitVal) -> (PatnMatrix, Vec<(Ident, Ident)>) {
        let objs = remove_nth(&self.objs, j);
        let mut bindings: Vec<(Ident, Ident)> = Vec::new();
        let (matrix, acts): (Vec<Vec<_>>, Vec<_>) = self
            .matrix
            .iter()
            .zip(self.acts.iter())
            .flat_map(|(row, act)| match &row[j] {
                Pattern::Lit { lit: lit2, .. } => {
                    if *lit2 == lit {
                        let new_row = remove_nth(row, j);
                        Some((new_row, act.clone()))
                    } else {
                        None
                    }
                }
                Pattern::Var { var, .. } => {
                    bindings.push((*var, self.objs[j]));
                    let new_row = remove_nth(row, j);
                    Some((new_row, act.clone()))
                }
                Pattern::Cons { .. } => {
                    unreachable!()
                }
                Pattern::Wild { .. } => {
                    let new_row = remove_nth(row, j);
                    Some((new_row, act.clone()))
                }
            })
            .unzip();
        let new_mat = PatnMatrix { objs, matrix, acts };
        (new_mat, bindings)
    }
    // return (new_mat, new_objs, bindings)
    pub fn specialize_cons(
        &self,
        j: usize,
        cons: &Ident,
        arity: usize,
    ) -> (PatnMatrix, Vec<Ident>, Vec<(Ident, Ident)>) {
        let new_objs: Vec<Ident> = (0..arity).map(|_| Ident::generate('o')).collect();
        let objs = insert_nth(&self.objs, j, &new_objs);
        let mut bindings: Vec<(Ident, Ident)> = Vec::new();
        let (matrix, acts): (Vec<Vec<_>>, Vec<_>) = self
            .matrix
            .iter()
            .zip(self.acts.iter())
            .flat_map(|(row, act)| match &row[j] {
                Pattern::Lit { .. } => {
                    unreachable!()
                }
                Pattern::Var { var, span } => {
                    let vec = std::iter::repeat(Pattern::Wild { span: *span })
                        .take(arity)
                        .collect();
                    let new_row = insert_nth(row, j, &vec);
                    bindings.push((*var, self.objs[j]));
                    Some((new_row, act.clone()))
                }
                Pattern::Cons {
                    cons: cons2, pars, ..
                } if *cons2 == *cons => {
                    assert_eq!(pars.len(), arity);
                    let new_row = insert_nth(row, j, &pars);
                    Some((new_row, act.clone()))
                }
                Pattern::Cons { .. } => None,
                Pattern::Wild { span } => {
                    let vec = std::iter::repeat(Pattern::Wild { span: *span })
                        .take(arity)
                        .collect();
                    let new_row = insert_nth(row, j, &vec);
                    Some((new_row, act.clone()))
                }
            })
            .unzip();
        let new_mat = PatnMatrix { objs, matrix, acts };
        (new_mat, new_objs, bindings)
    }
}
