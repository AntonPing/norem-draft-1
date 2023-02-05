use crate::anf::*;
use crate::env_map::*;
use crate::intern::Ident;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct ConstFold {
    atom_map: EnvMap<Ident, Atom>,
    alloc_map: EnvMap<Ident, usize>,
    store_map: EnvMap<(Ident, usize), Atom>,
    offset_map: EnvMap<Ident, (Ident, usize)>,
    ret_stack: Vec<(Ident, MExpr)>,
}

impl ConstFold {
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = ConstFold::new();
        pass.visit_expr(expr)
    }
    fn new() -> ConstFold {
        ConstFold {
            atom_map: EnvMap::new(),
            alloc_map: EnvMap::new(),
            store_map: EnvMap::new(),
            offset_map: EnvMap::new(),
            ret_stack: Vec::new(),
        }
    }
    #[allow(dead_code)]
    fn get_real_addr(&self, var: Ident, index: usize) -> (Ident, usize) {
        self.offset_map
            .get(&var)
            .map(|(var2, index2)| {
                assert!(self.alloc_map.contains_key(&var2));
                assert!(self.alloc_map[&var2] > *index2 + index);
                (*var2, *index2 + index)
            })
            .unwrap_or_else(|| {
                assert!(self.alloc_map.contains_key(&var));
                assert!(self.alloc_map[&var] > index);
                (var, index)
            })
    }
    fn enter_scope(&mut self) {
        self.atom_map.enter_scope();
        self.alloc_map.enter_scope();
        self.store_map.enter_scope();
        self.offset_map.enter_scope();
    }
    fn leave_scope(&mut self) {
        self.atom_map.leave_scope();
        self.alloc_map.leave_scope();
        self.store_map.leave_scope();
        self.offset_map.leave_scope();
    }
    fn visit_brch(&mut self, brch: MExpr) -> MExpr {
        self.enter_scope();
        let res = self.visit_expr(brch);
        self.leave_scope();
        res
    }
    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        self.enter_scope();
        let body = self.visit_expr(body);
        self.leave_scope();
        MDecl { func, pars, body }
    }
    fn visit_arg(&mut self, arg: Atom) -> Atom {
        // substitute until it's constant or no binding
        let mut atom = arg;
        loop {
            if let Atom::Var(sym) = atom {
                if let Some(res) = self.atom_map.get(&sym) {
                    atom = *res
                } else {
                    return atom;
                }
            } else {
                return atom;
            }
        }
    }
    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr.walk_arg(|arg| self.visit_arg(arg));
        let expr = match expr {
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                use Atom::*;
                use UnOpPrim::*;
                match &(prim, arg1) {
                    (Move, arg1) => {
                        self.atom_map.insert(bind, *arg1);
                        return self.visit_expr(*cont);
                    }
                    (INeg, Int(a)) => {
                        self.atom_map.insert(bind, Int(-a));
                        return self.visit_expr(*cont);
                    }
                    _ => {}
                }
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                }
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                use Atom::*;
                use BinOpPrim::*;
                match &(prim, arg1, arg2) {
                    // a + b
                    (IAdd, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a + b));
                        return self.visit_expr(*cont);
                    }
                    // x + 0 = 0 + x = x
                    (IAdd, Var(x), Int(0)) | (IAdd, Int(0), Var(x)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    // a - b
                    (ISub, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a - b));
                        return self.visit_expr(*cont);
                    }
                    // x - 0 = x
                    (ISub, Var(x), Int(0)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    // x - x = 0
                    (ISub, Var(x), Var(y)) if x == y => {
                        self.atom_map.insert(bind, Int(0));
                        return self.visit_expr(*cont);
                    }
                    // a * b
                    (IMul, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a * b));
                        return self.visit_expr(*cont);
                    }
                    // x * 0 = 0 * x = 0
                    (IMul, Var(_x), Int(0)) | (IMul, Int(0), Var(_x)) => {
                        self.atom_map.insert(bind, Int(0));
                        return self.visit_expr(*cont);
                    }
                    // x * 1 = 1 * x = x
                    (IMul, Var(x), Int(1)) | (IMul, Int(1), Var(x)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    _ => {}
                }
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Alloc { bind, size, cont } => {
                // self.alloc_map.insert(bind, size);
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                /*
                let var = arg1.unwrap_var();
                let addr = self.get_real_addr(var, index);
                if let Some(res) = self.store_map.get(&addr) {
                    self.atom_map.insert(bind, *res);
                    return self.visit_expr(*cont);
                }
                */
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                /*
                let var = arg1.unwrap_var();
                let addr = self.get_real_addr(var, index);
                self.store_map.insert(addr, arg2);
                */
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                }
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                if index == 0 {
                    self.atom_map.insert(bind, arg1);
                    return self.visit_expr(*cont);
                } else {
                    /*
                    let var = arg1.unwrap_var();
                    let addr = self.get_real_addr(var, index);
                    self.offset_map.insert(bind, addr);
                    */
                    MExpr::Offset {
                        bind,
                        arg1,
                        index,
                        cont,
                    }
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                if let Atom::Bool(p) = arg1 {
                    self.ret_stack.push((bind, *cont));
                    if p {
                        return self.visit_expr(*brch1);
                    } else {
                        return self.visit_expr(*brch2);
                    }
                } else {
                    MExpr::Ifte {
                        bind,
                        arg1,
                        brch1,
                        brch2,
                        cont,
                    }
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                if let Atom::Int(x) = arg1 {
                    self.ret_stack.push((bind, *cont));
                    for (i, brch) in brchs.into_iter() {
                        if i == x as usize {
                            return self.visit_expr(brch);
                        }
                    }
                    if let Some(dflt) = dflt {
                        return self.visit_expr(*dflt);
                    }
                    panic!("pattern match not exhaustive!");
                } else {
                    MExpr::Switch {
                        bind,
                        arg1,
                        brchs,
                        dflt,
                        cont,
                    }
                }
            }
            MExpr::Retn { arg1 } => {
                if let Some((bind, cont)) = self.ret_stack.pop() {
                    self.atom_map.insert(bind, arg1);
                    return self.visit_expr(cont);
                } else {
                    MExpr::Retn { arg1 }
                }
            }
            other => other,
        };
        expr.walk_brch(|brch| self.visit_brch(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont))
    }
}

fn fix_point<T>(used: HashSet<T>, graph: HashMap<T, HashSet<T>>) -> HashSet<T>
where
    T: std::hash::Hash + Copy + Eq,
{
    let mut set = used;
    let mut new = HashSet::new();
    loop {
        for key in &set {
            if let Some(val) = graph.get(&key) {
                for x in val {
                    if !set.contains(&x) {
                        new.insert(*x);
                    }
                }
            }
        }
        if new.is_empty() {
            return set;
        } else {
            set.extend(new.drain())
        }
    }
}

/*
    Dead-Code Elimination Pass
*/

pub struct DeadElim {
    free_set: FreeSet<Ident>,
    load_map: EnvMap<Ident, HashSet<usize>>,
    ret_used: Vec<bool>,
}

impl DeadElim {
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = DeadElim::new();
        pass.visit_expr(expr)
    }
    fn new() -> DeadElim {
        DeadElim {
            free_set: FreeSet::new(),
            load_map: EnvMap::new(),
            ret_used: vec![true],
        }
    }
    fn enter_scope(&mut self) {
        self.free_set.enter_scope();
        self.load_map.enter_scope();
    }
    fn leave_scope(&mut self) {
        self.free_set.leave_scope();
        self.load_map.leave_scope();
    }
    fn visit_arg(&mut self, arg: Atom) -> Atom {
        if let Atom::Var(var) = arg {
            self.free_set.insert(var);
        }
        arg
    }
    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr.walk_cont(|cont| self.visit_expr(cont));
        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                let used: HashSet<Ident> = decls
                    .iter()
                    .map(|decl| decl.func)
                    .filter(|func| self.free_set.contains(func))
                    .collect();

                let names: HashSet<Ident> = decls.iter().map(|decl| decl.func).collect();

                let (decls, sets): (Vec<MDecl>, Vec<HashSet<Ident>>) = decls
                    .into_iter()
                    .map(|decl| {
                        let MDecl { func, pars, body } = decl;
                        self.enter_scope();
                        let body = self.visit_expr(body);
                        let set = self
                            .free_set
                            .iter()
                            .filter(|var| names.contains(var))
                            .cloned()
                            .collect();
                        self.leave_scope();
                        (MDecl { func, pars, body }, set)
                    })
                    .unzip();

                let graph: HashMap<Ident, HashSet<Ident>> = decls
                    .iter()
                    .map(|decl| decl.func)
                    .zip(sets.into_iter())
                    .collect();

                let reachable = fix_point(used, graph);
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .filter(|decl| reachable.contains(&decl.func))
                    .collect();

                if decls.is_empty() {
                    return *cont;
                } else {
                    MExpr::LetIn { decls, cont }
                }
            }
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                }
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                if self.free_set.contains(&bind) {
                    // todo: pure dead-call elimination
                }
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { arg1 } => {
                if *self.ret_used.last().unwrap() {
                    MExpr::Retn { arg1 }
                } else {
                    MExpr::Retn { arg1: Atom::Unit }
                }
            }
            MExpr::Alloc { bind, size, cont } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                /*
                let var = arg1.unwrap_variable();
                // todo: better api (like `get_mut`) to avoid clone
                let mut set = self.load_map.get(&var).unwrap().clone();
                set.insert(index);
                self.load_map.insert(var, set);
                */
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                /*
                let var = arg1.unwrap_variable();
                let set = self.load_map.get(&var).unwrap();
                if set.is_empty() {
                    return *cont;
                } else {
                    MExpr::Store { arg1, index, arg2, cont }
                }
                */
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                }
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                /*
                let var = arg1.unwrap_variable();
                let set = self.load_map.get(&var)
                    .unwrap().iter()
                    .flat_map(|n| if *n >= index { Some(n - index) } else {None})
                    .collect();
                self.load_map.insert(var, set);
                */
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    self.ret_used.push(false)
                } else {
                    self.ret_used.push(true)
                }
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    self.ret_used.push(false)
                } else {
                    self.ret_used.push(true)
                }
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
        };

        let expr = expr.walk_brch(|brch| {
            self.enter_scope();
            let res = self.visit_expr(brch);
            self.leave_scope();
            res
        });

        match &expr {
            MExpr::Ifte { .. } => {
                self.ret_used.pop();
            }
            MExpr::Switch { .. } => {
                self.ret_used.pop();
            }
            _ => {}
        }

        expr.walk_arg(|arg| self.visit_arg(arg))
    }
}

struct LinearInlineScan {
    // map a unique identifier to (n,m)
    // where n is occur times, and m is call-site occur times
    // n >= m always
    occur: HashMap<Ident, (usize, usize)>,
    set: HashSet<Ident>,
}

impl LinearInlineScan {
    fn new() -> LinearInlineScan {
        LinearInlineScan {
            occur: HashMap::new(),
            set: HashSet::new(),
        }
    }
    fn run(expr: MExpr) -> (MExpr, HashSet<Ident>) {
        let mut pass = LinearInlineScan::new();
        let res = pass.visit_expr(expr);
        (res, pass.set)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr
            .walk_brch(|brch| self.visit_expr(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont))
            .walk_arg(|arg| self.visit_atom(arg));

        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                for decl in decls.iter() {
                    let (n, m) = self.occur.get(&decl.func).unwrap_or(&(0, 0)).clone();
                    assert!(n >= m);
                    if n == 1 && m == 1 {
                        self.set.insert(decl.func);
                    }
                }
                MExpr::LetIn { decls, cont }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let var = func.unwrap_var();
                let (_n, m) = self.occur.get_mut(&var).unwrap();
                *m += 1;
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            other => other,
        };
        expr
    }

    fn visit_atom(&mut self, atom: Atom) -> Atom {
        if let Atom::Var(var) = atom {
            if let Some((n, _m)) = self.occur.get_mut(&var) {
                *n += 1;
            } else {
                self.occur.insert(var, (1, 0));
            }
        }
        atom
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        MDecl { func, pars, body }
    }
}

fn concat(expr: MExpr, bind: Ident, cont: MExpr) -> MExpr {
    match expr {
        MExpr::Retn { arg1 } => MExpr::UnOp {
            bind,
            prim: UnOpPrim::Move,
            arg1,
            cont: Box::new(cont),
        },
        other => other.walk_cont(|expr| concat(expr, bind, cont)),
    }
}

fn inline_call(decl: MDecl, bind: Ident, func: Ident, args: Vec<Atom>, cont: Box<MExpr>) -> MExpr {
    let MDecl {
        func: func2,
        pars,
        body,
    } = decl;
    assert_eq!(func, func2);
    assert_eq!(args.len(), pars.len());
    let new_expr = pars
        .into_iter()
        .zip(args.into_iter())
        .fold(body, |cont, (x, a)| MExpr::UnOp {
            bind: x,
            prim: UnOpPrim::Move,
            arg1: a,
            cont: Box::new(cont),
        });
    concat(new_expr, bind, *cont)
}

struct InlinePerform {
    linear_set: HashSet<Ident>,
    inline_map: HashMap<Ident, MDecl>,
}

impl InlinePerform {
    fn new(linear_set: HashSet<Ident>) -> InlinePerform {
        InlinePerform {
            linear_set,
            inline_map: HashMap::new(),
        }
    }
    fn run(expr: MExpr, linear_set: HashSet<Ident>) -> MExpr {
        let mut pass = InlinePerform::new(linear_set);
        assert!(pass.inline_map.is_empty());
        pass.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .filter_map(|decl| {
                        if self.linear_set.contains(&decl.func) {
                            self.inline_map.insert(decl.func, decl);
                            None
                        } else {
                            Some(decl)
                        }
                    })
                    .collect();
                if decls.is_empty() {
                    return self.visit_expr(*cont);
                } else {
                    MExpr::LetIn { decls, cont }
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let func = func.unwrap_var();
                if self.linear_set.contains(&func) {
                    let decl = self.inline_map.remove(&func).unwrap();
                    let decl = self.visit_decl(decl);
                    inline_call(decl, bind, func, args, cont)
                } else {
                    MExpr::Call {
                        bind,
                        func: Atom::Var(func),
                        args,
                        cont,
                    }
                }
            }
            other => other,
        };
        let expr = expr
            .walk_brch(|brch| self.visit_expr(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont));

        expr
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        MDecl { func, pars, body }
    }
}

pub struct LinearInline;
impl LinearInline {
    pub fn run(expr: MExpr) -> MExpr {
        let (expr, set) = LinearInlineScan::run(expr);
        InlinePerform::run(expr, set)
    }
}

#[test]
fn const_fold_test() {
    use crate::anf::anf_build::*;

    // test arithmetic operation optimization(and move operation)
    let expr1 = chain(vec![
        iadd("x", i(1), i(2)),
        _move("y", v("x")),
        isub("z", v("y"), i(1)),
        imul("r", v("z"), i(3)),
        retn(v("r")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = retn(i(6));
    assert_eq!(expr1, expr2);

    // test if-then-else folding
    let expr1 = chain(vec![
        _move("x", i(42)),
        ifte(
            "j",
            b(false),
            chain(vec![iadd("y", v("x"), i(1)), retn(v("y"))]),
            chain(vec![iadd("z", v("x"), i(2)), retn(v("z"))]),
        ),
        retn(v("j")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = retn(i(44));
    assert_eq!(expr1, expr2);

    // test switch folding
    let expr1 = chain(vec![
        _move("x", i(42)),
        switch(
            "j",
            i(1),
            vec![
                (1, chain(vec![iadd("y1", v("x"), i(1)), retn(v("y1"))])),
                (2, chain(vec![iadd("y2", v("x"), i(2)), retn(v("y2"))])),
                (3, chain(vec![iadd("y3", v("x"), i(3)), retn(v("y3"))])),
            ],
            None,
        ),
        retn(v("j")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = retn(i(43));
    assert_eq!(expr1, expr2);

    /*
    // test alloc, store and offset optimization
    let expr1 = chain(vec![
        alloc("m", 3),
        store(v("m"), 0, i(1)),
        store(v("m"), 1, i(2)),
        store(v("m"), 2, i(3)),
        offset("n", v("m"), 1),
        load("x", v("n"), 0),
        load("y", v("n"), 1),
        iadd("z", v("x"), v("y")),
        retn(v("z")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = chain(vec![
        alloc("m", 3),
        store(v("m"), 0, i(1)),
        store(v("m"), 1, i(2)),
        store(v("m"), 2, i(3)),
        offset("n", v("m"), 1),
        retn(i(5)),
    ]);
    assert_eq!(expr1, expr2);
    */
    /*
    // some critical case
    let expr1 = chain(vec![
        alloc("m", 1),
        ifte(
            "_",
            v("?"),
            chain(vec![
                load("r1", v("m"), 0),
                store(v("m"), 0, i(1)),
                retn(v("r1")),
            ]),
            chain(vec![
                load("r2", v("m"), 0),
                store(v("m"), 0, i(2)),
                retn(v("r2")),
            ]),
        ),
        load("r", v("m"), 0),
        store(v("m"), 0, i(3)),
        retn(v("r")),
    ]);
    let expr1 = ConstFold::run(expr1);
    println!("{expr1}");
    */
}

#[test]
fn dead_elim_test() {
    use crate::anf::anf_build::*;

    // test dead arithmetic operation elimination
    let expr1 = chain(vec![
        iadd("x", i(1), i(1)),
        iadd("y", v("x"), i(1)),
        iadd("z", v("y"), i(1)),
        retn(v("x")),
    ]);
    let expr1 = DeadElim::run(expr1);
    let expr2 = chain(vec![iadd("x", i(1), i(1)), retn(v("x"))]);
    assert_eq!(expr1, expr2);

    // test unused branch result
    let expr1 = chain(vec![
        iadd("x", i(1), i(1)),
        ifte(
            "_",
            v("?"),
            chain(vec![iadd("y", v("x"), i(1)), retn(v("y"))]),
            chain(vec![retn(v("x"))]),
        ),
        retn(v("x")),
    ]);
    let expr1 = DeadElim::run(expr1);
    let expr2 = chain(vec![
        iadd("x", i(1), i(1)),
        ifte(
            "_",
            v("?"),
            chain(vec![retn(unit())]),
            chain(vec![retn(unit())]),
        ),
        retn(v("x")),
    ]);
    assert_eq!(expr1, expr2);

    // test unused function declaration
    let expr1 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![call("r2", "f1", vec![v("x2")]), retn(v("r2"))]),
            ),
            fun(
                "f3",
                vec!["x3"],
                chain(vec![call("r3", "f3", vec![v("x3")]), retn(v("r3"))]),
            ),
            fun("f4", vec!["x4"], chain(vec![retn(v("x4"))])),
        ],
        vec![call("y1", "f1", vec![i(1)]), retn(v("y1"))],
    );
    let expr1 = DeadElim::run(expr1);
    let expr2 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![call("r2", "f1", vec![v("x2")]), retn(v("r2"))]),
            ),
        ],
        vec![call("y1", "f1", vec![i(1)]), retn(v("y1"))],
    );
    assert_eq!(expr1, expr2);
}

#[test]
fn inliner_inline_test() {
    use crate::anf::anf_build::*;
    let expr1 = let_in(
        vec![fun(
            "f1",
            vec!["x1"],
            chain(vec![iadd("r1", v("x1"), i(1)), retn(v("r1"))]),
        )],
        vec![call("r2", "f1", vec![i(42)]), retn(v("r2"))],
    );
    let expr1 = LinearInline::run(expr1);
    let expr2 = chain(vec![
        _move("x1", i(42)),
        iadd("r1", v("x1"), i(1)),
        _move("r2", v("r1")),
        retn(v("r2")),
    ]);
    assert_eq!(expr1, expr2);

    let expr1 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
            ),
        ],
        vec![call("r3", "f1", vec![i(42)]), retn(v("r3"))],
    );

    let expr1 = LinearInline::run(expr1);
    let expr2 = chain(vec![
        _move("x1", i(42)),
        _move("x2", v("x1")),
        iadd("r1", v("x2"), i(1)),
        _move("r2", v("r1")),
        _move("r3", v("r2")),
        retn(v("r3")),
    ]);
    assert_eq!(expr1, expr2);

    let expr1 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
            ),
        ],
        vec![
            call("t1", "f1", vec![i(42)]),
            call("t2", "f2", vec![i(42)]),
            iadd("r3", v("t1"), v("t2")),
            retn(v("r3")),
        ],
    );
    let expr1 = LinearInline::run(expr1);
    let expr2 = let_in(
        vec![fun(
            "f2",
            vec!["x2"],
            chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
        )],
        vec![
            _move("t1", i(42)),
            call("t2", "f2", vec![v("t1")]),
            _move("t3", v("t2")),
            call("t4", "f2", vec![i(42)]),
            iadd("t5", v("t3"), v("t4")),
            retn(v("t5")),
        ],
    );
    assert_eq!(expr1, expr2);
}
