use std::collections::HashMap;
use std::{fmt, ops, sync::Mutex};

lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

struct Interner {
    str_to_idx: HashMap<String, usize>,
    idx_to_str: Vec<String>,
}

impl Interner {
    fn new() -> Interner {
        let map = ('a'..='z')
            .enumerate()
            .map(|(i, s)| (s.to_string(), i))
            .collect();
        let vec = ('a'..='z').map(|ch| ch.to_string()).collect();
        Interner {
            str_to_idx: map,
            idx_to_str: vec,
        }
    }

    fn intern<S: Into<String>>(&mut self, s: S) -> InternStr {
        let s: String = s.into();
        // we assume there is no "empty identifier"
        assert_ne!(s.as_str(), "");
        if let Some(idx) = self.str_to_idx.get(&s) {
            InternStr(*idx)
        } else {
            let idx = self.idx_to_str.len();
            self.idx_to_str.push(s.clone());
            self.str_to_idx.insert(s, idx);
            InternStr(idx)
        }
    }

    fn get_str<'a>(&'a self, s: InternStr) -> &'a str {
        &self.idx_to_str[s.0]
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct InternStr(usize);

impl InternStr {
    pub fn new<S: Into<String>>(s: S) -> InternStr {
        INTERNER.lock().unwrap().intern(s)
    }
}

impl ops::Deref for InternStr {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<str> for InternStr {
    fn as_ref(&self) -> &'static str {
        let interner = INTERNER.lock().unwrap();
        let s: &str = interner.get_str(*self);
        // The interner is static global and it never removes a string,
        // so this is safe to extend lifetime to static
        let s: &'static str = unsafe { std::mem::transmute(s) };
        s
    }
}

impl fmt::Debug for InternStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_ref())
    }
}

impl fmt::Display for InternStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

// start from 1, becase 0 is for dummy variables
static mut COUNTER: usize = 1;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub name: InternStr,
    pub index: usize,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.index == 0 {
            write!(f, "{:?}", self.name)
        } else {
            write!(f, "{:?}_{:?}", self.name, self.index)
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.index == 0 {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}_{}", self.name, self.index)
        }
    }
}

impl Ident {
    // a fast single-character variable generator
    // no need to lookup interner
    pub fn generate(ch: char) -> Ident {
        assert!(ch.is_ascii_alphabetic() && ch.is_ascii_lowercase());
        let n = ch as u8 - 'a' as u8;
        let ident = InternStr(n as usize);
        Ident {
            name: ident,
            index: 0,
        }
        .uniquify()
    }

    pub fn is_dummy(&self) -> bool {
        self.index == 0
    }

    pub fn uniquify(&self) -> Ident {
        unsafe {
            let index = COUNTER;
            COUNTER += 1;
            Ident {
                name: self.name,
                index,
            }
        }
    }
}

impl From<InternStr> for Ident {
    fn from(str: InternStr) -> Self {
        Ident {
            name: str,
            index: 0,
        }
    }
}

impl From<Ident> for InternStr {
    fn from(ident: Ident) -> Self {
        ident.name
    }
}

#[test]
fn intern_test() {
    // test function InternStr::new()
    let foo1: &str = "foo";
    let foo2: String = "foo".to_string();
    let bar1: &str = "bar";
    let bar2: String = "bar".to_string();

    let s1 = InternStr::new(foo1);
    let s2 = InternStr::new(foo2);
    let s3 = InternStr::new(bar1);
    let s4 = InternStr::new(bar2);

    assert_eq!(s1, s2);
    assert_eq!(s3, s4);
    assert_ne!(s1, s3);
    assert_ne!(s2, s4);

    assert_eq!(format!("{}", s1), "foo");
    assert_eq!(format!("{}", s2), "foo");
    assert_eq!(format!("{}", s3), "bar");
    assert_eq!(format!("{}", s4), "bar");
}

#[test]
fn uniquify_test() {
    // test function `Ident::uniquify`
    let baz: &str = "baz";
    let s1 = InternStr::new(baz);
    let x1 = Ident::from(s1).uniquify();
    let x2 = Ident::from(s1).uniquify();
    assert_ne!(x1, x2);
    assert_eq!(x1.name, x2.name);

    // test function `Ident::uniquify`, run twice on the same identifier
    let s1 = InternStr::new('x');
    let x1 = Ident::from(s1).uniquify();
    let x2 = x1.uniquify();
    assert_ne!(x1, x2);
    assert_eq!(x1.name, x2.name);

    // test function `Ident::generate`
    let x1 = Ident::generate('x');
    let x2 = Ident::generate('x');
    assert_ne!(x1, x2);
    assert_eq!(x1.name, x2.name);
}
