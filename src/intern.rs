use std::collections::HashMap;
use std::{fmt, ops, sync::Mutex};

#[derive(Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct InternStr {
    index: usize,
}

impl InternStr {
    pub fn as_dummy(self) -> Unique {
        Unique {
            ident: self,
            index: 0,
        }
    }
    pub fn to_unique(self) -> Unique {
        unsafe {
            let index = COUNTER;
            COUNTER += 1;
            Unique { ident: self, index }
        }
    }
    pub fn is_uppercase(&self) -> bool {
        self.as_ref().chars().nth(0).unwrap().is_ascii_uppercase()
    }
    pub fn is_lowercase(&self) -> bool {
        self.as_ref().chars().nth(0).unwrap().is_ascii_lowercase()
    }
}

pub struct Interner {
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

    pub fn intern<S: Into<String>>(&mut self, s: S) -> InternStr {
        let s: String = s.into();
        assert_ne!(s.as_str(), "");
        if let Some(idx) = self.str_to_idx.get(&s) {
            InternStr { index: *idx }
        } else {
            let idx = self.idx_to_str.len();
            self.idx_to_str.push(s.clone());
            self.str_to_idx.insert(s, idx);
            InternStr { index: idx }
        }
    }

    pub fn get_str<'a>(&'a self, s: InternStr) -> &'a str {
        &self.idx_to_str[s.index]
    }
}

lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

pub fn intern<S: Into<String>>(s: S) -> InternStr {
    INTERNER.lock().unwrap().intern(s)
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
        write!(f, "{}({})", self.as_ref(), self.index)
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
pub struct Unique {
    pub ident: InternStr,
    pub index: usize,
}

impl fmt::Debug for Unique {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}_{}", self.ident, self.index)
    }
}

impl fmt::Display for Unique {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}_{}", self.ident, self.index)
    }
}

impl Unique {
    pub fn generate(ch: char) -> Unique {
        assert!(ch.is_ascii_alphabetic() && ch.is_ascii_lowercase());
        let n = ch as u8 - 'a' as u8;
        let ident = InternStr { index: n as usize };
        ident.to_unique()
    }

    pub fn rename(&self) -> Unique {
        self.ident.to_unique()
    }
}

#[test]
fn intern_test() {
    // test function `intern`
    let foo1: &str = "foo";
    let foo2: String = "foo".to_string();
    let bar1: &str = "bar";
    let bar2: String = "bar".to_string();

    let s1 = intern(foo1);
    let s2 = intern(foo2);
    let s3 = intern(bar1);
    let s4 = intern(bar2);

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
fn unique_test() {
    // test function `Unique::from_intern`
    let baz: &str = "baz";
    let s1 = intern(baz);
    let u1 = s1.to_unique();
    let u2 = s1.to_unique();
    assert_ne!(u1, u2);
    assert_eq!(u1.ident, u2.ident);

    // test function `Unique::generate`
    let s1 = intern('x');
    let u1 = s1.to_unique();
    let u2 = Unique::generate('x');
    assert_ne!(u1, u2);
    assert_eq!(u1.ident, u2.ident);

    // test function `Unique::rename`
    let s1 = intern('x');
    let u1 = s1.to_unique();
    let u2 = u1.rename();
    assert_ne!(u1, u2);
    assert_eq!(u1.ident, u2.ident);
}
