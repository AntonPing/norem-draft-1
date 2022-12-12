use std::collections::HashMap;
use std::{fmt, ops, sync::Mutex};

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct InternStr {
    index: usize,
}

impl InternStr {
    pub fn new(index: usize) -> InternStr {
        InternStr { index }
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
    pub fn new() -> Interner {
        Interner {
            str_to_idx: HashMap::new(),
            idx_to_str: Vec::new(),
        }
    }

    pub fn intern<S: Into<String>>(&mut self, s: S) -> InternStr {
        let s: String = s.into();
        assert_ne!(s.as_str(),"");
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

#[test]
fn intern_test() {
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
