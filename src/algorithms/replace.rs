use crate::algorithms::DiffHook;

/// A [`DiffHook`] that combines deletions and insertions to give blocks
/// of maximal length, and replacements when appropriate.
pub struct Replace<D: DiffHook> {
    d: D,
    del: Option<(usize, usize, usize)>,
    ins: Option<(usize, usize, usize)>,
    eq: Option<(usize, usize, usize)>,
}

impl<D: DiffHook> Replace<D> {
    /// Creates a new replace hook wrapping another hook.
    pub fn new(d: D) -> Self {
        Replace {
            d,
            del: None,
            ins: None,
            eq: None,
        }
    }

    /// Extracts the inner hook.
    pub fn into_inner(self) -> D {
        self.d
    }

    fn flush_eq(&mut self) -> Result<(), D::Error> {
        if let Some((eq_old_index, eq_new_index, eq_len)) = self.eq.take() {
            self.d.equal(eq_old_index, eq_new_index, eq_len)?
        }
        Ok(())
    }

    fn flush_del_ins(&mut self) -> Result<(), D::Error> {
        if let Some((del_old_index, del_old_len, del_new_index)) = self.del.take() {
            if let Some((_, ins_new_index, ins_new_len)) = self.ins.take() {
                self.d
                    .replace(del_old_index, del_old_len, ins_new_index, ins_new_len)?;
            } else {
                self.d.delete(del_old_index, del_old_len, del_new_index)?;
            }
        } else if let Some((ins_old_index, ins_new_index, ins_new_len)) = self.ins.take() {
            self.d.insert(ins_old_index, ins_new_index, ins_new_len)?;
        }
        Ok(())
    }
}

impl<D: DiffHook> AsRef<D> for Replace<D> {
    fn as_ref(&self) -> &D {
        &self.d
    }
}

impl<D: DiffHook> AsMut<D> for Replace<D> {
    fn as_mut(&mut self) -> &mut D {
        &mut self.d
    }
}

impl<D: DiffHook> DiffHook for Replace<D> {
    type Error = D::Error;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), D::Error> {
        self.flush_del_ins()?;

        self.eq = if let Some((eq_old_index, eq_new_index, eq_len)) = self.eq.take() {
            Some((eq_old_index, eq_new_index, eq_len + len))
        } else {
            Some((old_index, new_index, len))
        };

        Ok(())
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), D::Error> {
        if let Some((a, b, c)) = self.eq.take() {
            self.d.equal(a, b, c)?;
        }
        if let Some((del_old_index, del_old_len, del_new_index)) = self.del.take() {
            assert_eq!(old_index, del_old_index + del_old_len);
            self.del = Some((del_old_index, del_old_len + old_len, del_new_index));
        } else {
            self.del = Some((old_index, old_len, new_index));
        }
        Ok(())
    }

    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), D::Error> {
        self.flush_eq()?;
        self.ins = if let Some((ins_old_index, ins_new_index, ins_new_len)) = self.ins.take() {
            debug_assert_eq!(ins_new_index + ins_new_len, new_index);
            Some((ins_old_index, ins_new_index, new_len + ins_new_len))
        } else {
            Some((old_index, new_index, new_len))
        };

        Ok(())
    }

    fn replace(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), D::Error> {
        self.flush_eq()?;
        self.d.replace(old_index, old_len, new_index, new_len)
    }

    fn finish(&mut self) -> Result<(), D::Error> {
        self.flush_eq()?;
        self.flush_del_ins()?;
        self.d.finish()
    }
}

#[test]
fn myers() {
    use crate::algorithms::myers;
    let a: &[&str] = &[
        ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
        "a\n",
        "b\n",
        "c\n",
        "================================\n",
        "d\n",
        "e\n",
        "f\n",
        "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n",
    ];
    let b: &[&str] = &[
        ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
        "x\n",
        "b\n",
        "c\n",
        "================================\n",
        "y\n",
        "e\n",
        "f\n",
        "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n",
    ];

    struct D(Vec<String>);
    impl DiffHook for D {
        type Error = ();
        fn equal(&mut self, o: usize, n: usize, len: usize) -> Result<(), ()> {
            self.0.push(format!("equal {:?} {:?} {:?}", o, n, len));
            Ok(())
        }
        fn delete(&mut self, o: usize, len: usize, new: usize) -> Result<(), ()> {
            self.0.push(format!("delete {:?} {:?} {:?}", o, len, new));
            Ok(())
        }
        fn insert(&mut self, o: usize, n: usize, len: usize) -> Result<(), ()> {
            self.0.push(format!("insert {:?} {:?} {:?}", o, n, len));
            Ok(())
        }
        fn replace(&mut self, o: usize, l: usize, n: usize, nl: usize) -> Result<(), ()> {
            self.0
                .push(format!("replace {:?} {:?} {:?} {:?}", o, l, n, nl));
            Ok(())
        }
    }
    let mut d = Replace::new(D(Vec::new()));
    myers::diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();

    insta::assert_yaml_snapshot!(&d.into_inner().0, @r###"
    ---
    - equal 0 0 1
    - replace 1 1 1 1
    - equal 2 2 3
    - replace 5 1 5 1
    - equal 6 6 3
    "###);
}
