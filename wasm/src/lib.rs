use std::{cell::RefCell, convert::TryInto};

use upstream::TextDiffConfig;
use wit_bindgen_rust::Handle;

use crate::similar::{Algorithm, Change};

wit_bindgen_rust::export!("similar.wit");

pub struct Similar;

impl crate::similar::Similar for Similar {
    fn diff_lines(alg: Algorithm, old: String, new: String) -> Vec<(similar::ChangeTag, String)> {
        upstream::utils::diff_lines(alg.into(), &old, &new)
            .into_iter()
            .map(|(tag, s)| (tag.into(), s.to_string()))
            .collect()
    }

    fn diff_words(alg: Algorithm, old: String, new: String) -> Vec<(similar::ChangeTag, String)> {
        upstream::utils::diff_words(alg.into(), &old, &new)
            .into_iter()
            .map(|(tag, s)| (tag.into(), s.to_string()))
            .collect()
    }

    fn diff_chars(alg: Algorithm, old: String, new: String) -> Vec<(similar::ChangeTag, String)> {
        upstream::utils::diff_chars(alg.into(), &old, &new)
            .into_iter()
            .map(|(tag, s)| (tag.into(), s.to_string()))
            .collect()
    }

    fn diff_lists(
        alg: Algorithm,
        old: Vec<String>,
        new: Vec<String>,
    ) -> Vec<(similar::ChangeTag, Vec<String>)> {
        upstream::utils::diff_slices(alg.into(), &old, &new)
            .into_iter()
            .map(|(tag, items)| (tag.into(), items.to_vec()))
            .collect()
    }

    fn unified_diff(
        alg: Algorithm,
        old: String,
        new: String,
        context_radius: u32,
        header: Option<(String, String)>,
    ) -> String {
        upstream::udiff::unified_diff(
            alg.into(),
            &old,
            &new,
            context_radius as usize,
            header.as_ref().map(|(l, r)| (l.as_str(), r.as_str())),
        )
    }
}

pub struct Config(RefCell<TextDiffConfig>);

impl crate::similar::Config for Config {
    fn default() -> Handle<Config> {
        Handle::new(Config(RefCell::new(TextDiffConfig::default())))
    }

    fn algorithm(&self, alg: Algorithm) {
        self.0.borrow_mut().algorithm(alg.into());
    }

    fn newline_terminated(&self, yes: bool) {
        self.0.borrow_mut().newline_terminated(yes);
    }

    fn diff_lines(&self, old: String, new: String) -> Handle<TextDiff> {
        Handle::new(TextDiff::new(old, new, |old, new| {
            self.0.borrow().diff_lines(old, new)
        }))
    }

    fn diff_words(&self, old: String, new: String) -> Handle<TextDiff> {
        Handle::new(TextDiff::new(old, new, |old, new| {
            self.0.borrow().diff_words(old, new)
        }))
    }

    fn diff_chars(&self, old: String, new: String) -> Handle<TextDiff> {
        Handle::new(TextDiff::new(old, new, |old, new| {
            self.0.borrow().diff_chars(old, new)
        }))
    }
}

#[ouroboros::self_referencing]
pub struct TextDiff {
    old: String,
    new: String,
    #[borrows(old, new)]
    #[not_covariant]
    diff: upstream::TextDiff<'this, 'this, 'this, str>,
}

impl crate::similar::TextDiff for TextDiff {
    fn algorithm(&self) -> Algorithm {
        self.with_diff(|d| d.algorithm()).into()
    }

    fn newline_terminated(&self) -> bool {
        self.with_diff(|d| d.newline_terminated())
    }

    fn ratio(&self) -> f32 {
        self.with_diff(|d| d.ratio())
    }

    fn changes(&self) -> Vec<Change> {
        self.with_diff(|d| d.iter_all_changes().map(|c| c.into()).collect())
    }
}

impl From<upstream::Algorithm> for similar::Algorithm {
    fn from(value: upstream::Algorithm) -> Self {
        match value {
            upstream::Algorithm::Myers => similar::Algorithm::Myers,
            upstream::Algorithm::Patience => similar::Algorithm::Patience,
            upstream::Algorithm::Lcs => similar::Algorithm::Lcs,
        }
    }
}

impl From<similar::Algorithm> for upstream::Algorithm {
    fn from(value: similar::Algorithm) -> Self {
        match value {
            similar::Algorithm::Myers => upstream::Algorithm::Myers,
            similar::Algorithm::Patience => upstream::Algorithm::Patience,
            similar::Algorithm::Lcs => upstream::Algorithm::Lcs,
        }
    }
}

impl From<upstream::Change<&'_ str>> for similar::Change {
    fn from(value: upstream::Change<&'_ str>) -> Self {
        similar::Change {
            tag: value.tag().into(),
            old_index: value.old_index().map(|ix| ix.try_into().unwrap()),
            new_index: value.new_index().map(|ix| ix.try_into().unwrap()),
            value: value.value().to_string(),
            missing_newline: value.missing_newline(),
        }
    }
}

impl From<upstream::ChangeTag> for similar::ChangeTag {
    fn from(value: upstream::ChangeTag) -> Self {
        match value {
            upstream::ChangeTag::Equal => similar::ChangeTag::Equal,
            upstream::ChangeTag::Delete => similar::ChangeTag::Delete,
            upstream::ChangeTag::Insert => similar::ChangeTag::Insert,
        }
    }
}
