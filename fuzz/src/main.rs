#![allow(clippy::all)]

use afl::fuzz;
use arbitrary::Arbitrary;
use similar::TextDiffConfig;

#[derive(Arbitrary, Debug)]
enum FuzzVariant {
    Chars(Vec<u8>, Vec<u8>),
    Graphemes(Vec<u8>, Vec<u8>),
    Lines(Vec<u8>, Vec<u8>),
    UnicodeWords(Vec<u8>, Vec<u8>),
    Words(Vec<u8>, Vec<u8>),
    Slices(Vec<String>, Vec<String>),
    SlicesBytes(Vec<Vec<u8>>, Vec<Vec<u8>>),
}

#[derive(Arbitrary, Debug)]
struct FuzzOptions {
    config: TextDiffConfig,
    variant: FuzzVariant,
}

fn main() {
    fuzz!(|data: FuzzOptions| {
        match data.variant {
            FuzzVariant::Chars(old, new) => {
                let _ = data.config.diff_chars(&old, &new);
            }
            FuzzVariant::Graphemes(old, new) => {
                let _ = data.config.diff_graphemes(&old, &new);
            }
            FuzzVariant::Lines(old, new) => {
                let _ = data.config.diff_lines(&old, &new);
            }
            FuzzVariant::Slices(old, new) => {
                let _ = data.config.diff_slices::<str>(
                    old.iter().map(|x| &**x).collect::<Vec<_>>().as_slice(),
                    new.iter().map(|x| &**x).collect::<Vec<_>>().as_slice(),
                );
            }
            FuzzVariant::SlicesBytes(old, new) => {
                let _ = data.config.diff_slices::<[u8]>(
                    old.iter()
                        .map(|x| x.as_slice())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    new.iter()
                        .map(|x| x.as_slice())
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            }
            FuzzVariant::UnicodeWords(old, new) => {
                let _ = data.config.diff_unicode_words(&old, &new);
            }
            FuzzVariant::Words(old, new) => {
                let _ = data.config.diff_words(&old, &new);
            }
        };
    });
}
