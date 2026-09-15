#[cfg(test)]
use alloc::vec;
use alloc::vec::Vec;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::ops::{Add, Index, Range};

use crate::types::int_key_map_with_capacity;

/// A small bucket that stores its first value inline.
///
/// Most hash buckets in diff inputs contain a single value.  Using a `Vec` for
/// every bucket adds one heap allocation per distinct input item, so the common
/// one-value case lives directly in the map instead.
pub(crate) struct HashBucket<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> HashBucket<T> {
    pub(crate) fn new(first: T) -> Self {
        Self {
            first,
            rest: Vec::new(),
        }
    }

    pub(crate) fn push(&mut self, value: T) {
        self.rest.push(value);
    }

    pub(crate) fn len(&self) -> usize {
        self.rest.len() + 1
    }

    pub(crate) fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        core::iter::once(&self.first).chain(self.rest.iter())
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        core::iter::once(&mut self.first).chain(self.rest.iter_mut())
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = T> {
        core::iter::once(self.first).chain(self.rest)
    }
}

/// Utility function to check if a range is empty that works on older rust versions
#[inline(always)]
#[allow(clippy::neg_cmp_op_on_partial_ord)]
pub fn is_empty_range<T: PartialOrd<T>>(range: &Range<T>) -> bool {
    !(range.start < range.end)
}

/// Multiplies two words and folds the 128-bit product back into 64 bits.
///
/// Both halves of the product depend on all input bits, so folding them
/// together produces a result whose low and high bits are well mixed.  This
/// is only suitable for mixing keys that are already hashes or dense ids;
/// content hashing uses SipHash below.
#[cfg(any(feature = "std", feature = "hashbrown", test))]
#[inline(always)]
pub(crate) fn folded_multiply(a: u64, b: u64) -> u64 {
    let full = (a as u128).wrapping_mul(b as u128);
    (full as u64) ^ ((full >> 64) as u64)
}

#[inline(always)]
fn read_u64(bytes: &[u8]) -> u64 {
    u64::from_le_bytes(bytes[..8].try_into().unwrap())
}

/// Returns the SipHash key used for content hashing.
///
/// Diff inputs may be untrusted, and the hash buckets in [`IdentifyDistinct`]
/// and friends degrade to linear scans on collisions.  A per-process random
/// key makes it impossible to craft colliding inputs ahead of time.  Without
/// `std` there is no entropy source, so a fixed key is used; SipHash has no
/// known practical collision families even with a public key, which is
/// still far better than the trivially invertible multiplicative hashes.
#[inline]
fn content_hash_keys() -> (u64, u64) {
    #[cfg(feature = "std")]
    {
        use std::hash::{BuildHasher, RandomState};
        use std::sync::OnceLock;

        static KEYS: OnceLock<(u64, u64)> = OnceLock::new();
        *KEYS.get_or_init(|| {
            let state = RandomState::new();
            (state.hash_one(0u64), state.hash_one(1u64))
        })
    }
    #[cfg(not(feature = "std"))]
    {
        (0x243F_6A88_85A3_08D3, 0x1319_8A2E_0370_7344)
    }
}

/// SipHash-1-3 for deduplicating diff input items.
///
/// Diff inputs are hashed once per item and only compared by hash within a
/// single invocation, but the inputs may be untrusted and the buckets keyed
/// by these hashes degrade to linear scans on collisions.  Ad-hoc
/// multiplicative mixers are cheap but admit collision families that can be
/// solved for algebraically; SipHash is the vetted choice for exactly this
/// situation (it is what `std`'s `HashMap` uses) and is still several times
/// faster than the previous byte-at-a-time FNV loop because it consumes
/// eight bytes per round.
#[derive(Clone, Copy)]
pub(crate) struct ContentHasher {
    v0: u64,
    v1: u64,
    v2: u64,
    v3: u64,
    /// Unprocessed trailing bytes, little endian.
    tail: u64,
    /// Number of valid bytes in `tail`.
    ntail: usize,
    length: usize,
}

impl ContentHasher {
    #[inline]
    fn new(k0: u64, k1: u64) -> Self {
        ContentHasher {
            v0: k0 ^ 0x736f_6d65_7073_6575,
            v1: k1 ^ 0x646f_7261_6e64_6f6d,
            v2: k0 ^ 0x6c79_6765_6e65_7261,
            v3: k1 ^ 0x7465_6462_7974_6573,
            tail: 0,
            ntail: 0,
            length: 0,
        }
    }

    #[inline(always)]
    fn sip_round(&mut self) {
        self.v0 = self.v0.wrapping_add(self.v1);
        self.v1 = self.v1.rotate_left(13);
        self.v1 ^= self.v0;
        self.v0 = self.v0.rotate_left(32);
        self.v2 = self.v2.wrapping_add(self.v3);
        self.v3 = self.v3.rotate_left(16);
        self.v3 ^= self.v2;
        self.v0 = self.v0.wrapping_add(self.v3);
        self.v3 = self.v3.rotate_left(21);
        self.v3 ^= self.v0;
        self.v2 = self.v2.wrapping_add(self.v1);
        self.v1 = self.v1.rotate_left(17);
        self.v1 ^= self.v2;
        self.v2 = self.v2.rotate_left(32);
    }

    #[inline(always)]
    fn compress(&mut self, word: u64) {
        self.v3 ^= word;
        self.sip_round();
        self.v0 ^= word;
    }
}

impl Default for ContentHasher {
    #[inline]
    fn default() -> Self {
        let (k0, k1) = content_hash_keys();
        ContentHasher::new(k0, k1)
    }
}

impl Hasher for ContentHasher {
    #[inline]
    fn finish(&self) -> u64 {
        let mut state = *self;
        let last = ((self.length as u64 & 0xff) << 56) | self.tail;
        state.compress(last);
        state.v2 ^= 0xff;
        state.sip_round();
        state.sip_round();
        state.sip_round();
        state.v0 ^ state.v1 ^ state.v2 ^ state.v3
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.length = self.length.wrapping_add(bytes.len());
        let len = bytes.len();

        // The common case is a single write of a whole line or word into a
        // fresh hasher.  Keep that path free of per-byte loops: full words
        // are consumed directly and the remainder is taken from an
        // overlapping read of the last word.
        if self.ntail == 0 {
            let mut pos = 0;
            while pos + 8 <= len {
                self.compress(read_u64(&bytes[pos..]));
                pos += 8;
            }
            let remainder = len - pos;
            if remainder != 0 {
                self.tail = if len >= 8 {
                    read_u64(&bytes[len - 8..]) >> (8 * (8 - remainder))
                } else {
                    let mut tail = 0u64;
                    for (index, &byte) in bytes.iter().enumerate() {
                        tail |= (byte as u64) << (8 * index);
                    }
                    tail
                };
                self.ntail = remainder;
            }
            return;
        }

        // Complete a partially filled word first.
        let mut rest = bytes;
        let take = (8 - self.ntail).min(rest.len());
        for &byte in &rest[..take] {
            self.tail |= (byte as u64) << (8 * self.ntail);
            self.ntail += 1;
        }
        rest = &rest[take..];
        if self.ntail < 8 {
            return;
        }
        self.compress(self.tail);
        self.tail = 0;
        self.ntail = 0;

        let mut chunks = rest.chunks_exact(8);
        for chunk in &mut chunks {
            self.compress(u64::from_le_bytes(chunk.try_into().unwrap()));
        }

        let remainder = chunks.remainder();
        let mut tail = 0u64;
        for (index, &byte) in remainder.iter().enumerate() {
            tail |= (byte as u64) << (8 * index);
        }
        self.tail = tail;
        self.ntail = remainder.len();
    }

    #[inline]
    fn write_u8(&mut self, i: u8) {
        // `str` hashing appends a single terminator byte after the content.
        self.length = self.length.wrapping_add(1);
        self.tail |= (i as u64) << (8 * self.ntail);
        self.ntail += 1;
        if self.ntail == 8 {
            self.compress(self.tail);
            self.tail = 0;
            self.ntail = 0;
        }
    }

    #[inline]
    fn write_u16(&mut self, i: u16) {
        self.write(&i.to_le_bytes());
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        if self.ntail == 0 {
            self.length = self.length.wrapping_add(4);
            self.tail = i as u64;
            self.ntail = 4;
        } else {
            self.write(&i.to_le_bytes());
        }
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        if self.ntail == 0 {
            self.length = self.length.wrapping_add(8);
            self.compress(i);
        } else {
            self.write(&i.to_le_bytes());
        }
    }

    #[inline]
    fn write_u128(&mut self, i: u128) {
        self.write(&i.to_le_bytes());
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.write(&i.to_le_bytes());
    }
}

/// Hashes a value into a `u64` that is stable for the duration of a diff.
///
/// The result is consistent within a process (and with `std`, randomized
/// per process); it is only used to bucket equal items within a single diff
/// operation and never influences the produced diff.
#[inline(always)]
pub(crate) fn stable_hash<T: Hash + ?Sized>(value: &T) -> u64 {
    let mut hasher = ContentHasher::default();
    value.hash(&mut hasher);
    hasher.finish()
}

/// Represents an item in the vector returned by [`unique`].
///
/// It compares like the underlying item does it was created from but
/// carries the index it was originally created from.
pub struct UniqueItem<'a, Idx: ?Sized> {
    lookup: &'a Idx,
    index: usize,
}

impl<Idx: ?Sized> UniqueItem<'_, Idx>
where
    Idx: Index<usize>,
{
    /// Returns the value.
    #[inline(always)]
    pub fn value(&self) -> &Idx::Output {
        &self.lookup[self.index]
    }

    /// Returns the original index.
    #[inline(always)]
    pub fn original_index(&self) -> usize {
        self.index
    }
}

impl<'a, Idx: Index<usize> + 'a> Debug for UniqueItem<'a, Idx>
where
    Idx::Output: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("UniqueItem")
            .field("value", &self.value())
            .field("original_index", &self.original_index())
            .finish()
    }
}

impl<'a, 'b, A, B> PartialEq<UniqueItem<'a, A>> for UniqueItem<'b, B>
where
    A: Index<usize> + 'b + ?Sized,
    B: Index<usize> + 'b + ?Sized,
    B::Output: PartialEq<A::Output>,
{
    #[inline(always)]
    fn eq(&self, other: &UniqueItem<'a, A>) -> bool {
        self.value() == other.value()
    }
}

impl<'a, Idx> Eq for UniqueItem<'a, Idx>
where
    Idx: Index<usize> + ?Sized,
    Idx::Output: Eq,
{
}

impl<Idx> Hash for UniqueItem<'_, Idx>
where
    Idx: Index<usize> + ?Sized,
    Idx::Output: Hash,
{
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value().hash(state);
    }
}

/// Returns only unique items in the sequence as vector.
///
/// Each item is wrapped in a [`UniqueItem`] so that both the value and the
/// index can be extracted.
pub fn unique<Idx>(lookup: &Idx, range: Range<usize>) -> Vec<UniqueItem<'_, Idx>>
where
    Idx: Index<usize> + ?Sized,
    Idx::Output: Hash + Eq,
{
    // We key buckets by hash to stay compatible with both HashMap and
    // BTreeMap backends without requiring `Idx::Output: Ord`.
    let mut by_hash =
        int_key_map_with_capacity::<u64, HashBucket<(usize, Option<usize>)>>(range.len());
    for index in range {
        let hash = stable_hash(&lookup[index]);
        let Some(bucket) = by_hash.get_mut(&hash) else {
            by_hash.insert(hash, HashBucket::new((index, Some(index))));
            continue;
        };

        let mut found = false;
        for (representative, unique_index) in bucket.iter_mut() {
            if lookup[index] == lookup[*representative] {
                if unique_index.is_some() {
                    *unique_index = None;
                }
                found = true;
                break;
            }
        }

        if !found {
            bucket.push((index, Some(index)));
        }
    }

    let mut rv = by_hash
        .into_values()
        .flat_map(|bucket| bucket.into_iter())
        .filter_map(|(_, unique_index)| unique_index)
        .map(|index| UniqueItem { lookup, index })
        .collect::<Vec<_>>();
    rv.sort_by_key(|a| a.original_index());
    rv
}

/// Given two lookups and ranges calculates the length of the common prefix.
pub fn common_prefix_len<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if is_empty_range(&old_range) || is_empty_range(&new_range) {
        return 0;
    }
    new_range
        .zip(old_range)
        .take_while(
            #[inline(always)]
            |x| new[x.0] == old[x.1],
        )
        .count()
}

/// Given two lookups and ranges calculates the length of common suffix.
pub fn common_suffix_len<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if is_empty_range(&old_range) || is_empty_range(&new_range) {
        return 0;
    }
    new_range
        .rev()
        .zip(old_range.rev())
        .take_while(
            #[inline(always)]
            |x| new[x.0] == old[x.1],
        )
        .count()
}

struct OffsetLookup<Int> {
    offset: usize,
    vec: Vec<Int>,
}

impl<Int> Index<usize> for OffsetLookup<Int> {
    type Output = Int;

    #[inline(always)]
    fn index(&self, index: usize) -> &Self::Output {
        &self.vec[index - self.offset]
    }
}

/// A utility struct to convert distinct items to unique integers.
///
/// This can be helpful on larger inputs to speed up the comparisons
/// performed by doing a first pass where the data set gets reduced
/// to (small) integers. For lazily computed or projected values, see
/// [`CachedLookup`](super::CachedLookup).
///
/// The idea is that instead of passing two sequences to a diffling algorithm
/// you first pass it via [`IdentifyDistinct`]:
///
/// ```rust
/// use similar::capture_diff;
/// use similar::algorithms::{Algorithm, IdentifyDistinct};
///
/// let old = &["foo", "bar", "baz"][..];
/// let new = &["foo", "blah", "baz"][..];
/// let h = IdentifyDistinct::<u32>::new(old, 0..old.len(), new, 0..new.len());
/// let ops = capture_diff(
///     Algorithm::Myers,
///     h.old_lookup(),
///     h.old_range(),
///     h.new_lookup(),
///     h.new_range(),
/// );
/// ```
///
/// The indexes are the same as with the passed source ranges.
pub struct IdentifyDistinct<Int> {
    old: OffsetLookup<Int>,
    new: OffsetLookup<Int>,
    old_distinct: usize,
}

impl<Int> IdentifyDistinct<Int>
where
    Int: Add<Output = Int> + From<u8> + Default + Copy,
{
    /// Creates an int hasher for two sequences.
    pub fn new<Old, New>(
        old: &Old,
        old_range: Range<usize>,
        new: &New,
        new_range: Range<usize>,
    ) -> Self
    where
        Old: Index<usize> + ?Sized,
        Old::Output: Eq + Hash,
        New: Index<usize> + ?Sized,
        New::Output: Eq + Hash + PartialEq<Old::Output>,
    {
        Self::build(old, old_range, new, new_range, false)
    }

    /// Like [`new`](Self::new), but every value that only occurs in `new`
    /// shares the single identifier [`old_distinct_count`](Self::old_distinct_count).
    ///
    /// Algorithms that only ever compare old items against new items (Myers,
    /// Hunt, Histogram) never need to tell two new-only values apart, and
    /// skipping them keeps the map proportional to the old side.  This is
    /// not suitable for Patience, which needs uniqueness within `new`.
    pub(crate) fn new_matching_old<Old, New>(
        old: &Old,
        old_range: Range<usize>,
        new: &New,
        new_range: Range<usize>,
    ) -> Self
    where
        Old: Index<usize> + ?Sized,
        Old::Output: Eq + Hash,
        New: Index<usize> + ?Sized,
        New::Output: Eq + Hash + PartialEq<Old::Output>,
    {
        Self::build(old, old_range, new, new_range, true)
    }

    fn build<Old, New>(
        old: &Old,
        old_range: Range<usize>,
        new: &New,
        new_range: Range<usize>,
        collapse_new_only: bool,
    ) -> Self
    where
        Old: Index<usize> + ?Sized,
        Old::Output: Eq + Hash,
        New: Index<usize> + ?Sized,
        New::Output: Eq + Hash + PartialEq<Old::Output>,
    {
        #[derive(Clone, Copy)]
        enum Representative {
            Old(usize),
            New(usize),
        }

        let mut map =
            int_key_map_with_capacity::<u64, HashBucket<(Representative, Int)>>(old_range.len());
        let mut old_seq = Vec::with_capacity(old_range.len());
        let mut new_seq = Vec::with_capacity(new_range.len());
        let mut next_id = Int::default();
        let mut old_distinct = 0usize;
        let step = Int::from(1);
        let old_start = old_range.start;
        let new_start = new_range.start;

        for idx in old_range {
            let hash = stable_hash(&old[idx]);
            let id = if let Some(bucket) = map.get_mut(&hash) {
                if let Some((_, id)) = bucket.iter().find(
                    |(rep, _)| matches!(rep, Representative::Old(rep_idx) if old[idx] == old[*rep_idx]),
                ) {
                    *id
                } else {
                    let id = next_id;
                    next_id = next_id + step;
                    old_distinct += 1;
                    bucket.push((Representative::Old(idx), id));
                    id
                }
            } else {
                let id = next_id;
                next_id = next_id + step;
                old_distinct += 1;
                map.insert(hash, HashBucket::new((Representative::Old(idx), id)));
                id
            };
            old_seq.push(id);
        }

        let new_only_id = next_id;
        if collapse_new_only {
            next_id = next_id + step;
        }

        for idx in new_range {
            let hash = stable_hash(&new[idx]);
            let found = map.get_mut(&hash).map(|bucket| {
                let existing = bucket
                    .iter()
                    .find(|(rep, _)| match rep {
                        Representative::Old(rep_idx) => new[idx] == old[*rep_idx],
                        Representative::New(rep_idx) => new[idx] == new[*rep_idx],
                    })
                    .map(|(_, id)| *id);
                (bucket, existing)
            });
            let id = match found {
                Some((_, Some(id))) => id,
                _ if collapse_new_only => new_only_id,
                Some((bucket, None)) => {
                    let id = next_id;
                    next_id = next_id + step;
                    bucket.push((Representative::New(idx), id));
                    id
                }
                None => {
                    let id = next_id;
                    next_id = next_id + step;
                    map.insert(hash, HashBucket::new((Representative::New(idx), id)));
                    id
                }
            };
            new_seq.push(id);
        }

        IdentifyDistinct {
            old: OffsetLookup {
                offset: old_start,
                vec: old_seq,
            },
            new: OffsetLookup {
                offset: new_start,
                vec: new_seq,
            },
            old_distinct,
        }
    }

    /// Returns the number of distinct values that occur in the old lookup.
    ///
    /// Old values are numbered first, so exactly the identifiers below this
    /// count occur in the old lookup; values that only occur in the new
    /// lookup have larger identifiers.
    pub(crate) fn old_distinct_count(&self) -> usize {
        self.old_distinct
    }

    /// Returns a lookup for the old side.
    pub fn old_lookup(&self) -> &impl Index<usize, Output = Int> {
        &self.old
    }

    /// Returns a lookup for the new side.
    pub fn new_lookup(&self) -> &impl Index<usize, Output = Int> {
        &self.new
    }

    /// Convenience method to get back the old range.
    pub fn old_range(&self) -> Range<usize> {
        self.old.offset..self.old.offset + self.old.vec.len()
    }

    /// Convenience method to get back the new range.
    pub fn new_range(&self) -> Range<usize> {
        self.new.offset..self.new.offset + self.new.vec.len()
    }
}

#[test]
fn test_content_hasher_matches_reference_and_streams_consistently() {
    use alloc::vec::Vec;

    let mut data = Vec::new();
    let mut seed = 0x9E37_79B9u32;
    for _ in 0..80 {
        seed ^= seed << 13;
        seed ^= seed >> 17;
        seed ^= seed << 5;
        data.push(seed as u8);
    }

    for len in 0..=64 {
        let bytes = &data[..len];
        let mut whole = ContentHasher::new(0, 0);
        whole.write(bytes);
        let expected = whole.finish();

        // `DefaultHasher::new()` is SipHash-1-3 with a zero key.
        #[cfg(feature = "std")]
        {
            let mut reference = std::hash::DefaultHasher::new();
            reference.write(bytes);
            assert_eq!(expected, reference.finish(), "len {len}");
        }

        // Splitting the input across writes, including through the
        // integer shortcuts, must not change the result.
        for split in 0..=len {
            let mut pieces = ContentHasher::new(0, 0);
            pieces.write(&bytes[..split]);
            pieces.write(&bytes[split..]);
            assert_eq!(pieces.finish(), expected, "len {len} split {split}");

            let mut bytewise = ContentHasher::new(0, 0);
            bytewise.write(&bytes[..split]);
            for &byte in &bytes[split..] {
                bytewise.write_u8(byte);
            }
            assert_eq!(bytewise.finish(), expected, "len {len} bytewise {split}");
        }
        if len >= 4 {
            let mut prefixed = ContentHasher::new(0, 0);
            prefixed.write_u32(u32::from_le_bytes(bytes[..4].try_into().unwrap()));
            prefixed.write(&bytes[4..]);
            assert_eq!(prefixed.finish(), expected, "len {len} u32 prefix");
        }
        if len >= 8 {
            let mut prefixed = ContentHasher::new(0, 0);
            prefixed.write_u64(read_u64(bytes));
            prefixed.write(&bytes[8..]);
            assert_eq!(prefixed.finish(), expected, "len {len} u64 prefix");
        }
    }
}

#[test]
fn test_stable_hash_quality() {
    use alloc::format;
    use alloc::string::String;

    // Typical diff inputs: dense integer ids and many short, similar strings.
    // A weak hash would collide or cluster on these, which would make the
    // hash buckets degenerate into linear scans.
    let mut seen = crate::types::IntKeyMap::<u64, ()>::default();
    for value in 0..100_000u32 {
        assert!(seen.insert(stable_hash(&value), ()).is_none());
    }

    let mut seen = crate::types::IntKeyMap::<u64, ()>::default();
    let mut ones_per_bit = [0usize; 64];
    let mut samples = 0usize;
    for index in 0..50_000u32 {
        for text in [
            format!("line {index}\n"),
            format!("    return {index};\n"),
            format!("{index}"),
            String::from_utf8(alloc::vec![b'x'; (index % 40) as usize + 1]).unwrap() + "\n",
        ] {
            let hash = stable_hash(text.as_str());
            // Repeated padding lines are equal across iterations; only
            // distinct strings must produce distinct hashes.
            if text.starts_with('x') && index >= 40 {
                continue;
            }
            assert!(seen.insert(hash, ()).is_none(), "collision for {text:?}");
            for (bit, count) in ones_per_bit.iter_mut().enumerate() {
                *count += ((hash >> bit) & 1) as usize;
            }
            samples += 1;
        }
    }
    // Every output bit should be set roughly half of the time; hashbrown
    // relies on the top bits as tags and the low bits as bucket index.
    for (bit, count) in ones_per_bit.iter().enumerate() {
        let ratio = *count as f64 / samples as f64;
        assert!(
            (0.45..0.55).contains(&ratio),
            "bit {bit} is set in {ratio} of hashes"
        );
    }
}

#[test]
fn test_unique() {
    let u = unique(&vec!['a', 'b', 'c', 'd', 'd', 'b'], 0..6)
        .into_iter()
        .map(|x| (*x.value(), x.original_index()))
        .collect::<Vec<_>>();
    assert_eq!(u, vec![('a', 0), ('c', 2)]);
}

#[test]
fn test_int_hasher() {
    let ih = IdentifyDistinct::<u8>::new(
        &["", "foo", "bar", "baz"][..],
        1..4,
        &["", "foo", "blah", "baz"][..],
        1..4,
    );
    assert_eq!(ih.old_lookup()[1], 0);
    assert_eq!(ih.old_lookup()[2], 1);
    assert_eq!(ih.old_lookup()[3], 2);
    assert_eq!(ih.new_lookup()[1], 0);
    assert_eq!(ih.new_lookup()[2], 3);
    assert_eq!(ih.new_lookup()[3], 2);
    assert_eq!(ih.old_range(), 1..4);
    assert_eq!(ih.new_range(), 1..4);
}

#[test]
fn test_identify_distinct_matching_old() {
    let old = &["a", "b", "a", "c"][..];
    let new = &["x", "b", "y", "c", "x", "a"][..];
    let ih = IdentifyDistinct::<u32>::new_matching_old(old, 0..old.len(), new, 0..new.len());

    // Old values are numbered densely in order of appearance.
    assert_eq!(ih.old_distinct_count(), 3);
    assert_eq!(
        (0..4).map(|i| ih.old_lookup()[i]).collect::<Vec<_>>(),
        [0, 1, 0, 2]
    );
    // New values map to their old identifier when they have one and all
    // share the sentinel `old_distinct_count()` otherwise.
    assert_eq!(
        (0..6).map(|i| ih.new_lookup()[i]).collect::<Vec<_>>(),
        [3, 1, 3, 2, 3, 0]
    );

    // The regular constructor keeps new-only values apart.
    let ih = IdentifyDistinct::<u32>::new(old, 0..old.len(), new, 0..new.len());
    assert_eq!(
        (0..6).map(|i| ih.new_lookup()[i]).collect::<Vec<_>>(),
        [3, 1, 4, 2, 3, 0]
    );
}

#[test]
fn test_common_prefix_len() {
    assert_eq!(
        common_prefix_len("".as_bytes(), 0..0, "".as_bytes(), 0..0),
        0
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 0..9, "foobarblah".as_bytes(), 0..10),
        7
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 0..9, "blablabla".as_bytes(), 0..9),
        0
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 3..9, "foobarblah".as_bytes(), 3..10),
        4
    );
}

#[test]
fn test_common_suffix_len() {
    assert_eq!(
        common_suffix_len("".as_bytes(), 0..0, "".as_bytes(), 0..0),
        0
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 0..4, "X0001234".as_bytes(), 0..8),
        4
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 0..4, "Xxxx".as_bytes(), 0..4),
        0
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 2..4, "01234".as_bytes(), 2..5),
        2
    );
}

/// Counts equality comparisons so tests can observe bucket degradation.
#[cfg(test)]
struct CountedBlob<'a> {
    bytes: &'a [u8],
    comparisons: &'a core::cell::Cell<usize>,
}

#[cfg(test)]
impl Hash for CountedBlob<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Standard slice hashing: length prefix followed by the bytes.
        self.bytes.hash(state);
    }
}

#[cfg(test)]
impl PartialEq for CountedBlob<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.comparisons.set(self.comparisons.get() + 1);
        self.bytes == other.bytes
    }
}

#[cfg(test)]
impl Eq for CountedBlob<'_> {}

/// Asserts that `inputs` hash distinctly and that `IdentifyDistinct` stays
/// linear on them, which is the observable property an attacker targets.
#[cfg(test)]
fn assert_no_cheap_collisions(inputs: &[Vec<u8>], family: &str) {
    use core::cell::Cell;

    let comparisons = Cell::new(0usize);
    let blobs: Vec<_> = inputs
        .iter()
        .map(|bytes| CountedBlob {
            bytes,
            comparisons: &comparisons,
        })
        .collect();

    let mut seen = crate::types::IntKeyMap::<u64, ()>::default();
    for blob in &blobs {
        seen.insert(stable_hash(blob), ());
    }
    assert_eq!(seen.len(), inputs.len(), "{family}: hashes collide");

    let n = blobs.len();
    IdentifyDistinct::<u32>::new(&blobs, 0..n, &blobs, 0..n);
    assert!(
        comparisons.get() <= n * 4,
        "{family}: {} comparisons for {n} values",
        comparisons.get()
    );
}

/// Regression test for the multiplicative mixer this crate used before
/// switching to SipHash.
///
/// That mixer computed `folded_multiply((a ^ state) | HIGH, (b ^ K1) | HIGH)`
/// plus linear feed-forward terms.  Choosing `b = K1` turns the second
/// multiplicand into `1 << 63`, which makes the multiplication a shift and
/// the whole block affine over GF(2).  Two such blocks give 128 bits of
/// freedom for 64 bits of state, so colliding inputs can be produced with
/// plain linear algebra.  The test rebuilds that family (checking that it
/// does collide under the old mixer) and asserts the current hash keeps
/// the values apart.
#[test]
fn test_stable_hash_resists_affine_block_family() {
    use alloc::vec::Vec;

    const SEED: u64 = 0x243F_6A88_85A3_08D3;
    const K0: u64 = 0x9E37_79B9_7F4A_7C15;
    const K1: u64 = 0xD6E8_FEB8_6659_FD93;
    const K2: u64 = 0xA076_1D64_78BD_642F;
    const HIGH: u64 = 1 << 63;

    fn weak_mix(state: u64, a: u64, b: u64) -> u64 {
        let x = (a ^ state) | HIGH;
        let y = (b ^ K1) | HIGH;
        folded_multiply(x, y) ^ a.rotate_left(29) ^ b.rotate_left(47) ^ state.rotate_left(11)
    }

    fn weak_write_u64(state: u64, i: u64) -> u64 {
        folded_multiply((state ^ i) | HIGH, K1) ^ i.rotate_left(29) ^ state.rotate_left(11)
    }

    // State after hashing a `[u8; 32]` up to (excluding) the final empty
    // tail mix and finalization: length prefix, length mixing, two blocks.
    fn weak_two_blocks(a1: u64, a2: u64) -> u64 {
        let state = weak_write_u64(SEED, 32) ^ 32u64.wrapping_mul(K0);
        let state = weak_mix(state, a1, K1);
        weak_mix(state, a2, K1)
    }

    fn weak_hash(a1: u64, a2: u64) -> u64 {
        folded_multiply(weak_mix(weak_two_blocks(a1, a2), 0, 0), K2)
    }

    // `weak_two_blocks` is affine over GF(2) in the 128 input bits.  Extract
    // the linear part column by column and compute its kernel with a xor
    // basis; every kernel vector added to an input leaves the state as is.
    let split = |v: u128| (v as u64, (v >> 64) as u64);
    let constant = weak_two_blocks(0, 0);
    let mut basis: [Option<(u64, u128)>; 64] = [None; 64];
    let mut kernel: Vec<u128> = Vec::new();
    for column in 0..128 {
        let input = 1u128 << column;
        let (a1, a2) = split(input);
        let mut vector = weak_two_blocks(a1, a2) ^ constant;
        let mut combination = input;
        for bit in (0..64).rev() {
            if (vector >> bit) & 1 == 0 {
                continue;
            }
            match basis[bit] {
                Some((basis_vector, basis_combination)) => {
                    vector ^= basis_vector;
                    combination ^= basis_combination;
                }
                None => {
                    basis[bit] = Some((vector, combination));
                    break;
                }
            }
        }
        if vector == 0 {
            kernel.push(combination);
        }
    }
    assert!(kernel.len() >= 12, "kernel dimension {}", kernel.len());

    let n = 4_000usize;
    let inputs: Vec<Vec<u8>> = (1..=n)
        .map(|index| {
            let mut vector = 0u128;
            for (bit, kernel_vector) in kernel.iter().take(12).enumerate() {
                if (index >> bit) & 1 == 1 {
                    vector ^= kernel_vector;
                }
            }
            let (a1, a2) = split(vector);
            let mut bytes = Vec::with_capacity(32);
            bytes.extend_from_slice(&a1.to_le_bytes());
            bytes.extend_from_slice(&K1.to_le_bytes());
            bytes.extend_from_slice(&a2.to_le_bytes());
            bytes.extend_from_slice(&K1.to_le_bytes());
            bytes
        })
        .collect();

    // Sanity check the construction: the family collides under the mixer
    // it was built against.
    let reference = weak_hash(0, 0);
    for input in &inputs {
        let a1 = u64::from_le_bytes(input[..8].try_into().unwrap());
        let a2 = u64::from_le_bytes(input[16..24].try_into().unwrap());
        assert_eq!(weak_hash(a1, a2), reference);
    }

    assert_no_cheap_collisions(&inputs, "affine two-block family");
}

/// Families that trivially collided under earlier mixers because one word
/// could zero a multiplication operand.
#[test]
fn test_stable_hash_resists_zero_operand_families() {
    use alloc::vec::Vec;

    const K1: u64 = 0xD6E8_FEB8_6659_FD93;
    let n = 4_000u64;
    let block = |a: u64, b: u64| -> Vec<u8> {
        let mut out = Vec::with_capacity(16);
        out.extend_from_slice(&a.to_le_bytes());
        out.extend_from_slice(&b.to_le_bytes());
        out
    };

    let inputs: Vec<_> = (0..n).map(|i| block(i, K1)).collect();
    assert_no_cheap_collisions(&inputs, "fixed second word");
    let inputs: Vec<_> = (0..n).map(|i| block(K1, i)).collect();
    assert_no_cheap_collisions(&inputs, "fixed first word");
    let inputs: Vec<_> = (0..n)
        .map(|i| {
            let mut out = block(i, K1);
            out.extend(block(i.rotate_left(32), K1));
            out
        })
        .collect();
    assert_no_cheap_collisions(&inputs, "fixed words in both blocks");
    let inputs: Vec<_> = (0..n).map(|i| i.to_le_bytes().to_vec()).collect();
    assert_no_cheap_collisions(&inputs, "single word");
    let inputs: Vec<_> = (0..n).map(|i| (i as u32).to_le_bytes().to_vec()).collect();
    assert_no_cheap_collisions(&inputs, "half word");
}
