#![no_std]
#![warn(clippy::indexing_slicing)]

//! This library aims to provide useful constructs to vastly simplify data formatting tasks.
//!
//! Being a no_std library, no allocations will be made, ever. Even with this restriction however,
//! the provided functions are flexible and ergonomic.
//!
//! This code snippet:
//! ```rust
//! # let list = &[1, 2, 3];
//! for (i, item) in list.iter().enumerate() {
//!     if i == list.len() - 1 {
//!         println!("{}", item);
//!     } else {
//!         print!("{} - ", item);
//!     }
//! }
//! ```
//! ...simplifies to:
//! ```rust
//! # let list = &[1, 2, 3];
//! println!("{}", display_utils::join(list, " - "));
//! ```
//!
//! Other functions work in a similar fashion. Browser through the crate functions for an overview
//! of what you can do.
//!
//! Extension traits ([`DisplayExt`], [`IteratorExt`]) which may be used to make method chains
//! more readable.

mod extension_traits;
pub use extension_traits::{DisplayExt, IteratorExt};
// mod wrapper;
// pub use wrapper::Wrapper;

// Sigh, how I wish std exposed non-panicking functions by default
fn checked_split_at(s: &[u8], index: usize) -> Option<(&[u8], &[u8])> {
	Some((s.get(..index)?, s.get(index..)?))
}

// and while we're at it, doctests for private functions would be nice too
#[test]
#[rustfmt::skip]
fn test_checked_split_at() {
	assert_eq!(checked_split_at(b"", 0), Some((&b""[..], &b""[..])));
	assert_eq!(checked_split_at(b"Hello", 0), Some((&b""[..], &b"Hello"[..])));
	assert_eq!(checked_split_at(b"Hello", 3), Some((&b"Hel"[..], &b"lo"[..])));
	assert_eq!(checked_split_at(b"Hello", 5), Some((&b"Hello"[..], &b""[..])));
	assert_eq!(checked_split_at(b"Hello", 6), None);
}

/// Print a loading-style bar using Unicode block characters.
///
/// The bar is very high-resolution: 8 states can be represented per character.
///
/// Accepts the total length of the bar and a float from 0.0 to 1.0 as the filled proportion.
///
/// Prints exactly max_length chars (not bytes!), right-padded with spaces.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(unicode_block_bar(13, 0.0).to_string(), "             ");
/// assert_eq!(unicode_block_bar(13, 0.1).to_string(), "█▎           ");
/// assert_eq!(unicode_block_bar(13, 0.2).to_string(), "██▌          ");
/// assert_eq!(unicode_block_bar(13, 0.3).to_string(), "███▉         ");
/// assert_eq!(unicode_block_bar(13, 0.4).to_string(), "█████▏       ");
/// assert_eq!(unicode_block_bar(13, 0.5).to_string(), "██████▌      ");
/// assert_eq!(unicode_block_bar(13, 0.6).to_string(), "███████▊     ");
/// assert_eq!(unicode_block_bar(13, 0.7).to_string(), "█████████    ");
/// assert_eq!(unicode_block_bar(13, 0.8).to_string(), "██████████▍  ");
/// assert_eq!(unicode_block_bar(13, 0.9).to_string(), "███████████▋ ");
/// assert_eq!(unicode_block_bar(13, 1.0).to_string(), "█████████████");
/// # assert_eq!(unicode_block_bar(4, 0.0).to_string(), "    ");
/// # assert_eq!(unicode_block_bar(4, 0.125).to_string(), "▌   ");
/// # assert_eq!(unicode_block_bar(4, 0.25).to_string(), "█   ");
/// # assert_eq!(unicode_block_bar(4, 1.0).to_string(), "████");
/// # assert_eq!(unicode_block_bar(4, 1.5).to_string(), "████");
/// # assert_eq!(unicode_block_bar(4, -1.0).to_string(), "    ");
/// # assert_eq!(unicode_block_bar(1, 1.0).to_string(), "█");
/// # assert_eq!(unicode_block_bar(0, 0.0).to_string(), "");
/// # assert_eq!(unicode_block_bar(0, 1.0).to_string(), "");
/// ```
pub fn unicode_block_bar(max_length: usize, proportion: f32) -> UnicodeBlockBar {
	// index x = x 8ths of a full block
	const BLOCK_CHARS: [&str; 9] = [" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█"];

	impl core::fmt::Display for UnicodeBlockBar {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			for _ in 0..self.num_full_blocks {
				f.write_str(&BLOCK_CHARS[8])?;
			}
			f.write_str(self.midpoint)?;
			for _ in 0..self.num_spaces {
				f.write_str(&BLOCK_CHARS[0])?;
			}
			Ok(())
		}
	}

	let max_steps = max_length * 8; // number of steps for the bar to be full

	let steps = proportion * max_steps as f32;
	let steps = (steps.max(0.0) as usize).min(max_steps);

	if steps == max_steps {
		UnicodeBlockBar {
			num_full_blocks: max_length,
			midpoint: "",
			num_spaces: 0,
		}
	} else {
		#[allow(clippy::indexing_slicing)] // index will be in 0..8 always due to modulo
		UnicodeBlockBar {
			num_full_blocks: steps / 8,
			midpoint: &BLOCK_CHARS[steps % 8],
			num_spaces: max_length - (steps / 8 + 1),
		}
	}
}

/// See [`unicode_block_bar()`].
pub struct UnicodeBlockBar {
	num_full_blocks: usize,
	/// may be empty!
	midpoint: &'static str,
	num_spaces: usize,
}

/// Print a sequence of equalizer-style vertical bars using Unicode block characters.
///
/// The bars are very high-resolution: 8 states can be represented per character.
///
/// Accepts the total maximum height of the bars and an iterator over each bar's fill percentage.
///
/// ```rust
/// let expected_output = "\
/// █          █
/// █         ▆█
/// █        ▄██
/// █       ▁███
/// █       ████
/// █      ▇████
/// █     ▄█████
/// █    ▂██████
/// █    ███████
/// █   ████████
/// █  ▅████████
/// █ ▃█████████
/// █ ██████████";
///
/// assert_eq!(
///     display_utils::vertical_unicode_block_bars(13,
///         [1.0, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter().copied()
///     ).to_string(),
///     expected_output,
/// );
/// ```
pub fn vertical_unicode_block_bars<I>(
	max_height: usize,
	proportions: I,
) -> VerticalUnicodeBlockBars<I::IntoIter>
where
	I: IntoIterator<Item = f32>,
	I::IntoIter: Clone,
{
	// index x = x 8ths of a full block
	const BLOCK_CHARS: [&str; 9] = [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];

	impl<I: Iterator<Item = f32> + Clone> core::fmt::Display for VerticalUnicodeBlockBars<I> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let max_steps = self.max_height * 8;
			for row in 0..self.max_height {
				if row > 0 {
					f.write_str("\n")?;
				}

				for proportion in self.proportions.clone() {
					// steps are measured in terms of whitespace
					let steps = (1.0 - proportion) * max_steps as f32;
					let steps = (steps.max(0.0) as usize).min(max_steps);

					f.write_str(match row.cmp(&(steps / 8)) {
						core::cmp::Ordering::Less => &BLOCK_CHARS[0],
						#[allow(clippy::indexing_slicing)] // that index will always be in 0..=8
						core::cmp::Ordering::Equal => &BLOCK_CHARS[8 - steps % 8],
						core::cmp::Ordering::Greater => &BLOCK_CHARS[8],
					})?;
				}
			}
			Ok(())
		}
	}

	VerticalUnicodeBlockBars {
		max_height,
		proportions: proportions.into_iter(),
	}
}

/// See [`vertical_unicode_block_bars()`].
pub struct VerticalUnicodeBlockBars<I> {
	max_height: usize,
	proportions: I,
}

/// Concatenate iterator elements, separating each element pair with a given joiner.
///
/// Equivalent to [`slice::join`](https://doc.rust-lang.org/std/primitive.slice.html#method.join).
///
/// The iterator must be cloneable, because Display objects may be printed multiple times.
///
/// ```rust
/// # use display_utils::*;
/// let strings = &["hello", "wonderful", "world"];
///
/// let output = join(strings, ", ");
/// assert_eq!(output.to_string(), "hello, wonderful, world");
/// # assert_eq!(join(&[] as &[u8], ", ").to_string(), "");
/// # assert_eq!(join(&["hello"], ", ").to_string(), "hello");
/// ```
pub fn join<T, I, J>(iterator: I, joiner: J) -> Join<I::IntoIter, J>
where
	T: core::fmt::Display,
	I: IntoIterator<Item = T>,
	I::IntoIter: Clone,
	J: core::fmt::Display,
{
	impl<T, I, J> core::fmt::Display for Join<I, J>
	where
		T: core::fmt::Display,
		I: Iterator<Item = T> + Clone,
		J: core::fmt::Display,
	{
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut iter = self.iterator.clone();

			if let Some(first_item) = iter.next() {
				first_item.fmt(f)?;
			}
			for remaining_item in iter {
				self.joiner.fmt(f)?;
				remaining_item.fmt(f)?;
			}

			Ok(())
		}
	}

	Join {
		iterator: iterator.into_iter(),
		joiner,
	}
}

/// See [`join()`].
pub struct Join<I, J> {
	iterator: I,
	joiner: J,
}

/// Concatenate iterator elements, separating each element pair with a given joiner, where each
/// iterator element can be formatted using a callback.
///
/// The callback must be Fn and the iterator must be cloneable, because Display objects may be
/// printed multiple times.
///
/// ```rust
/// # use display_utils::*;
/// let strings = &["hello", "wonderful", "world"];
///
/// let output = join_format(
///     strings.iter().enumerate(),
///     ", ",
///     |(i, string), f| write!(f, "{}={}", i, string),
/// );
/// assert_eq!(output.to_string(), "0=hello, 1=wonderful, 2=world");
/// ```
pub fn join_format<I, J, C>(iterator: I, joiner: J, callback: C) -> JoinFormat<I::IntoIter, J, C>
where
	I: IntoIterator,
	I::IntoIter: Clone,
	J: core::fmt::Display,
	C: Fn(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
{
	impl<I, J, C> core::fmt::Display for JoinFormat<I, J, C>
	where
		I: Iterator + Clone,
		J: core::fmt::Display,
		C: Fn(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
	{
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut iter = self.iterator.clone();

			if let Some(first_item) = iter.next() {
				(self.callback)(first_item, f)?;
			}
			for remaining_item in iter {
				self.joiner.fmt(f)?;
				(self.callback)(remaining_item, f)?;
			}

			Ok(())
		}
	}

	JoinFormat {
		iterator: iterator.into_iter(),
		callback,
		joiner,
	}
}

/// See [`join_format()`].
pub struct JoinFormat<I, J, C> {
	iterator: I,
	joiner: J,
	callback: C,
}

/// Repeat an object a certain number of times.
///
/// Equivalent to `str::repeat`.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(repeat("fun", 5).to_string(), "funfunfunfunfun");
/// assert_eq!(repeat(7, 7).to_string(), "7777777");
/// # assert_eq!(repeat("a", 0).to_string(), "");
/// # assert_eq!(repeat("", 5).to_string(), "");
/// ```
pub fn repeat<T: core::fmt::Display>(token: T, times: usize) -> Repeat<T> {
	impl<T: core::fmt::Display> core::fmt::Display for Repeat<T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			for _ in 0..self.times {
				write!(f, "{}", self.token)?;
			}
			Ok(())
		}
	}

	Repeat { token, times }
}

/// See [`repeat()`].
pub struct Repeat<T> {
	token: T,
	times: usize,
}

/// Print a Unicode-compliant lowercase version of the Display object.
///
/// Equivalent to `str::to_lowercase`, except it works on any Display object.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(lowercase("GRÜẞE JÜRGEN").to_string(), "grüße jürgen");
///
/// // Works with literally any Display object
/// assert_eq!(lowercase(join(&["HeLlO", "wOrLd"], ", ")).to_string(), "hello, world");
/// ```
pub fn lowercase<T: core::fmt::Display>(object: T) -> Lowercase<T> {
	struct LowercaseWriter<'a, 'b> {
		f: &'a mut core::fmt::Formatter<'b>,
	}

	impl core::fmt::Write for LowercaseWriter<'_, '_> {
		fn write_str(&mut self, s: &str) -> core::fmt::Result {
			for input_char in s.chars() {
				write!(self.f, "{}", input_char.to_lowercase())?;
			}
			Ok(())
		}
	}

	impl<T: core::fmt::Display> core::fmt::Display for Lowercase<T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			use core::fmt::Write as _;
			write!(LowercaseWriter { f }, "{}", self.object)
		}
	}

	Lowercase { object }
}

/// See [`lowercase()`].
pub struct Lowercase<T: core::fmt::Display> {
	object: T,
}

/// Print a Unicode-compliant uppercase version of the string.
///
/// Equivalent to `str::to_uppercase`.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(uppercase("grüße jürgen").to_string(), "GRÜSSE JÜRGEN");
///
/// // Works with literally any Display object
/// let parse_int_error = "a".parse::<i32>().unwrap_err();
/// assert_eq!(uppercase(parse_int_error).to_string(), "INVALID DIGIT FOUND IN STRING");
/// ```
pub fn uppercase<T: core::fmt::Display>(object: T) -> Uppercase<T> {
	struct UppercaseWriter<'a, 'b> {
		f: &'a mut core::fmt::Formatter<'b>,
	}

	impl core::fmt::Write for UppercaseWriter<'_, '_> {
		fn write_str(&mut self, s: &str) -> core::fmt::Result {
			for input_char in s.chars() {
				write!(self.f, "{}", input_char.to_uppercase())?;
			}
			Ok(())
		}
	}

	impl<T: core::fmt::Display> core::fmt::Display for Uppercase<T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			use core::fmt::Write as _;
			write!(UppercaseWriter { f }, "{}", self.object)
		}
	}

	Uppercase { object }
}

/// See [`uppercase()`].
pub struct Uppercase<T> {
	object: T,
}

/// Replace instances of the `from` string with the `to` Display object.
///
/// Note: this function, contrary to its std equivalent
/// [`str::replace`](https://doc.rust-lang.org/std/primitive.str.html#method.replace),
/// does not support the Pattern API because that API is not yet stabilized.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(replace("this is old", "old", "new").to_string(), "this is new");
/// # assert_eq!(replace("", "aaaaa", "xinbuldfgh").to_string(), "");
/// # assert_eq!(replace("old is this", "old", "new").to_string(), "new is this");
/// # assert_eq!(replace("old is this", "old", 5).to_string(), "5 is this");
/// ```
// TODO: change `from` parameter type to Pattern, once that API is stabilized
pub fn replace<'a, T: core::fmt::Display>(source: &'a str, from: &'a str, to: T) -> Replace<'a, T> {
	impl<T: core::fmt::Display> core::fmt::Display for Replace<'_, T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut last_end = 0;
			for (start, part) in self.source.match_indices(self.from) {
				#[allow(clippy::indexing_slicing)] // match_indices returns well-aligned indices
				f.write_str(&self.source[last_end..start])?;

				write!(f, "{}", self.to)?;
				last_end = start + part.len();
			}
			#[allow(clippy::indexing_slicing)] // last_end is well-aligned still
			f.write_str(&self.source[last_end..])?;
			Ok(())
		}
	}

	Replace { source, from, to }
}

/// See [`replace()`].
pub struct Replace<'a, T> {
	source: &'a str,
	from: &'a str,
	to: T,
}

/// Replace the first n instances of the `from` string with the `to` string.
///
/// Note: this function, contrary to its std equivalent
/// [`str::replacen`](https://doc.rust-lang.org/std/primitive.str.html#method.replacen),
/// does not support the Pattern API because that API is not yet stabilized.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(replace_n("old old old", "old", "new", 2).to_string(), "new new old");
/// # assert_eq!(replace_n("old old old", "old", 123, 2).to_string(), "123 123 old");
/// # assert_eq!(replace_n("", "aaaaa", "xinbuldfgh", 987).to_string(), "");
/// # assert_eq!(replace_n("old is this", "old", "new", 0).to_string(), "old is this");
/// ```
// TODO: change `from` parameter type to Pattern, once that API is stabilized
pub fn replace_n<'a, T>(source: &'a str, from: &'a str, to: T, n: usize) -> ReplaceN<'a, T>
where
	T: core::fmt::Display,
{
	impl<T: core::fmt::Display> core::fmt::Display for ReplaceN<'_, T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut last_end = 0;
			for (start, part) in self.source.match_indices(self.from).take(self.n) {
				#[allow(clippy::indexing_slicing)] // match_indices returns well-aligned indices
				f.write_str(&self.source[last_end..start])?;

				write!(f, "{}", self.to)?;
				last_end = start + part.len();
			}
			#[allow(clippy::indexing_slicing)] // last_end is well-aligned still
			f.write_str(&self.source[last_end..])?;
			Ok(())
		}
	}

	ReplaceN {
		source,
		from,
		to,
		n,
	}
}

/// See [`replace_n()`].
pub struct ReplaceN<'a, T> {
	source: &'a str,
	from: &'a str,
	to: T,
	n: usize,
}

/// Concatenate the contents of an iterator.
///
/// If you want to insert a separator inbetween elements, use [`join`] or [`join_format`].
///
/// ```rust
/// # use display_utils::*;
/// let string = String::from("It's not much, but it's honest work");
/// assert_eq!(concat(&[
///     &string[11..13],
///     &string[25..27],
///     &string[34..35],
/// ]).to_string(), "chonk");
/// # assert_eq!(concat(&[1, 2, 3]).to_string(), "123");
/// # assert_eq!(concat(None::<u8>).to_string(), "");
/// ```
pub fn concat<I>(iterator: I) -> Concat<I::IntoIter>
where
	I: IntoIterator,
	I::Item: core::fmt::Display,
	I::IntoIter: Clone,
{
	impl<I> core::fmt::Display for Concat<I>
	where
		I: Iterator + Clone,
		I::Item: core::fmt::Display,
	{
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			for item in self.iterator.clone() {
				write!(f, "{}", item)?;
			}
			Ok(())
		}
	}

	Concat {
		iterator: iterator.into_iter(),
	}
}

/// See [`concat()`].
pub struct Concat<I> {
	iterator: I,
}

/// Write a Display object into a fixed-size buffer and returns the resulting &mut str.
///
/// Can be used in combination with all of the functions in this crate that returns Display.
///
/// Returns Ok if the object fully fits into the given buffer, and Err with the
/// truncated string otherwise.
///
/// ```rust
/// # use display_utils::*;
/// let mut buf = [0; 20];
///
/// assert_eq!(
///     collect_str(&mut buf, format_args!("I have {} apples", 12)).unwrap(),
///     "I have 12 apples",
/// );
///
/// assert_eq!(
///     collect_str(&mut buf, format_args!("I have {} apples", 615233821)).unwrap_err(),
///     "I have 615233821 app",
/// );
///
/// # let mut buf = [0; 5];
/// # // Filling the buffer snugly works?
/// # assert_eq!(
/// #     collect_str(&mut buf, "12345").unwrap(),
/// #     "12345",
/// # );
/// #
/// # // Multibyte characters are truncated at a proper char boundary?
/// # assert_eq!(
/// #     collect_str(&mut buf, "1234ü").unwrap_err(),
/// #     "1234",
/// # );
/// #
/// # // Long input strings don't break anything?
/// # assert_eq!(
/// #     collect_str(&mut buf, "w4o598etr7zgho8ws97e45rutqhwl93458tufcah34t89anmo94").unwrap_err(),
/// #     "w4o59",
/// # );
/// #
/// # // Short input strings don't break anything?
/// # assert_eq!(
/// #     collect_str(&mut buf, format_args!("{}{}{}{}{}{}", 1, 2, 3, 4, 5, 6)).unwrap_err(),
/// #     "12345",
/// # );
/// ```
pub fn collect_str_mut(
	buf: &mut [u8],
	object: impl core::fmt::Display,
) -> Result<&mut str, &mut str> {
	use core::fmt::Write;

	// minimal no_std reimplementation of std::io::Cursor
	struct Cursor<'a> {
		buf: &'a mut [u8],
		ptr: usize,
	}

	impl Write for Cursor<'_> {
		fn write_str(&mut self, s: &str) -> core::fmt::Result {
			let s = s.as_bytes();

			if self.ptr < self.buf.len() {
				let bytes_to_write = usize::min(s.len(), self.buf.len() - self.ptr);

				#[allow(clippy::indexing_slicing)] // ... any errors are caught by unit tests ^^
				self.buf[self.ptr..(self.ptr + bytes_to_write)]
					.copy_from_slice(&s[..bytes_to_write]);
			}
			self.ptr += s.len();

			Ok(())
		}
	}

	let mut cursor = Cursor { buf, ptr: 0 };

	// our Write implementation doesn't return errors anyways
	let _ = write!(cursor, "{}", object);

	if cursor.ptr > cursor.buf.len() {
		Err(match core::str::from_utf8_mut(&mut cursor.buf[..]) {
			// UNWRAP: we repeat the same function call so it's gonna succeed again
			Ok(_) => core::str::from_utf8_mut(&mut cursor.buf[..]).unwrap(),
			#[allow(clippy::indexing_slicing)] // see UNWRAP
			// UNWRAP: valid_up_to() points to a valid char boundary by definition
			Err(err) => core::str::from_utf8_mut(&mut cursor.buf[..err.valid_up_to()]).unwrap(),
		})
	} else {
		#[allow(clippy::indexing_slicing)] // if buffer didn't spill, the pointer will point to the
		// end of a copied-in &str, so it must be a valid char boundary
		Ok(core::str::from_utf8_mut(&mut cursor.buf[..cursor.ptr]).unwrap())
	}
}

/// Write a Display object into a fixed-size buffer and returns the resulting &str.
///
/// Can be used in combination with all of the functions in this crate that returns Display.
///
/// Returns Ok if the object fully fits into the given buffer, and Err with the
/// truncated string otherwise.
///
/// ```rust
/// # use display_utils::*;
/// let mut buf = [0; 20];
///
/// assert_eq!(
///     collect_str(&mut buf, format_args!("I have {} apples", 12)),
///     Ok("I have 12 apples"),
/// );
///
/// assert_eq!(
///     collect_str(&mut buf, format_args!("I have {} apples", 615233821)),
///     Err("I have 615233821 app"),
/// );
/// ```
pub fn collect_str(buf: &mut [u8], object: impl core::fmt::Display) -> Result<&str, &str> {
	// big brain
	match collect_str_mut(buf, object) {
		Ok(x) => Ok(x),
		Err(x) => Err(x),
	}
}

// /// Wrap a Display object and extend it with many common str trait implementations.
// ///
// /// Wrapped Display objects support equality checks, lexicographical ordering, and even indexing.
// /// For details, please see [`Wrapper`].
// pub fn wrap<T: core::fmt::Display>(object: T) -> Wrapper<T> {
// 	Wrapper { inner: object }
// }

/// Extract a slice from the given Display object, similar to indexing a `&str`.
///
/// This function will not panic if the slice range is out of bounds, however it will panic when the
/// slice bounds do not lie on char boundaries.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(slice("Hello", 1..).to_string(), "ello");
///
/// let parse_int_error = "a".parse::<i32>().unwrap_err();
/// assert_eq!(slice(parse_int_error, 3..=15).to_string(), "alid digit fo");
///
/// # assert_eq!(slice(concat(&["foo", "bar"]), 1..5).to_string(), "ooba");
/// ```
pub fn slice<T, R>(object: T, range: R) -> DisplaySlice<T>
where
	T: core::fmt::Display,
	R: core::ops::RangeBounds<usize>,
{
	struct ExtractingWriter<'a, 'b> {
		extract_range_start: usize,
		extract_range_end: Option<usize>,
		pointer: usize,
		sink: &'a mut core::fmt::Formatter<'b>,
	}

	impl core::fmt::Write for ExtractingWriter<'_, '_> {
		fn write_str(&mut self, segment: &str) -> core::fmt::Result {
			let segment_slice_start = self
				.extract_range_start
				.saturating_sub(self.pointer)
				.min(segment.len());

			let segment_slice_end = match self.extract_range_end {
				Some(extract_range_end) => extract_range_end
					.saturating_sub(self.pointer)
					.min(segment.len()),
				None => segment.len(),
			};

			#[allow(clippy::indexing_slicing)] // we _want_ to panic here
			self.sink
				.write_str(&segment[segment_slice_start..segment_slice_end])?;

			self.pointer += segment.len();

			Ok(())
		}
	}

	impl<T: core::fmt::Display> core::fmt::Display for DisplaySlice<T> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			use core::fmt::Write as _;
			write!(
				ExtractingWriter {
					extract_range_start: self.extract_range_start,
					extract_range_end: self.extract_range_end,
					pointer: 0,
					sink: f,
				},
				"{}",
				self.object
			)
		}
	}

	DisplaySlice {
		object,
		extract_range_start: match range.start_bound() {
			core::ops::Bound::Included(&x) => x,
			core::ops::Bound::Excluded(&x) => x + 1,
			core::ops::Bound::Unbounded => 0,
		},
		extract_range_end: match range.end_bound() {
			core::ops::Bound::Included(&x) => Some(x + 1),
			core::ops::Bound::Excluded(&x) => Some(x),
			core::ops::Bound::Unbounded => None,
		},
	}
}

/// See [`slice()`];
pub struct DisplaySlice<T> {
	object: T,
	extract_range_start: usize,
	extract_range_end: Option<usize>,
}

/// Lexicographically compares a Display object against a string.
///
/// `cmp(a, b)` is functionally equivalent to `a.to_string().cmp(b)`, but requires no allocations.
///
/// ```rust
/// # use display_utils::*;
/// use std::cmp::Ordering;
///
/// assert_eq!(cmp(true, "trud"), Ordering::Greater);
/// assert_eq!(cmp(true, "true"), Ordering::Equal);
/// assert_eq!(cmp(true, "truf"), Ordering::Less);
///
/// # let test = |segments: &[&'static str], reference| assert_eq!(
/// #     cmp(display_utils::concat(segments), reference),
/// #     segments.concat().as_str().cmp(reference),
/// # );
/// #
/// # test(&["hello"], "hello");
/// # test(&["hel", "lo"], "hello");
/// # test(&["hel", "a"], "hello");
/// # test(&["hel", "z"], "hello");
/// # test(&["hel", "lo", "lo"], "hello");
/// # test(&["", "", "hello", "", ""], "hello");
/// ```
#[allow(clippy::should_implement_trait)] // for some unholy reason we can't do this
pub fn cmp<T: core::fmt::Display>(this: T, other: &str) -> core::cmp::Ordering {
	struct CompareWriter<'a> {
		// we downcast strings to byte slices because std does it too in its PartialOrd impl
		// for str, and it's simpler
		reference: &'a [u8],
		state: core::cmp::Ordering,
	}

	impl core::fmt::Write for CompareWriter<'_> {
		fn write_str(&mut self, s: &str) -> core::fmt::Result {
			// If the ordering could already be determined in earlier segments, stop
			if self.state != core::cmp::Ordering::Equal {
				return Ok(());
			}

			if let Some((reference_segment, rest)) = checked_split_at(self.reference, s.len()) {
				self.state = s.as_bytes().cmp(reference_segment);
				self.reference = rest;
			} else {
				// We were not able to get a segment of the reference string to compare against;
				// in other words, the reference string is shorter than the Display object.
				// In Rust, longer strings are considered greater than shorter strings, hence
				// the Display object we are being fed is greater than the reference
				self.state = core::cmp::Ordering::Greater;
			};

			Ok(())
		}
	}

	let mut compare_writer = CompareWriter {
		reference: other.as_bytes(),
		state: core::cmp::Ordering::Equal,
	};

	use core::fmt::Write as _;
	// we ignore errors because our Write impl doesn't yield errors in any case anyways
	let _ = write!(compare_writer, "{}", this);

	if compare_writer.state == core::cmp::Ordering::Less && !compare_writer.reference.is_empty() {
		// The two strings were the same so far, but the reference string is not yet exhausted
		// so the reference string is greater
		core::cmp::Ordering::Less
	} else {
		compare_writer.state
	}
}

// TODO: if I feel like an evil genius one day, implement cmp function for any Display object,
// i.e. essentially `cmp(a: impl Display, b: impl Display) -> Ordering`
// In case future me forgot how to implement something that cursed: every time a sends a string
// segment, request and loop through all of b's segments, comparing the appropriate segments
// Could probably use the Index implementation there

/// Create an ordinal from a number.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(ordinal(-1).to_string(), "-1st");
/// assert_eq!(ordinal(0).to_string(), "0th");
/// assert_eq!(ordinal(1).to_string(), "1st");
/// assert_eq!(ordinal(2).to_string(), "2nd");
/// assert_eq!(ordinal(3).to_string(), "3rd");
/// assert_eq!(ordinal(4).to_string(), "4th");
/// # assert_eq!(ordinal(-2).to_string(), "-2nd");
/// # assert_eq!(ordinal(11).to_string(), "11th");
/// # assert_eq!(ordinal(102).to_string(), "102nd");
/// # assert_eq!(ordinal(112).to_string(), "112th");
/// # assert_eq!(ordinal(999).to_string(), "999th");
/// ```
pub fn ordinal(number: i32) -> Ordinal {
	impl core::fmt::Display for Ordinal {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let n = self.number.abs();
			let suffix = match n % 10 {
				1 if n % 100 != 11 => "st",
				2 if n % 100 != 12 => "nd",
				3 if n % 100 != 13 => "rd",
				_ => "th",
			};
			write!(f, "{}{}", self.number, suffix)
		}
	}

	Ordinal { number }
}

/// See [`ordinal()`].
pub struct Ordinal {
	number: i32,
}
