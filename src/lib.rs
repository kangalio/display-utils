#![no_std]
#![warn(clippy::indexing_slicing)]

//! This library provides several useful constructs to format data in a human-readable fashion with
//! zero allocations
//!
//! Some of these functions may seem to partly reinvent existing std functionality, for example
//! [`join`]:
//!
//! ```rust
//! println!("{}", display_utils::join(&[1, 2, 3], " + "));
//! println!("{}", ["1", "2", "3"].join(" + "));
//!
//! println!("{}", display_utils::repeat("abc", 4));
//! println!("{}", "abc".repeat(4));
//! ```
//!
//! The important difference is that the std approach involves 4 allocations, whereas the
//! display_utils approach operates 100% on stack and is therefore no_std compatible and likely
//! faster.

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
pub fn unicode_block_bar(max_length: usize, proportion: f32) -> impl core::fmt::Display {
	// index x = x 8ths of a full block
	const BLOCK_CHARS: [&str; 9] = [" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█"];

	struct UnicodeBlockBar {
		num_full_blocks: usize,
		/// may be empty!
		midpoint: &'static str,
		num_spaces: usize,
	}

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
pub fn vertical_unicode_block_bars<I>(max_height: usize, proportions: I) -> impl core::fmt::Display
where
	I: IntoIterator<Item = f32>,
	I::IntoIter: Clone,
{
	// index x = x 8ths of a full block
	const BLOCK_CHARS: [&str; 9] = [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];

	struct VerticalUnicodeBlockBars<I> {
		max_height: usize,
		proportions: I,
	}

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
pub fn join<T, I, J>(iterator: I, joiner: J) -> impl core::fmt::Display
where
	T: core::fmt::Display,
	I: IntoIterator<Item = T>,
	I::IntoIter: Clone,
	J: core::fmt::Display,
{
	struct Join<I, J> {
		iterator: I,
		joiner: J,
	}

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
///     |(i, string), f| write!(f, "{}={}", i, string),
///     ", ",
/// );
/// assert_eq!(output.to_string(), "0=hello, 1=wonderful, 2=world");
/// ```
pub fn join_format<I, C, J>(iterator: I, callback: C, joiner: J) -> impl core::fmt::Display
where
	I: IntoIterator,
	I::IntoIter: Clone,
	C: Fn(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
	J: core::fmt::Display,
{
	struct Join<I, C, J> {
		iterator: I,
		callback: C,
		joiner: J,
	}

	impl<I, C, J> core::fmt::Display for Join<I, C, J>
	where
		I: Iterator + Clone,
		C: Fn(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
		J: core::fmt::Display,
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

	Join {
		iterator: iterator.into_iter(),
		callback,
		joiner,
	}
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
pub fn repeat<T: core::fmt::Display>(token: T, times: usize) -> impl core::fmt::Display {
	struct Repeat<T> {
		token: T,
		times: usize,
	}

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

/// Indent to a given depth using the tab character.
///
/// This is a shortcut for `repeat("\t", depth)`; please see that function if you wish to use a
/// different indent string.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(indent_tab(2).to_string(), "\t\t");
/// # assert_eq!(indent_tab(0).to_string(), "");
/// # assert_eq!(indent_tab(1).to_string(), "\t");
/// ```
pub fn indent_tab(depth: usize) -> impl core::fmt::Display {
	repeat("\t", depth)
}

/// Indent to a given depth using 4 spaces.
///
/// This is a shortcut for `repeat("    ", depth)`; please see that function if you wish to use a
/// different indent string.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(indent_4(2).to_string(), "        ");
/// # assert_eq!(indent_4(0).to_string(), "");
/// # assert_eq!(indent_4(1).to_string(), "    ");
/// ```
pub fn indent_4(depth: usize) -> impl core::fmt::Display {
	repeat("    ", depth)
}

/// Print a Unicode-compliant lowercase version of the string.
///
/// Equivalent to `str::to_lowercase`.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(lowercase("GRÜẞE JÜRGEN").to_string(), "grüße jürgen");
/// ```
pub fn lowercase(source: &str) -> impl core::fmt::Display + '_ {
	struct AsciiLowercase<'a> {
		source: &'a str,
	}

	impl<'a> core::fmt::Display for AsciiLowercase<'a> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			use core::fmt::Write;
			for input_char in self.source.chars() {
				for output_char in input_char.to_lowercase() {
					f.write_char(output_char)?;
				}
			}
			Ok(())
		}
	}

	AsciiLowercase { source }
}

/// Print a Unicode-compliant uppercase version of the string.
///
/// Equivalent to `str::to_uppercase`.
///
/// ```rust
/// # use display_utils::*;
/// assert_eq!(uppercase("grüße jürgen").to_string(), "GRÜSSE JÜRGEN");
/// ```
pub fn uppercase(source: &str) -> impl core::fmt::Display + '_ {
	struct AsciiUppercase<'a> {
		source: &'a str,
	}

	impl<'a> core::fmt::Display for AsciiUppercase<'a> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			use core::fmt::Write;
			for input_char in self.source.chars() {
				for output_char in input_char.to_uppercase() {
					f.write_char(output_char)?;
				}
			}
			Ok(())
		}
	}

	AsciiUppercase { source }
}

/// Replace instances of the `from` string with the `to` string.
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
/// ```
// TODO: change `from` parameter type to Pattern, once that API is stabilized
pub fn replace<'a>(source: &'a str, from: &'a str, to: &'a str) -> impl core::fmt::Display + 'a {
	struct Replace<'a> {
		source: &'a str,
		from: &'a str,
		to: &'a str,
	}

	impl<'a> core::fmt::Display for Replace<'a> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut last_end = 0;
			for (start, part) in self.source.match_indices(self.from) {
				// UNWRAP: match_indices returns well-aligned indices
				f.write_str(self.source.get(last_end..start).unwrap())?;
				f.write_str(self.to)?;
				last_end = start + part.len();
			}
			// UNWRAP: last_end is well-aligned still
			f.write_str(self.source.get(last_end..).unwrap())?;
			Ok(())
		}
	}

	Replace { source, from, to }
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
/// # assert_eq!(replace_n("", "aaaaa", "xinbuldfgh", 987).to_string(), "");
/// # assert_eq!(replace_n("old is this", "old", "new", 0).to_string(), "old is this");
/// ```
// TODO: change `from` parameter type to Pattern, once that API is stabilized
pub fn replace_n<'a>(
	source: &'a str,
	from: &'a str,
	to: &'a str,
	n: usize,
) -> impl core::fmt::Display + 'a {
	struct ReplaceN<'a> {
		source: &'a str,
		from: &'a str,
		to: &'a str,
		n: usize,
	}

	impl<'a> core::fmt::Display for ReplaceN<'a> {
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut last_end = 0;
			for (start, part) in self.source.match_indices(self.from).take(self.n) {
				// UNWRAP: match_indices returns well-aligned indices
				f.write_str(self.source.get(last_end..start).unwrap())?;
				f.write_str(self.to)?;
				last_end = start + part.len();
			}
			// UNWRAP: last_end is well-aligned still
			f.write_str(self.source.get(last_end..).unwrap())?;
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
pub fn concat<I>(iterator: I) -> impl core::fmt::Display
where
	I: IntoIterator,
	I::Item: core::fmt::Display,
	I::IntoIter: Clone,
{
	struct Concat<I> {
		iterator: I,
	}

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

/// Write a Display object into a fixed-size buffer and returns the resulting &mut str.
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
		fn write_str(&mut self, s: &str) -> Result<(), core::fmt::Error> {
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
