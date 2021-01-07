#![no_std]

//! This library provides several useful constructs to format data in a human-readable fashion with
//! zero allocations
//! 
//! Some of these functions seem to partly reinvent existing std functionality, for example
//! [`join`]:
//! 
//! ```rust
//! # use display_utils::*;
//! println!("{}", display_utils::join(&[1, 2, 3], " + ")); // display_utils
//! println!("{}", ["1", "2", "3"].join(" + ")); // std
//! 
//! println!("{}", display_utils::repeat("abc", 4)); // display_utils
//! println!("{}", "abc".repeat(4)); // std
//! ```
//! 
//! The important difference is that the straightforward std way involves 4 allocations, whereas
//! the display_utils way operates 100% on stack and is therefore no_std compatible and much faster.

/// Print a loading-style bar using the Unicode block characters. The bar is very high-resolution:
/// 8 states can be represented per character.
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
		UnicodeBlockBar  {
			num_full_blocks: max_length,
			midpoint: "",
			num_spaces: 0,
		}
	} else {
		UnicodeBlockBar {
			num_full_blocks: steps / 8,
			midpoint: &BLOCK_CHARS[steps % 8],
			num_spaces: max_length - (steps / 8 + 1),
		}
	}
}

/// ```rust
/// let expected_output = "\
/// █          
/// █▆         
/// ██▄        
/// ███▁       
/// ████       
/// ████▇      
/// █████▄     
/// ██████▂    
/// ███████    
/// ████████   
/// ████████▅  
/// █████████▃ 
/// ██████████ ";
/// 
/// assert_eq!(
///     display_utils::vertical_unicode_block_bars(
///         13,
///         [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0].iter().copied()
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
	const BLOCK_CHARS: &[&str] = &[" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];

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
						core::cmp::Ordering::Equal => &BLOCK_CHARS[8 - steps % 8],
						core::cmp::Ordering::Greater => &BLOCK_CHARS[8],
					})?;
				}
			}
			Ok(())
		}
	}

	VerticalUnicodeBlockBars { max_height, proportions: proportions.into_iter() }
}

/// Concatenate iterator elements, separating each element pair with a given joiner.
/// 
/// The returned Display object is meant for immediate consumption
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
	
	Join { iterator: iterator.into_iter(), joiner }
}

/// Concatenate iterator elements, separating each element pair with a given joiner. Each iterator
/// element can be formatted using a callback.
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
	C: FnMut(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
	J: core::fmt::Display,
{
	struct Join<I, C, J> {
		iterator_and_callback: core::cell::RefCell<(I, C)>,
		joiner: J,
	}

	impl<I, C, J> core::fmt::Display for Join<I, C, J>
	where
		I: Iterator,
		C: FnMut(I::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
		J: core::fmt::Display,
	{
		fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
			let mut ref_guard = self.iterator_and_callback.borrow_mut();
			let (iter, callback) = &mut *ref_guard;

			if let Some(first_item) = iter.next() {
				(callback)(first_item, f)?;
			}
			for remaining_item in iter {
				self.joiner.fmt(f)?;
				(callback)(remaining_item, f)?;
			}

			Ok(())
		}
	}
	
	Join { iterator_and_callback: core::cell::RefCell::new((iterator.into_iter(), callback)), joiner }
}

/// Repeat an object a certain number of times.
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

/// Indent to a given depth using the tab character. This is a shortcut for `repeat("\t", depth)`;
/// please see that function if you want to use a different indent string.
/// 
/// ```rust
/// # use display_utils::*;
/// assert_eq!(display_utils::indent(2).to_string(), "\t\t");
/// # assert_eq!(display_utils::indent(0).to_string(), "");
/// # assert_eq!(display_utils::indent(1).to_string(), "\t");
/// ```
pub fn indent(depth: usize) -> impl core::fmt::Display {
	repeat("\t", depth)
}