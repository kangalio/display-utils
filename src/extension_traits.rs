/// Extension trait providing convenient access to the base functions in the crate root.
pub trait IteratorExt: Iterator + Sized + Clone
where
	Self::Item: core::fmt::Display,
{
	/// Alias for [`crate::concat`]
	fn display_concat(self) -> crate::Concat<Self> {
		crate::concat(self)
	}

	/// Alias for [`crate::join`]
	fn display_join<J>(self, joiner: J) -> crate::Join<Self, J>
	where
		J: core::fmt::Display,
	{
		crate::join(self, joiner)
	}

	/// Alias for [`crate::join_format`]
	fn display_join_format<J, C>(self, joiner: J, callback: C) -> crate::JoinFormat<Self, J, C>
	where
		J: core::fmt::Display,
		C: Fn(Self::Item, &mut core::fmt::Formatter) -> core::fmt::Result,
	{
		crate::join_format(self, joiner, callback)
	}
}
impl<I: Iterator + Clone> IteratorExt for I where I::Item: core::fmt::Display {}

/// Extension trait providing convenient access to the base functions in the crate root.
pub trait DisplayExt: core::fmt::Display
where
	Self: Sized,
{
	/// Alias for [`crate::collect_str`]
	fn collect_str(self, buf: &mut [u8]) -> Result<&str, &str> {
		crate::collect_str(buf, self)
	}

	/// Alias for [`crate::collect_str_mut`]
	fn collect_str_mut(self, buf: &mut [u8]) -> Result<&mut str, &mut str> {
		crate::collect_str_mut(buf, self)
	}

	/// Alias for [`crate::lowercase`]
	fn display_lowercase(self) -> crate::Lowercase<Self> {
		crate::lowercase(self)
	}

	/// Alias for [`crate::uppercase`]
	fn display_uppercase(self) -> crate::Uppercase<Self> {
		crate::uppercase(self)
	}

	/// Alias for [`crate::repeat`]
	fn display_repeat(self, times: usize) -> crate::Repeat<Self> {
		crate::repeat(self, times)
	}

	/// Alias for [`crate::slice`]
	fn extract_slice<R: core::ops::RangeBounds<usize>>(
		self,
		range: R,
	) -> crate::DisplaySlice<Self> {
		crate::slice(self, range)
	}

	/// Alias for [`crate::cmp`]
	fn display_cmp(self, other: &str) -> core::cmp::Ordering {
		crate::cmp(self, other)
	}
}
impl<T: core::fmt::Display> DisplayExt for T {}
