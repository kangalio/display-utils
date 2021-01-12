pub struct Wrapper<T> {
	pub(crate) inner: T,
}

impl<T: core::fmt::Display> Wrapper<T> {}

impl<T: core::fmt::Display> core::fmt::Display for Wrapper<T> {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		self.inner.fmt(f)
	}
}

// impl<T: core::fmt::Display> core::fmt::Debug for Wrapper<T> {
// 	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
// 		todo!() // STUB
// 	}
// }

impl<T: core::fmt::Display> PartialEq<str> for Wrapper<T> {
	fn eq(&self, other: &str) -> bool {
		self.cmp(other) == core::cmp::Ordering::Equal
	}
}

impl<T: core::fmt::Display> PartialOrd<str> for Wrapper<T> {
	fn partial_cmp(&self, reference: &str) -> Option<core::cmp::Ordering> {
		Some(self.cmp(reference))
	}
}

// trait SliceIndex {}
// impl<T: core::fmt::Display, I: SliceIndex> core::ops::Index<I> for Wrapper<T> {}

// TODO: Implement core::str::Pattern for Wrapper once stabilized
