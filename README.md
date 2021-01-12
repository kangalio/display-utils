This no_std compatible library provides several useful constructs to format data in a human-readable
fashion with zero allocations.

Why hand-roll annoying and verbose formatting code everywhere...
```rust
for i, item in list.iter().enumerate() {
	if i == list.len() - 1 {
		println!("{}", item);
	} else {
		print!("{} - ", item);
	}
}
```
...when you could just use this?
```rust
println!("{}", display_utils::join(list, " - "));
```

This library makes ingenious use of Rust's formatting abstractions to provide super flexible and
powerful formatting utilities, without any allocations.

For more information, please see the documentation: https://docs.rs/display_utils