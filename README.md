This library provides several useful constructs to format data in a human-readable fashion with
zero allocations

Some of these functions may seem to partly reinvent existing std functionality, for example
[`join`](https://docs.rs/display-utils/0/display-utils/fn.join.html):

```rust
println!("{}", display_utils::join(&[1, 2, 3], " + ")); // display_utils
println!("{}", ["1", "2", "3"].join(" + ")); // std

println!("{}", display_utils::repeat("abc", 4)); // display_utils
println!("{}", "abc".repeat(4)); // std
```

The important difference is that the std approach involves 4 allocations, whereas the
display_utils approach operates 100% on stack and is therefore no_std compatible and likely 
faster.