# Language Comparison: Lean 4 vs Rust vs F*

## F* Considerations

F* is .NET-based (originally implemented in F#), with extraction to:
- F# → .NET P/Invoke for FFI
- OCaml → ctypes for FFI
- C (KaRaMeL) → native interop

Ecosystem focused on verification: HACL* (crypto), miTLS (protocols).

For a TUI app like this, F* would be awkward - FFI goes through extraction layer, no TUI ecosystem.

## Lean 4 Experience (This Project)

| Aspect          | Expected           | Actual                         |
|--------         |----------          |--------                        |
| Dependent types | Rich use           | Mostly just `Fin` for bounds   |
| Theorem proving | Correctness proofs | Many `sorry`, hard to complete |
| Ecosystem       | Growing            | Built FFI from scratch         |
| Binary size     | Reasonable         | Large (runtime overhead)       |

## Rust Alternative

Advantages for this type of project:
- `polars` for fast dataframes
- `ratatui`/`crossterm` for TUI
- `arrow-rs` + `duckdb-rs` for ADBC
- Small binaries (~2MB)
- Huge ecosystem, great tooling
- Fast incremental builds

## Pure FP and AI Reasoning

Pure functional code helps AI assistance:
- No hidden state mutations to track
- Referential transparency
- Types as documentation
- Explicit data flow

But Rust can approximate this:
```rust
// Functional style, immutable by default
let cols: Vec<_> = data.iter()
    .filter(|r| r.valid)
    .map(|r| r.value)
    .collect();

// Ownership makes mutation explicit
fn process(data: Vec<Row>) -> Vec<Row>
```

Comparison for AI-friendliness:

| Factor           | Lean     | Rust      |
|--------          |------    |------     |
| Pure by default  | Yes      | Opt-in    |
| Types as docs    | Yes      | Yes       |
| Explicit effects | IO monad | Ownership |
| Training data    | Sparse   | Abundant  |

## Conclusion

Lean 4 shines when proofs provide value (compilers, crypto, math libraries). For systems tools like TUI apps where verification overhead doesn't pay off, Rust with functional patterns may be more practical.

Middle ground: Rust with functional style (iterator chains, immutable defaults, small pure functions).
