# Scriptability Design Options

Three designs, terse to rich, all composable.

## 1. Whitney style: string commands as the scripting language

Already half-built. `Cmd` is `Obj×Verb` = 2-char string (`"c+"` = col inc). Scriptability = accept a **pipe of these atoms** from stdin/CLI arg.

```
tv data.parquet -e "c+c+C.C.c+"
```

meaning: right, right, sort asc, sort asc, right. Compound ops get single-char aliases:

| char | meaning |
|------|---------|
| `f"expr"` | filter |
| `s.` / `s,` | sort asc/desc cursor col |
| `h` | hide cursor col |
| `g` | group cursor col |
| `t5` | take 5 |
| `>` | select marked cols |

A "script" is just a string of these. The `-c` test infra already does this — promote it to a user-facing feature. No new types needed, just expose `Cmd.parse?` on a longer string and feed the main loop.

**Pros**: zero new abstraction, testable today, Whitney-terse.
**Cons**: positional (must navigate to column by index), no column names in scripts.

## 2. Kakoune style: selection-first with named operations

Kakoune's model: **select**, then **act**. Map this to tables:

```
:col name age     # select columns by name (like kakoune 's' to select)
:hide              # act on selection
:sort -age +name   # sort desc age, asc name
:filter age > 30   # filter rows
:group name {count, sum age}  # group
:take 20
```

Implementation: add a `:` command-line mode (fzf prompts already exist). Parse into `Op` and `Query.pipe` it. The selection state (`colSel`, `rowSel`) already exists — `:hide` just means "hide selected".

This is the `Op` inductive made interactive. Script = newline-separated commands in a file:

```
tv data.parquet -s script.tv
```

**Pros**: readable, named columns, composable pipeline matches the `Query` model exactly.
**Cons**: needs a command parser (but `Op` already defines the grammar).

## 3. Hybrid: PRQL is already the scripting language

Tc already compiles PRQL → SQL. Just let users pass raw PRQL ops:

```
tv data.parquet -p "filter age > 30 | sort {-name} | take 20"
```

Or in-TUI via `:` mode, type PRQL fragments that get `Query.pipe`'d. `Op.render` already produces PRQL — make `Op.parse` the inverse. Round-trip: user types PRQL → parse to `Op` → pipe into `Query` → render → compile → execute.

Script file = `.prql` file. No new language.

**Pros**: users already know PRQL (or learn once), no custom grammar, compiler infra exists.
**Cons**: PRQL syntax is heavier than options 1-2 for simple ops.

## Current State

**Option 3** (`-p` PRQL flag) was implemented and later removed in favor of the **socket command interface**, which is more flexible — it allows external tools to send commands to a running Tc instance, supports both simple and compound operations, and doesn't require a separate CLI mode. The `-c` keystroke injection stays for testing.
