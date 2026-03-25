# fzf Socket Bind Pattern

Live-preview via fzf arrow keys + unix socket. Removed in favor of direct
selection (m0-m3), but the pattern is reusable for any fzf menu that needs
live cycling through options.

## How it worked

1. Menu items prefixed with `!` are "previewable" (pure visual, no IO)
2. fzf `left`/`right` arrow keys trigger a helper script via `execute-silent`
3. Helper script extracts the 2-char command code, replaces the verb char
   with `-`/`+`, sends it through the unix socket
4. Main loop's poll callback receives the socket command, applies it, re-renders

## Why a helper script

fzf `--tmux` re-launches inside a tmux popup. Special shell chars (`>`, `|`,
`$`, `"`) in `execute-silent` get corrupted during re-launch. Writing a script
to tmpdir avoids this — the bind string has no special chars.

## Lean code (was in Fzf.lean cmdMode)

```lean
let sockPath := (← IO.getEnv "TV_SOCK").getD ""
let tmp := (← IO.getEnv "TMPDIR").getD "/tmp"
let script := tmp ++ "/tv-fzf-send.sh"
let sockBinds ← if sockPath.isEmpty then pure #[]
  else do
    let lines := [
      "#!/bin/sh",
      "cmd=\"${1%%\t*}\"",  -- real tab: strip label after \t
      "case \"$cmd\" in '!'*) ;; *) exit 0;; esac",
      "code=\"${cmd#!}\"",
      "if [ \"$2\" = f ]; then printf '%s\\n' \"$code\"",
      "else printf '%s\\n' \"${code%?}$2\"",
      "fi | socat - UNIX-CONNECT:" ++ sockPath ++ " 2>/dev/null"
    ]
    IO.FS.writeFile script (String.intercalate "\n" lines ++ "\n")
    let _ ← IO.Process.output { cmd := "chmod", args := #["+x", script] }
    let bind (key dir : String) :=
      "--bind=" ++ key ++ ":execute-silent(" ++ script ++ " {} " ++ dir ++ ")"
    pure #[bind "focus" "f", bind "left" "-", bind "right" "+",
           "--header=← dec  → inc"]
let opts := #["--with-nth=2..", "--prompt=cmd "] ++ sockBinds
```

## Shell script breakdown

```sh
#!/bin/sh
cmd="${1%%	*}"                    # strip label after real tab
case "$cmd" in '!'*) ;; *) exit 0;; esac  # only previewable (! prefix)
code="${cmd#!}"                    # strip ! prefix → 2-char code
if [ "$2" = f ]; then              # focus event → send full code
  printf '%s\n' "$code"
else                               # left/right → replace verb char
  printf '%s\n' "${code%?}$2"     # e.g. "m+" → "m-" (left sends -)
fi | socat - UNIX-CONNECT:$SOCK 2>/dev/null
```

## flatItems previewable collapse

Previewable `<`/`>` verbs were collapsed into a single `!`-prefixed entry
(cycled via arrow keys), while non-previewable verbs got individual entries:

```lean
let previewable := (mk .inc).isPreviewable
let acc := if previewable && verbs.any (fun (k, _, _) => k == '<' || k == '>') then
  acc.push s!"!{mk .inc}\t{objLabel}"
else acc
verbs.foldl (fun acc (verbKey, verbLabel, verb) =>
  if (verbKey == '<' || verbKey == '>') && previewable then acc  -- skip, already collapsed
  else acc.push s!"{cmd}\t{objLabel} {verbLabel}"
) acc
```
