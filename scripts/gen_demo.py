#!/usr/bin/env python3
"""Record tv demo GIFs as asciinema .cast via scripted keystrokes in a real terminal.

Usage:
  python3 scripts/gen_demo.py           # record all GIFs
  python3 scripts/gen_demo.py folder    # record one feature
"""
import os, pty, select, signal, struct, sys, json, time, fcntl, termios, subprocess

TC = ".lake/build/bin/tv"
AGG = os.environ.get("AGG", "agg")
W, H = 80, 24
FONT = 20
BOX_W = int(W * 0.618)  # golden ratio title box

NYSE = "data/nyse10k.parquet"
_HIDE_INFO = ("", None, "I", 0.3)  # turn off info overlay

def F(cli_args, steps):
    """Feature with info overlay disabled."""
    return (cli_args, [_HIDE_INFO] + steps)

# -- Feature definitions: (cli_args, steps) ------------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)
# IMPORTANT: Keys that open fzf (\ = : space / s) need a separate step
# before typing into the fzf prompt — fzf needs startup time.

FEATURES = {
    # folder: enter subfolder, backspace to parent, open csv, open parquet
    # sorted asc: row0=.., row1=basic.csv, row2=diff_test(dir), row3=filtered_test.parquet
    "folder": F("data/", [
        ("Browse a folder of data files",                     "tv data/",   None,    3.0),
        ("Enter a subfolder",                                 "jj Enter",   "jj\r",  3.0),  # row2=diff_test
        ("Backspace goes to parent folder",                   "Backspace",  "\x7f",  3.0),
        ("Press / to search for a file",                      "/",          "/",     2.0),  # fzf step
        ("Type a name to jump to it",                         None,         "nyse\r", 2.5),
        ("Open the parquet file\nPress q to go back",         "Enter",      "\r",    3.0),
        ("",                                                  None,         "q",     1.0),
        ("Open a CSV file",                                   "j Enter",    "j\r",   3.0),  # next file after nyse
    ]),

    "sparkline": F(NYSE, [
        ("Each column header has a sparkline showing the value distribution", None, None, 5.0),
    ]),

    "freq": F(NYSE, [
        ("Move cursor to Exchange column",                 "l",       "l",   2.0),
        ("Press F for frequency count",                    "F",       "F",   3.5),
        ("Select a value and press Enter\nOnly matching rows remain", "j Enter", "j\r", 4.0),
    ]),

    # heatmap: title shows AFTER apply so text overlays the colored table.
    # hea Enter always sends .inc (0→1→2→3, capped at 3).
    "heatmap": F(NYSE, [
        ("",                                    None,     " ",      0.5),
        ("Mode 1: color numeric columns",       None,     "hea\r",  3.5),
        ("",                                    None,     " ",      0.5),
        ("Mode 2: color categorical columns",   None,     "hea\r",  3.5),
        ("",                                    None,     " ",      0.5),
        ("Mode 3: color all columns",           None,     "hea\r",  3.5),
    ]),

    "plot": F(NYSE, [
        ("Move cursor to a numeric column",                  "lll",        "lll",    2.0),
        ("Open command menu with Space",                     "Space",      " ",      3.0),  # fzf needs startup
        ("Render a histogram with ggplot2\nPress q to close", "hist Enter", "hist\r", 5.0),
        ("",                                                 None,         "q",      1.0),
    ]),

    "fzf": F(NYSE, [
        ("Press Space to open the command menu", "Space",    " ",    2.5),
        ("Type to search, Enter to run",         "th Enter", "th\r", 3.5),
    ]),

    "meta": F(NYSE, [
        ("M shows column names, types, nulls, and unique counts", "M",     "M",  3.5),
        ("Press 0 to select all-null columns\nPress 1 to select single-value columns", "0 1", "01", 3.5),
        ("Enter hides the selected columns from the table",       "Enter", "\r", 3.5),
    ]),

    "sort": F(NYSE, [
        ("Press [ to sort ascending\nPress ] to sort descending", "[", "l[", 3.0),
        ("",                                                      None, "l]", 3.0),
        ("Press ! to pin a column as key (left)\nPress ! again to unpin", "!", "l!", 3.0),
        ("",                                                      None, "!",  3.0),
    ]),

    # split: use split_test.csv which has "a-b" values to split on "-"
    "split": F("data/split_test.csv", [
        ("A table with a column containing a-b values", None, None,          3.0),
        ("Press : to split a column by a delimiter",    None, None,          2.0),
        ("",                                            None, ":...",        3.0),  # fzf char loss padding
        ("",                                            None, "\x15-\r",     5.0),  # ctrl-u clear, type -, enter
    ]),

    "filter": F(NYSE, [
        ("Press \\ to open the filter prompt",                      None,  None,                        2.0),
        ("",                                                        None,  "\\.....",                    3.0),  # fzf char loss padding
        ("Type a PRQL expression\nPress Enter to apply the filter", None,  "\x15Sym ~= 'AAP'\r",        4.0),
    ]),

    # derive: pad with dots before real input — fzf eats first chars during startup.
    # ctrl-u clears the line so only the real expression remains.
    "derive": F("data/numeric.csv", [
        ("A simple table with columns x, y, z",               None, None,                     3.0),
        ("Press = to open the derive prompt",                  None, None,                     2.0),
        ("",                                                   None, "=......",                3.0),  # dots absorb fzf char loss
        ("",                                                   None, "\x15double = x * 2",     3.0),  # ctrl-u clears, then type
        ("",                                                   None, "\r",                     2.0),  # enter, back to table
        ("The new 'double' column appears",                    None, "gl",                     4.0),
    ]),

    # diff: open folder, open first table, swap back, open second, then V to diff
    # folder sorts asc: row0=.., 1=after, 2=before, 3=first, 4=second
    "diff": F("data/diff_test/", [
        ("Open the first table",                               "jjjj Enter", "[jjjj\r", 2.5),
        ("S swaps back to the folder",                         "S",          "S",       1.5),
        ("Open the second table",                              "j Enter",    "j\r",     2.5),
        ("V compares the two tables\nChanged columns get a delta prefix", "V", "SqV",   5.0),
    ]),

    # "theme": F(NYSE, [
    #     ("Cycle through color themes",  "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    #     ("Each theme changes all colors", "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    #     ("Pick the one you like",       "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    # ]),

    # "s3": F("s3://nyc-tlc/ +n", [
    #     ("Browse S3 buckets like folders", "tv s3://nyc-tlc/ +n", None, 4.0),
    #     ("Navigate and open files",        "j j",                 "jj", 3.5),
    # ]),

    "hf": F("hf://", [
        ("List all HuggingFace datasets",                        "tv hf://",  None,   4.0),
        ("Sort by downloads and open the top dataset\nBrowse the dataset files", "] Enter", "l]\r", 5.0),
    ]),
}

# -- Title overlay -------------------------------------------------------------

def title_escape(desc, keys):
    """Centered golden-ratio title box in the middle of the screen.
    Supports multi-line descriptions via \\n in desc string."""
    if not desc:
        return ""
    first = f"{desc.split(chr(10))[0]}  ({keys})" if keys else desc.split(chr(10))[0]
    lines = [first] + desc.split(chr(10))[1:] if chr(10) in desc else [first]
    # pad each line to box width
    rendered = []
    for ln in lines:
        pad = max(BOX_W - len(ln), 0)
        left = pad // 2
        rendered.append(" " * left + ln + " " * (pad - left))
    blank = " " * BOX_W
    margin = (W - BOX_W) // 2
    indent = f"\x1b[{margin + 1}G"
    # center vertically: box = blank + lines + blank
    box_h = len(rendered) + 2
    row = H // 2 - box_h // 2
    esc = "\x1b[1;97;44m"
    rst = "\x1b[0m"
    out = f"\x1b7\x1b[{row};1H{indent}{esc}{blank}{rst}"
    for i, ln in enumerate(rendered):
        out += f"\x1b[{row+1+i};1H{indent}{esc}{ln}{rst}"
    out += f"\x1b[{row+1+len(rendered)};1H{indent}{esc}{blank}{rst}\x1b8"
    return out

# -- Recording engine ----------------------------------------------------------

def record(cli_args, steps, cast_path):
    os.makedirs(os.path.dirname(cast_path), exist_ok=True)
    env = {**os.environ,
           "LD_LIBRARY_PATH": "/usr/local/lib:" + os.environ.get("LD_LIBRARY_PATH", ""),
           "TERM": "xterm-256color"}
    env.pop("TMUX", None)  # fzf --tmux fails in pty; force inline mode

    header = {"version": 2, "width": W, "height": H,
              "env": {"TERM": "xterm-256color", "SHELL": "/bin/bash"}}
    args = [TC] + cli_args.split()
    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(TC, args, env)

    winsize = struct.pack("HHHH", H, W, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)
    os.kill(pid, signal.SIGWINCH)
    t0 = time.monotonic()
    child_dead = False

    with open(cast_path, "w") as cast_f:
        cast_f.write(json.dumps(header) + "\n")

        def emit(text):
            t = time.monotonic() - t0
            cast_f.write(json.dumps([round(t, 3), "o", text]) + "\n")
            sys.stdout.buffer.write(text.encode("utf-8", errors="replace"))
            sys.stdout.buffer.flush()

        def drain(timeout=0.1):
            nonlocal child_dead
            buf = b""
            while True:
                r, _, _ = select.select([fd], [], [], timeout)
                if not r:
                    break
                try:
                    chunk = os.read(fd, 65536)
                    if not chunk:
                        child_dead = True
                        break
                    buf += chunk
                except OSError:
                    child_dead = True
                    break
                timeout = 0.01  # fast follow-up reads after first data arrives
            if buf:
                t = time.monotonic() - t0
                text = buf.decode("utf-8", errors="replace")
                cast_f.write(json.dumps([round(t, 3), "o", text]) + "\n")
                sys.stdout.buffer.write(buf)
                sys.stdout.buffer.flush()

        # Warmup: wait for tv to call tb_init() and render first frame.
        # tb_init uses TCSAFLUSH which discards pending pty input.
        # drain catches the first render, then sleep ensures tb_poll_event is ready.
        drain(2.0)
        time.sleep(1.0)

        try:
            for desc, keys_shown, keys, pause in steps:
                if child_dead:
                    break
                if keys is not None:
                    # No drain() between chars: reading pty output mid-keystroke
                    # causes fzf to lose input chars (pty flow control issue).
                    for ch in keys:
                        os.write(fd, ch.encode())
                        time.sleep(0.08)
                drain(0.5)
                title = title_escape(desc, keys_shown)
                if title:
                    emit(title)
                time.sleep(pause)
                drain(0.1)
        except OSError:
            pass

        # Don't send Q — it clears the screen (alternate buffer exit),
        # leaving a black frame at the end of the GIF.

    # cleanup: kill child, don't wait for graceful exit
    os.close(fd)
    try:
        os.kill(pid, signal.SIGTERM)
        _, status = os.waitpid(pid, 0)
        rc = os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1
    except (ChildProcessError, ProcessLookupError, OSError):
        rc = -1

    elapsed = time.monotonic() - t0
    if child_dead and rc != 0:
        print(f"  ERROR: {TC} died early (exit {rc}, {elapsed:.1f}s)")
        return False
    print(f"  {cast_path} ({elapsed:.1f}s)")
    return True

def gen(name):
    cli_args, steps = FEATURES[name]
    cast = f"doc/{name}.cast"
    gif = f"doc/{name}.gif"
    if not record(cli_args, steps, cast):
        if os.path.exists(cast):
            os.remove(cast)
        return False
    subprocess.run([AGG, cast, gif, "--font-size", str(FONT)], check=True)
    os.remove(cast)
    sz = os.path.getsize(gif)
    print(f"  {gif} ({sz // 1024}K)")
    return True

if __name__ == "__main__":
    names = sys.argv[1:] or list(FEATURES.keys())
    for name in names:
        if name not in FEATURES:
            print(f"Unknown feature: {name}. Available: {', '.join(FEATURES)}")
            sys.exit(1)
    failed = []
    for name in names:
        print(f"[{name}]")
        if not gen(name):
            failed.append(name)
    if failed:
        print(f"\nFAILED: {', '.join(failed)}")
        sys.exit(1)
