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
        ("Browse a folder of data files",    "tv data/",   None,   3.0),
        ("Enter a subfolder",                "jj Enter",   "jj\r", 3.0),  # row2=diff_test
        ("Backspace goes to parent folder",  "Backspace",  "\x7f", 3.0),
        ("Open a CSV file as a table",       "j Enter",    "j\r",  3.0),  # row1=basic.csv
        ("Press q to go back to folder",     "q",          "q",    2.0),
        ("Open a parquet file",              "jj Enter",   "jj\r", 3.0),  # row3=filtered_test.parquet
    ]),

    "sparkline": F(NYSE, [
        ("Each column header has a sparkline showing the value distribution", None, None, 5.0),
    ]),

    "freq": F(NYSE, [
        ("Move cursor to Exchange column",  "l",       "l",   2.0),
        ("Press F for frequency count",     "F",       "F",   3.5),
        ("Select a value to filter by",     "j Enter", "j\r", 3.5),
        ("Only matching rows remain",       None,      None,  3.0),
    ]),

    "heatmap": F(NYSE, [
        ("Mode 1: color numeric columns",       "Space",  " ",      1.5),
        ("",                                    None,     "hea\r",  2.5),
        ("Mode 2: color categorical columns",   "Space",  " ",      1.5),
        ("",                                    None,     "hea\r",  2.5),
        ("Mode 3: color all columns",           "Space",  " ",      1.5),
        ("",                                    None,     "hea\r",  2.5),
        ("Mode 0: heatmap off",                 "Space",  " ",      1.5),
        ("",                                    None,     "hea\r",  2.5),
    ]),

    "plot": F(NYSE, [
        ("Move cursor to a numeric column",  "lll",        "lll",    2.0),
        ("Open command menu with Space",     "Space",      " ",      2.0),
        ("Render a histogram with ggplot2",  "hist Enter", "hist\r", 5.0),
        ("Press q to close the plot",        "q",          "q",      2.0),
    ]),

    "fzf": F(NYSE, [
        ("Press Space to open the command menu", "Space",    " ",    2.5),
        ("Type to search, Enter to run",         "th Enter", "th\r", 3.5),
    ]),

    "meta": F(NYSE, [
        ("M shows column names, types, nulls, and unique counts", "M",     "M",  3.5),
        ("Press 0 to select columns that have null values",       "0",     "0",  3.0),
        ("Press 1 to select columns with only one unique value",  "1",     "1",  3.0),
        ("Enter hides the selected columns from the table",       "Enter", "\r", 3.5),
    ]),

    "sort": F(NYSE, [
        ("Press [ to sort the current column ascending",  "[", "l[", 3.0),
        ("Press ] to sort the current column descending", "]", "l]", 3.0),
    ]),

    "split": F(NYSE, [
        ("Press : to split a column by a delimiter", ":",  ":",   1.5),  # fzf step
        ("Type the delimiter and press Enter",       None, "-\r", 3.5),
        ("New columns appear from the split parts",  None, None,  3.0),
    ]),

    "filter": F(NYSE, [
        ("Press \\ to open PRQL filter",          "\\",  "\\",              3.0),  # fzf step
        ("Type a filter expression",              None,  "Bid_Price > 100", 3.5),
        ("Press Enter to apply the filter",       None,  "\r",              3.5),
    ]),

    "derive": F(NYSE, [
        ("Press = to create a new computed column", "=",  "=",             1.5),  # fzf step
        ("Type an expression using column names",   None, "Bid_Price * 2", 3.5),
        ("Press Enter to add the new column",       None, "\r",            3.0),
    ]),

    # diff: open folder, use S(swap) to open both files, then V to diff
    # folder sorts asc: row0=.., row1=after.csv, row2=before.csv
    "diff": F("data/diff_test/", [
        ("Step 1: open the before table",         "jj Enter", "[jj\r", 2.5),
        ("Step 2: S swaps back to the folder",    "S",        "S",     1.5),
        ("Step 3: open the after table",          "k Enter",  "k\r",   2.5),
        ("Step 4: V compares the two tables",     "V",        "SqV",   4.0),
        ("Changed columns get a delta prefix",    None,       None,    3.5),
    ]),

    # "theme": F(NYSE, [
    #     ("Cycle through color themes",  "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    #     ("Each theme changes all colors", "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    #     ("Pick the one you like",       "Space", " ",    1.5),
    #     ("",                            None,    "th\r", 2.5),
    # ]),

    "s3": F("s3://nyc-tlc/ +n", [
        ("Browse S3 buckets like folders", "tv s3://nyc-tlc/ +n", None, 4.0),
        ("Navigate and open files",        "j j",                 "jj", 3.5),
    ]),

    "hf": F("hf://", [
        ("List all HuggingFace datasets",   "tv hf://",  None,   4.0),
        ("Sort by downloads",               "]",         "l]",   3.5),
        ("Open the top dataset",            "Enter",     "\r",   4.0),
        ("Browse the dataset files",        None,        None,   3.5),
    ]),
}

# -- Title overlay -------------------------------------------------------------

def title_escape(desc, keys):
    """Centered golden-ratio title box in the middle of the screen."""
    if not desc:
        return ""
    text = f"{desc}  ({keys})" if keys else desc
    pad = max(BOX_W - len(text), 0)
    left = pad // 2
    line = " " * left + text + " " * (pad - left)
    blank = " " * BOX_W
    # center the box horizontally
    margin = (W - BOX_W) // 2
    indent = f"\x1b[{margin + 1}G"  # move cursor to column
    row = H // 2 - 1
    esc = "\x1b[1;97;44m"
    rst = "\x1b[0m"
    return (f"\x1b7"
            f"\x1b[{row};1H{indent}{esc}{blank}{rst}"
            f"\x1b[{row+1};1H{indent}{esc}{line}{rst}"
            f"\x1b[{row+2};1H{indent}{esc}{blank}{rst}"
            f"\x1b8")

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

        if not child_dead:
            try:
                os.write(fd, b"Q")
                time.sleep(0.3)
                drain(0.1)
            except OSError:
                pass

    # cleanup pty and child process
    os.close(fd)
    try:
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
