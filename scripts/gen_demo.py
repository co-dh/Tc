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
_HIDE_INFO = ("", None, "I", 0.3)  # turn off info overlay silently

def F(cli_args, steps):
    """Feature with info overlay disabled."""
    return (cli_args, [_HIDE_INFO] + steps)

# -- Feature definitions: (cli_args, steps) ------------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)
# IMPORTANT: Keys that open fzf (\ = : space / s) need a separate step
# before typing into the fzf prompt — fzf needs startup time.

FEATURES = {
    "demo": F("data/", [
        # Act 1: Folder browse
        ("Browse folder",                 "tv data/",    None,       3.5),
        ("Sort by size",                  "l ]",         "l]",       3.5),
        # Act 2: Open parquet → sparklines
        ("Open nyse10k.parquet",          "Enter",       "jjjjj\r", 3.5),
        ("Sparklines show distributions", None,          None,       3.5),
        # Act 3: fzf command menu
        ("Command menu",                  "Space",       " ",        2.0),
        ("Theme cycle",                   "th Enter",    "th\r",     3.0),
        # Act 4: Frequency analysis
        ("Move to Exchange",              "l",           "l",        2.0),
        ("Frequency view",                "F",           "F",        3.5),
        ("Filter by value",               "j Enter",     "j\r",      3.5),
        ("Filtered rows",                 None,          None,       2.5),
        # Act 5: Histogram plot
        ("Move to Bid_Price",             "lll",         "lll",      2.0),
        ("Command menu",                  "Space",       " ",        2.0),
        ("Histogram plot",                "hist Enter",  "hist\r",   5.0),
        ("Exit plot",                     "q",           "q",        2.5),
    ]),

    "folder": F("data/", [
        ("Browse folder",   "tv data/",  None,  3.0),
        ("Sort by size",    "] desc",    "l]",  3.5),
        ("Navigate",        "j j j",     "jjj", 3.0),
    ]),

    "sparkline": F(NYSE, [
        ("Sparkline distributions", None, None, 4.0),
    ]),

    "freq": F(NYSE, [
        ("Move to Exchange", "l",       "l",   2.0),
        ("Frequency view",   "F",       "F",   3.5),
        ("Filter by value",  "j Enter", "j\r", 3.5),
        ("Filtered rows",    None,      None,  3.0),
    ]),

    "heatmap": F(NYSE, [
        ("Heatmap numeric",     "Space",  " ",      1.5),
        ("",                    None,     "hea\r",  2.5),
        ("Heatmap categorical", "Space",  " ",      1.5),
        ("",                    None,     "hea\r",  2.5),
        ("Heatmap both",        "Space",  " ",      1.5),
        ("",                    None,     "hea\r",  2.5),
    ]),

    "plot": F(NYSE, [
        ("Move to Bid_Price",  "lll",        "lll",    2.0),
        ("Command menu",       "Space",      " ",      2.0),
        ("Histogram",          "hist Enter", "hist\r", 5.0),
        ("Exit plot",          "q",          "q",      2.0),
    ]),

    "fzf": F(NYSE, [
        ("Command palette", "Space",    " ",    2.5),
        ("Search & select", "th Enter", "th\r", 3.5),
    ]),

    "meta": F(NYSE, [
        ("Meta view", "M", "M", 4.0),
    ]),

    "sort": F(NYSE, [
        ("Sort asc",  "[", "l[", 3.0),
        ("Sort desc", "]", "l]", 3.0),
    ]),

    "split": F(NYSE, [
        ("Split column",    ":",  ":",   1.5),  # : opens fzf — separate step
        ("Split Time by -", None, "-\r", 3.5),
        ("New columns",     None, None,  3.0),
    ]),

    "filter": F(NYSE, [
        ("PRQL filter",   "\\",  "\\",                2.0),  # \ opens fzf — separate step
        ("",              None,  "Bid_Price > 100\r", 3.5),
        ("Filtered rows", None,  None,                3.5),
    ]),

    "derive": F(NYSE, [
        ("Derive column", "=",  "=",             1.5),  # = opens fzf — separate step
        ("",              None, "Bid_Price * 2", 3.5),  # pause at fzf for user to read
        ("New column",    None, "\r",            3.0),
    ]),

    # diff: open folder, use S(swap) to open both files, then V to diff
    # folder sorts asc: row0=.., row1=after.csv, row2=before.csv
    "diff": F("data/diff_test/", [
        ("Open before.csv",  "jj Enter",  "[jj\r", 2.5),  # sort asc, nav to row2, open
        ("Back to folder",   "S",         "S",     1.5),   # swap: folder on top
        ("Open after.csv",   "k Enter",   "k\r",   2.5),   # up to row1=after, open
        ("Diff view",        "V",         "SqV",   4.0),   # swap, pop folder, diff
    ]),

    "theme": F(NYSE, [
        ("Theme 1", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
        ("Theme 2", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
        ("Theme 3", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
    ]),

    "s3": F("s3://nyc-tlc/ +n", [
        ("S3 public bucket", "tv s3://nyc-tlc/ +n", None, 4.0),
        ("Navigate",         "j j",                 "jj", 3.5),
    ]),

    "hf": F("hf://datasets/stanfordnlp/imdb", [
        ("HuggingFace dataset", "tv hf://...imdb", None, 4.0),
        ("Navigate",            "j j",             "jj", 3.5),
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

        try:
            for desc, keys_shown, keys, pause in steps:
                if child_dead:
                    break
                if keys is not None:
                    for ch in keys:
                        drain(0.05)
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
