#!/usr/bin/env python3
"""Record tc demo GIFs as asciinema .cast via scripted keystrokes in a real terminal.

Usage:
  python3 scripts/gen_demo.py           # record all GIFs
  python3 scripts/gen_demo.py folder    # record one feature
"""
import os, pty, select, signal, struct, sys, json, time, fcntl, termios, subprocess

TC = ".lake/build/bin/tc"
AGG = os.environ.get("AGG", "agg")
W, H = 80, 24
FONT = 20
BOX_W = int(W * 0.618)  # golden ratio title box

NYSE = "data/nyse10k.parquet"

# -- Feature definitions: (cli_args, steps) ------------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)

FEATURES = {
    "demo": ("data/", [
        # Act 1: Folder browse
        ("Browse folder",                 "tc data/",    None,       3.5),
        ("Sort by size",                  "l ]",         "l]",       3.5),
        # Act 2: Open parquet → sparklines
        ("Open nyse10k.parquet",          "Enter",       "jjjjj\r", 3.5),
        ("Sparklines show distributions", None,          None,       3.5),
        # Act 3: fzf command menu
        ("Command menu",                  "Space",       " ",        2.0),
        ("Theme cycle",                   "th Enter",    "th\r",     3.0),
        ("Command menu",                  "Space",       " ",        2.0),
        ("Heatmap toggle",                "hea Enter",   "hea\r",    3.0),
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

    "folder": ("data/", [
        ("Browse folder",   "tc data/",  None,  3.0),
        ("Sort by size",    "] desc",    "l]",  3.5),
        ("Navigate",        "j j j",     "jjj", 3.0),
    ]),

    "sparkline": (NYSE, [
        ("Sparkline distributions", None, None, 4.0),
    ]),

    "freq": (NYSE, [
        ("Move to Exchange", "l",       "l",   2.0),
        ("Frequency view",   "F",       "F",   3.5),
        ("Filter by value",  "j Enter", "j\r", 3.5),
        ("Filtered rows",    None,      None,  3.0),
    ]),

    "heatmap": (NYSE, [
        ("Heatmap on",       "Space",  " ",      1.5),
        ("",                 None,     "hea\r",  2.5),
        ("Heatmap mode 2",  "Space",   " ",      1.5),
        ("",                 None,     "hea\r",  2.5),
        ("Heatmap mode 3",  "Space",   " ",      1.5),
        ("",                 None,     "hea\r",  2.5),
    ]),

    "plot": (NYSE, [
        ("Move to Bid_Price", "lll",   "lll", 2.0),
        ("Histogram",         "P h",   "Ph",  5.0),
        ("Exit plot",         "q",     "q",   2.0),
    ]),

    "fzf": (NYSE, [
        ("Command palette", "Space",    " ",    2.5),
        ("Search & select", "th Enter", "th\r", 3.5),
    ]),

    "meta": (NYSE, [
        ("Meta view", "M", "M", 4.0),
    ]),

    "sort": (NYSE, [
        ("Sort asc",  "[", "l[", 3.0),
        ("Sort desc", "]", "l]", 3.0),
    ]),

    "split": (NYSE, [
        ("Split Time by -", ":", ":-\r", 3.5),
        ("New columns",     None, None,  3.0),
    ]),

    "filter": (NYSE, [
        ("PRQL filter",   "\\",  "\\Bid_Price > 100\r", 3.5),
        ("Filtered rows", None,  None,                   3.5),
    ]),

    "derive": (NYSE, [
        ("Derive column", "=", "=Bid_Price * 2\r", 3.5),
        ("New column",    None, None,               3.0),
    ]),

    "diff": (NYSE, [
        ("Frequency view", "F",  "lF", 2.5),
        ("Back to table",  "q",  "q",  2.5),
        ("Diff top 2",     "V",  "V",  4.0),
    ]),

    "theme": (NYSE, [
        ("Theme 1", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
        ("Theme 2", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
        ("Theme 3", "Space", " ",    1.5),
        ("",        None,    "th\r", 2.5),
    ]),

    "s3": ("s3://nyc-tlc/ +n", [
        ("S3 public bucket", "tc s3://nyc-tlc/ +n", None, 4.0),
        ("Navigate",         "j j",                 "jj", 3.5),
    ]),

    "hf": ("hf://datasets/stanfordnlp/imdb", [
        ("HuggingFace dataset", "tc hf://...imdb", None, 4.0),
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
    cast_f = open(cast_path, "w")
    cast_f.write(json.dumps(header) + "\n")

    args = [TC] + cli_args.split()
    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(TC, args, env)

    winsize = struct.pack("HHHH", H, W, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)
    os.kill(pid, signal.SIGWINCH)

    t0 = time.monotonic()

    def emit(text):
        t = time.monotonic() - t0
        cast_f.write(json.dumps([round(t, 3), "o", text]) + "\n")
        sys.stdout.buffer.write(text.encode("utf-8", errors="replace"))
        sys.stdout.buffer.flush()

    def drain(timeout=0.1):
        buf = b""
        while True:
            r, _, _ = select.select([fd], [], [], timeout)
            if not r:
                break
            try:
                chunk = os.read(fd, 65536)
                if not chunk:
                    break
                buf += chunk
            except OSError:
                break
            timeout = 0.01
        if buf:
            t = time.monotonic() - t0
            text = buf.decode("utf-8", errors="replace")
            cast_f.write(json.dumps([round(t, 3), "o", text]) + "\n")
            sys.stdout.buffer.write(buf)
            sys.stdout.buffer.flush()

    try:
        for desc, keys_shown, keys, pause in steps:
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

    try:
        os.write(fd, b"Q")
        time.sleep(0.3)
        drain(0.1)
    except OSError:
        pass

    try:
        os.waitpid(pid, os.WNOHANG)
        time.sleep(0.2)
        os.kill(pid, signal.SIGTERM)
        os.waitpid(pid, 0)
    except (ChildProcessError, ProcessLookupError, OSError):
        pass

    cast_f.close()
    t_total = time.monotonic() - t0
    print(f"  {cast_path} ({t_total:.1f}s)")

def gen(name):
    cli_args, steps = FEATURES[name]
    cast = f"doc/{name}.cast"
    gif = f"doc/{name}.gif"
    record(cli_args, steps, cast)
    subprocess.run([AGG, cast, gif, "--font-size", str(FONT)], check=True)
    os.remove(cast)
    sz = os.path.getsize(gif)
    print(f"  {gif} ({sz // 1024}K)")

if __name__ == "__main__":
    names = sys.argv[1:] or list(FEATURES.keys())
    for name in names:
        if name not in FEATURES:
            print(f"Unknown feature: {name}. Available: {', '.join(FEATURES)}")
            sys.exit(1)
    for name in names:
        print(f"[{name}]")
        gen(name)
