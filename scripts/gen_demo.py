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

# -- Feature definitions: (file_arg, steps) -----------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)

# nyse10k.parquet is at row 5 after size-desc sort in data/ folder
OPEN_NYSE = [("", None, "l]jjjjj\r", 1.5)]  # silent open helper

FEATURES = {
    "demo": ("data/", [
        # Act 1: Folder browse
        ("Browse folder",                 "tc data/",    None,       2.5),
        ("Sort by size",                  "l ]",         "l]",       2.5),
        # Act 2: Open parquet → sparklines
        ("Open nyse10k.parquet",          "Enter",       "jjjjj\r", 2.5),
        ("Sparklines show distributions", None,          None,       2.5),
        # Act 3: fzf command menu
        ("Command menu",                  "Space",       " ",        1.5),
        ("Theme cycle",                   "th Enter",    "th\r",     2.0),
        ("Command menu",                  "Space",       " ",        1.5),
        ("Heatmap toggle",                "hea Enter",   "hea\r",    2.0),
        # Act 4: Frequency analysis
        ("Move to Exchange",              "l",           "l",        1.5),
        ("Frequency view",                "F",           "F",        2.5),
        ("Filter by value",               "j Enter",     "j\r",      2.5),
        ("Filtered rows",                 None,          None,       1.5),
        # Act 5: Histogram plot
        ("Move to Bid_Price",             "lll",         "lll",      1.5),
        ("Command menu",                  "Space",       " ",        1.5),
        ("Histogram plot",                "hist Enter",  "hist\r",   4.0),
        ("Exit plot",                     "q",           "q",        2.0),
    ]),

    "folder": ("data/", [
        ("Browse folder",   "tc data/",  None,  2.0),
        ("Sort by size",    "] desc",    "l]",  2.5),
        ("Navigate",        "j j j",     "jjj", 2.0),
    ]),

    "sparkline": ("data/", OPEN_NYSE + [
        ("Sparkline distributions", None, None, 3.0),
    ]),

    "freq": ("data/", OPEN_NYSE + [
        ("Move to Exchange", "l",       "l",  1.0),
        ("Frequency view",   "F",       "F",  2.5),
        ("Filter by value",  "j Enter", "j\r", 2.5),
        ("Filtered rows",    None,      None, 2.0),
    ]),

    "heatmap": ("data/", OPEN_NYSE + [
        ("Heatmap on",       "Space",     " ",      1.0),
        ("",                 None,        "hea\r",  1.5),
        ("Heatmap mode 2",  "Space",      " ",      1.0),
        ("",                 None,        "hea\r",  1.5),
        ("Heatmap mode 3",  "Space",      " ",      1.0),
        ("",                 None,        "hea\r",  1.5),
    ]),

    "plot": ("data/", OPEN_NYSE + [
        ("Move to Bid_Price", "lll",        "lll",    1.0),
        ("Histogram",         "P h",        "Ph",     4.0),
        ("Exit plot",         "q",          "q",      1.5),
    ]),

    "fzf": ("data/", OPEN_NYSE + [
        ("Command palette", "Space",    " ",    1.5),
        ("Search & select", "th Enter", "th\r", 2.5),
    ]),

    "meta": ("data/", OPEN_NYSE + [
        ("Meta view",    "M",   "M",  3.0),
    ]),

    "sort": ("data/", OPEN_NYSE + [
        ("Sort asc",     "[",   "l[",  2.0),
        ("Sort desc",    "]",   "l]",  2.0),
    ]),

    "split": ("data/", OPEN_NYSE + [
        ("Split Time by -",  ":",    ":-\r",  2.5),
        ("New columns",      None,   None,    2.0),
    ]),

    "filter": ("data/", OPEN_NYSE + [
        ("PRQL filter",    "\\",   "\\Bid_Price > 100\r",  2.5),
        ("Filtered rows",  None,   None,                    2.5),
    ]),

    "derive": ("data/", OPEN_NYSE + [
        ("Derive column",  "=",   "=Bid_Price * 2\r",  2.5),
        ("New column",     None,  None,                 2.0),
    ]),

    "diff": ("data/", OPEN_NYSE + [
        # open nyse, push freq on Exchange, pop back, diff the two views
        ("Frequency view",  "F",   "lF",  1.5),
        ("Back to table",   "q",   "q",   1.5),
        ("Diff top 2",      "V",   "V",   3.0),
    ]),

    "theme": ("data/", OPEN_NYSE + [
        ("Theme 1",  "Space", " ",    0.8),
        ("",         None,    "th\r", 1.5),
        ("Theme 2",  "Space", " ",    0.8),
        ("",         None,    "th\r", 1.5),
        ("Theme 3",  "Space", " ",    0.8),
        ("",         None,    "th\r", 1.5),
    ]),
}

# -- Title overlay -------------------------------------------------------------

def title_escape(desc, keys):
    """Centered movie-title overlay: 3-row box in the middle of the screen."""
    if not desc:
        return ""
    text = f"{desc}  ({keys})" if keys else desc
    pad = max(W - len(text), 0)
    left = pad // 2
    line = " " * left + text + " " * (pad - left)
    blank = " " * W
    row = H // 2 - 1
    esc = "\x1b[1;97;44m"
    rst = "\x1b[0m"
    return (f"\x1b7"
            f"\x1b[{row};1H{esc}{blank}{rst}"
            f"\x1b[{row+1};1H{esc}{line}{rst}"
            f"\x1b[{row+2};1H{esc}{blank}{rst}"
            f"\x1b8")

# -- Recording engine ----------------------------------------------------------

def record(file_arg, steps, cast_path):
    os.makedirs(os.path.dirname(cast_path), exist_ok=True)
    env = {**os.environ,
           "LD_LIBRARY_PATH": "/usr/local/lib:" + os.environ.get("LD_LIBRARY_PATH", ""),
           "TERM": "xterm-256color"}
    env.pop("TMUX", None)  # fzf --tmux fails in pty; force inline mode

    header = {"version": 2, "width": W, "height": H,
              "env": {"TERM": "xterm-256color", "SHELL": "/bin/bash"}}
    cast_f = open(cast_path, "w")
    cast_f.write(json.dumps(header) + "\n")

    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(TC, [TC, file_arg], env)

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
    file_arg, steps = FEATURES[name]
    cast = f"doc/{name}.cast"
    gif = f"doc/{name}.gif"
    record(file_arg, steps, cast)
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
