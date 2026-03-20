#!/usr/bin/env python3
"""Record tc demo as asciinema .cast via scripted keystrokes in a real terminal.

Usage: python3 scripts/gen_demo.py
  Records doc/demo.cast, then run: agg doc/demo.cast doc/demo.gif --font-size 14
"""
import os, pty, select, signal, struct, sys, json, time, fcntl, termios

TC = ".lake/build/bin/tc"
CAST = "doc/demo.cast"
W, H = 80, 24

def title_escape(desc, keys):
    """Centered movie-title overlay: 3-row box in the middle of the screen."""
    text = f"{desc}  ({keys})" if keys else desc
    pad = max(W - len(text), 0)
    left = pad // 2
    line = " " * left + text + " " * (pad - left)
    blank = " " * W
    row = H // 2 - 1  # vertically centered
    esc = "\x1b[1;97;44m"  # bold white on blue
    rst = "\x1b[0m"
    return (f"\x1b7"
            f"\x1b[{row};1H{esc}{blank}{rst}"
            f"\x1b[{row+1};1H{esc}{line}{rst}"
            f"\x1b[{row+2};1H{esc}{blank}{rst}"
            f"\x1b8")

# (description, keys_shown, keys_to_send, pause_seconds)
STEPS = [
    # Act 1: Folder browse (5s)
    ("Browse folder",                 "tc data/",    None,       2.5),
    ("Sort by size",                  "l ]",         "l]",       2.5),

    # Act 2: Open parquet → sparklines appear automatically (5s)
    ("Open nyse10k.parquet",          "Enter",       "jjjjj\r", 2.5),
    ("Sparklines show distributions", None,          None,       2.5),

    # Act 3: fzf command menu (7s)
    ("Command menu",                  "Space",       " ",        1.5),
    ("Theme cycle",                   "th Enter",    "th\r",     2.0),
    ("Command menu",                  "Space",       " ",        1.5),
    ("Heatmap toggle",                "hea Enter",   "hea\r",    2.0),

    # Act 4: Frequency analysis (8s)
    ("Move to Exchange",              "l",           "l",        1.5),
    ("Frequency view",                "F",           "F",        2.5),
    ("Filter by value",               "j Enter",     "j\r",      2.5),
    ("Filtered rows",                 None,          None,       1.5),

    # Act 5: Histogram plot (9s)
    ("Move to Bid_Price",             "lll",         "lll",      1.5),
    ("Command menu",                  "Space",       " ",        1.5),
    ("Histogram plot",                "hist Enter",  "hist\r",   4.0),
    ("Exit plot",                     "q",           "q",        2.0),
]

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
            emit(title_escape(desc, keys_shown))
            time.sleep(pause)
            drain(0.1)
    except OSError:
        pass

    # Force-quit tc
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
    print(f"\nWrote {cast_path} ({t_total:.1f}s)")

if __name__ == "__main__":
    record("data/", STEPS, CAST)
    print(f"Now run: agg {CAST} doc/demo.gif --font-size 14")
