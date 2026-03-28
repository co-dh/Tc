#!/usr/bin/env python3
"""Record tv demo GIFs as asciinema .cast via scripted keystrokes in a real terminal.

Usage:
  python3 scripts/gen_demo.py           # record all GIFs
  python3 scripts/gen_demo.py folder    # record one feature
"""
import os, pty, select, signal, socket as sock_mod, struct, sys, json, time, fcntl, termios, subprocess

TC = ".lake/build/bin/tv"
AGG = os.environ.get("AGG", "agg")
W, H = 80, 24
FONT = 20
BOX_W = int(W * 0.618)  # golden ratio title box

NYSE = "data/nyse10k.parquet"
_HIDE_INFO = ("", None, "!i~", 0.3)  # turn off info overlay via socket

def F(cli_args, steps, expects=None):
    """Feature with info overlay disabled. expects: strings that must appear in cast."""
    return (cli_args, [_HIDE_INFO] + steps, expects or [])

# -- Feature definitions: (cli_args, steps) ------------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)
# IMPORTANT: Keys that open fzf (\ = : space / s) need a separate step
# before typing into the fzf prompt — fzf needs startup time.

FEATURES = {
    # folder: use / search to navigate (jj is fragile, depends on sort order)
    "folder": F("data/", [
        ("Browse a folder of data files",                     "tv data/",   None,    3.0),
        ("",                                                  None,         "/.....",   3.0),
        ("",                                                  None,         "\x15diff", 3.0),
        ("Enter a subfolder",                                 "/ diff Enter Enter", "\r\r",  3.0),
        ("Backspace goes to parent folder",                   "Backspace",  "\x7f",  3.0),
        ("",                                                  None,         "/.....",      3.0),
        ("",                                                  None,         "\x15nyse10k", 3.0),
        ("Cursor jumps to the matched file\nPress Enter to open", "/ nyse10k Enter Enter", "\r\r", 3.0),
        ("",                                                  None,         "q",     1.0),
        ("Open a CSV file",                                   "j Enter",    "j\r",   3.0),
    ], expects=["diff_test", "nyse10k.parquet"]),

    "sparkline": F(NYSE, [
        ("Each column header has a sparkline\nshowing the value distribution", None, None, 7.0),
    ], expects=["Bid_Pri"]),

    "freq": F(NYSE, [
        ("Move cursor to Exchange column",                 "l",       "l",   2.0),
        ("",                                               None,      " .....",       3.0),
        ("",                                               None,      "\x15^F+", 3.0),
        ("",                                                None,      "\r",           1.5),
        ("Frequency count of each Exchange value\nSelect a value and press Enter", "j Enter", "j", 5.0),
        ("",                                                None,      "\r",            1.0),
        ("Only matching rows remain",                       None,      None,            5.0),
    ], expects=["freq Exchange", "51.800", "filter Exchange"]),

    # heatmap: use ^code to match fzf item by obj/verb prefix
    "heatmap": F(NYSE, [
        ("",                                              None,  " .....",          3.0),
        ("",                                              None,  "\x15^i1",         3.0),
        ("",                                              None,  "\r",              1.0),
        ("Color numeric columns by value",                None,  None,              5.0),
        ("",                                              None,  None,              0.5),
        ("",                                              None,  " .....",          3.0),
        ("",                                              None,  "\x15^i2",         3.0),
        ("",                                              None,  "\r",              1.0),
        ("Color categorical columns by group",            None,  None,              5.0),
    ], expects=["Bid_Pri"]),

    # plot: Space opens fzf cmd menu, select histogram
    "plot": F(NYSE, [
        ("Move cursor to a numeric column",                    "lll",   "lll",  2.0),
        ("Open command menu with Space",                       None,    None,   2.0),
        ("",                                                   None,    " .....",       3.0),  # fzf char loss padding
        ("",                                                   None,    "\x15^c6", 3.0),
        ("Render a histogram with ggplot2\nPress q to close",  None,    "\r",           5.0),
        ("",                                                   None,    "q",            1.0),
    ], expects=["histogram"]),

    "fzf": F(NYSE, [
        ("Press Space to open the command menu", None,       None,         2.0),
        ("",                                     None,       " .....",     3.0),  # fzf char loss padding
        ("",                                     None,       "\x15^c[",  3.5),
        ("",                                     None,       "\r",        3.5),
    ], expects=["Sort ascending"]),

    "meta": F(NYSE, [
        ("",                                                     None,    " .....",        3.0),
        ("",                                                     None,    "\x15^M+",  3.0),
        ("Column metadata: names, types, nulls, unique counts",  None,    "\r",            3.5),
        ("",                                                     None,    " .....",        3.0),
        ("",                                                     None,    "\x15^M0",      3.0),
        ("",                                                     None,    "\r",            2.0),
        ("",                                                     None,    " .....",        3.0),
        ("",                                                     None,    "\x15^M1",    3.0),
        ("",                                                     None,    "\r",            2.0),
        ("Enter hides the selected columns from the table",      "Enter", "\r",            3.5),
    ], expects=["meta", "coltype", "null"]),

    "sort": F(NYSE, [
        ("Press [ to sort ascending\nPress ] to sort descending", "[", "l[", 3.0),
        ("",                                                      None, "l]", 3.0),
        ("Press ! to pin a column as key (left)\nPress ! again to unpin", "!", "l!", 3.0),
        ("",                                                      None, "!c~",  3.0),
    ], expects=["sort"]),

    # split: send :- via socket (bypasses fzf, works in pty recording)
    "split": F("data/split_test.csv", [
        ("A table with a column containing a-b values",           None, None,                        3.0),
        ("Press : to split, type - and Enter",                     ": - Enter", "!:-",                3.0),
        ("New columns appear from the split parts",               None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      5.0),
    ], expects=["split_test.csv"]),

    # filter: press \ to open filter prompt, type expression
    "filter": F(NYSE, [
        ("Move to the Exchange column",                             "l",   "l",                         2.0),
        ("Press \\\\ to filter, type Exchange ~= 'P'",               None,  None,                        3.0),
        ("",                                                        None,  None,                        0.5),
        ("",                                                        None,  "\\........",                 3.0),
        ("",                                                        None,  "\x15Exchange ~= 'P'",        3.0),
        ("",                                                        None,  "\r",                        1.0),
        ("Only rows where Exchange contains P remain",              None,  None,                        5.0),
    ], expects=["filter Exchange"]),

    # derive: send =expr via socket (bypasses fzf)
    "derive": F("data/numeric.csv", [
        ("A simple table with columns x, y, z",               None, None,                     3.0),
        ("Press = to derive: double = x * 2",                 "= double = x * 2 Enter", "!=double = x * 2", 3.0),
        ("The new 'double' column appears",                    None, "!c>",                   0.5),
        ("",                                                   None, "!c>",                   0.5),
        ("",                                                   None, "!c>",                   5.0),
    ], expects=["double"]),

    # diff_compare: static side-by-side showing first, second, and diff result
    # folder sorts asc: row0=.., 1=after, 2=before, 3=first, 4=second
    "diff": F("data/diff_test/", [
        ("Table 1: first.csv",                                 "jjjj Enter", "[jjjj\r", 5.0),
        ("",                                                   None,         "!s~",     0.5),  # swap back to folder
        ("Table 2: second.csv\nbob's sales changed, bonus→rating swapped", "j Enter", "j\r", 5.0),
        ("",                                                   None,         "!s~",     0.3),  # swap folder to top
        ("",                                                   None,         "q",       0.3),  # pop folder
        ("",                                                           None, " .....",     3.0),
        ("",                                                           None, "\x15^s2",   3.0),
        ("Diff compares the two tables\nChanged columns get a Δ prefix", None, "\r",       5.0),
    ], expects=["diff", "first.csv", "second.csv"]),

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
    ], expects=["hf://"]),
}

# -- Title overlay -------------------------------------------------------------

BOX_H = 4  # fixed: blank + line1 + line2 + blank
BOX_ROW = H // 2 - BOX_H // 2  # fixed vertical position

def title_escape(desc, keys):
    """Fixed-size title box: 2 content lines + top/bottom padding.
    Line 1 = description, line 2 = keys (or second line of desc if multiline)."""
    if not desc:
        return "", 0
    # Split desc into lines, append keys to first line if short enough
    parts = desc.split(chr(10))
    line1 = f"{parts[0]}  ({keys})" if keys and len(parts[0]) + len(keys) + 4 <= BOX_W else parts[0]
    line2 = parts[1] if len(parts) > 1 else (f"({keys})" if keys and line1 == parts[0] else "")
    # Truncate if still too wide
    if len(line1) > BOX_W: line1 = line1[:BOX_W]
    if len(line2) > BOX_W: line2 = line2[:BOX_W]
    # Center each line within box
    def pad(s):
        p = max(BOX_W - len(s), 0)
        return " " * (p // 2) + s + " " * (p - p // 2)
    blank = " " * BOX_W
    margin = (W - BOX_W) // 2
    indent = f"\x1b[{margin + 1}G"
    esc, rst = "\x1b[1;97;44m", "\x1b[0m"
    row = BOX_ROW
    out = f"\x1b7"
    for i, ln in enumerate([blank, pad(line1), pad(line2), blank]):
        out += f"\x1b[{row+i};1H{indent}{esc}{ln}{rst}"
    out += "\x1b8"
    return out, 2

# -- Recording engine ----------------------------------------------------------

def record(cli_args, steps, cast_path):
    os.makedirs(os.path.dirname(cast_path), exist_ok=True)
    env = {**os.environ,
           "LD_LIBRARY_PATH": "/usr/local/lib:" + os.environ.get("LD_LIBRARY_PATH", ""),
           "TERM": "xterm-256color",
           "TMPDIR": "/tmp"}  # real /tmp for socket; gen_demo.py is in excludedCommands
    env.pop("TMUX", None)  # fzf --tmux popup not captured by pty recording; force inline

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
                # Drop alternate-screen-exit frames — they cause a black frame in GIFs
                if "\x1b[?1049l" not in text:
                    cast_f.write(json.dumps([round(t, 3), "o", text]) + "\n")
                sys.stdout.buffer.write(buf)
                sys.stdout.buffer.flush()

        # Send command via tv's Unix socket (bypasses fzf)
        sock_path = f"{env.get('TMPDIR', '/tmp')}/tv-{pid}.sock"
        def sock_send(cmd):
            """Send a command string to tv's socket. Retries briefly if socket not ready."""
            for _ in range(20):
                try:
                    s = sock_mod.socket(sock_mod.AF_UNIX, sock_mod.SOCK_STREAM)
                    s.connect(sock_path)
                    s.sendall(cmd.encode())
                    s.close()
                    return
                except (ConnectionRefusedError, FileNotFoundError):
                    time.sleep(0.1)

        # Warmup: wait for tv to call tb_init() and render first frame.
        # tb_init uses TCSAFLUSH which discards pending pty input.
        # drain catches the first render, then sleep ensures tb_poll_event is ready.
        drain(2.0)
        time.sleep(1.0)

        last_title_lines = 0
        try:
            for desc, keys_shown, keys, pause in steps:
                if child_dead:
                    break
                # Clear previous title overlay before each step
                if last_title_lines > 0:
                    margin = (W - BOX_W) // 2
                    indent = f"\x1b[{margin + 1}G"
                    clr = "\x1b7"
                    for i in range(BOX_H):
                        clr += f"\x1b[{BOX_ROW+i};1H{indent}\x1b[{BOX_W}X"
                    clr += "\x1b8"
                    emit(clr)
                    last_title_lines = 0
                if keys is not None:
                    if keys.startswith("!"):
                        # Socket command: bypass fzf, send directly to tv socket
                        sock_send(keys[1:])
                    else:
                        # Keystroke injection via pty
                        for ch in keys:
                            os.write(fd, ch.encode())
                            time.sleep(0.08)
                drain(0.5)
                title, nlines = title_escape(desc, keys_shown)
                if title:
                    emit(title)
                    last_title_lines = nlines
                # Marker: desired pause for this step (post-process rewrites timestamps)
                emit(f"\x1b]999;pause={pause}\x07")
                time.sleep(min(pause, 0.3))
                drain(0.1)
        except OSError:
            pass

        # Linger: drain periodically so late renders (e.g. split result) are captured
        for _ in range(10):
            drain(1.0)

        # Close cast file BEFORE killing child — SIGTERM triggers tb_shutdown
        # which exits alternate screen buffer, writing a black frame.

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

    # Post-process: rewrite timestamps using pause markers.
    # Markers are \x1b]999;pause=N\x07 embedded in frames.
    # All frames between markers get compressed to 0.1s gaps,
    # then the marker adds N seconds of hold time.
    import re as _re
    with open(cast_path) as f:
        raw = f.readlines()
    header, frames = raw[0], [json.loads(l) for l in raw[1:]]
    t = 0.0
    new_frames = []
    for frame in frames:
        text = frame[2]
        m = _re.search(r'\x1b\]999;pause=([\d.]+)\x07', text)
        if m:
            pause_dur = float(m.group(1))
            clean = text.replace(m.group(0), "")
            if clean:
                new_frames.append([round(t, 3), "o", clean])
            t += pause_dur
        else:
            new_frames.append([round(t, 3), "o", text])
            t += 0.1  # compress real gaps to 0.1s between frames
    with open(cast_path, "w") as f:
        f.write(header)
        for frame in new_frames:
            f.write(json.dumps(frame) + "\n")

    print(f"  {cast_path} ({elapsed:.1f}s)")
    return True

def verify_cast(cast_path, expects=None):
    """Check cast for known issues and expected content. Returns True if OK."""
    with open(cast_path) as f:
        lines = f.readlines()
    all_text = ""
    table_seen = False
    for i, line in enumerate(lines[1:], 1):
        text = json.loads(line)[2]
        all_text += text
        if any(k in text for k in ("Time", "name", "column")):
            table_seen = True
        # Clear screen after table is a bug — unless inside alt-screen-enter (tb_init) or fzf start
        if table_seen and "\x1b[2J" in text and "\x1b[?1049h" not in text and "\x1b[?2004h" not in text:
            print(f"  WARN: {cast_path} frame {i} clears screen after table")
            return False
    # Check expected content appears in cast
    if expects:
        for exp in expects:
            if exp not in all_text:
                print(f"  WARN: {cast_path} missing expected content: {exp!r}")
                return False
    return True

def gen(name):
    cli_args, steps, expects = FEATURES[name]
    cast = f"doc/{name}.cast"
    gif = f"doc/{name}.gif"
    if not record(cli_args, steps, cast):
        if os.path.exists(cast):
            os.remove(cast)
        return False
    if not verify_cast(cast, expects):
        print(f"  FAIL: {cast} failed verification")
        return False
    subprocess.run([AGG, cast, gif, "--font-size", str(FONT)], check=True)
    sz = os.path.getsize(gif)
    print(f"  {gif} ({sz // 1024}K)")
    return True

if __name__ == "__main__":
    names = sys.argv[1:] or list(FEATURES.keys())
    for name in names:
        if name not in FEATURES:
            print(f"Unknown feature: {name}. Available: {', '.join(FEATURES)}")
            sys.exit(1)
    if len(names) == 1:
        if not gen(names[0]):
            sys.exit(1)
    else:
        from concurrent.futures import ProcessPoolExecutor, as_completed
        with ProcessPoolExecutor() as pool:
            futures = {pool.submit(gen, n): n for n in names}
            failed = [futures[f] for f in as_completed(futures) if not f.result()]
        if failed:
            print(f"\nFAILED: {', '.join(failed)}")
            sys.exit(1)
