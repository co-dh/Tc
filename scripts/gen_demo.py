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
_HIDE_INFO = ("", None, "!i~", 0.3)  # turn off info overlay (socket: info toggle)

def F(cli_args, steps):
    """Feature with info overlay disabled."""
    return (cli_args, [_HIDE_INFO] + steps)

# -- Feature definitions: (cli_args, steps) ------------------------------------
# Steps: (description, keys_shown, keys_to_send, pause_seconds)
# IMPORTANT: Keys that open fzf (\ = : space / s) need a separate step
# before typing into the fzf prompt — fzf needs startup time.

FEATURES = {
    # folder: sort asc first (worktrees lack dirs-first ordering), then navigate
    # sorted asc: row0=.., row1=basic.csv, row2=diff_test(dir), row3=filtered_test.parquet
    "folder": F("data/", [
        ("Browse a folder of data files",                     "tv data/",   "[",     3.0),  # sort asc for stable order
        ("Enter a subfolder",                                 "jj Enter",   "jj\r",  3.0),  # row2=diff_test
        ("Backspace goes to parent folder",                   "Backspace",  "\x7f",  3.0),
        ("Press / to search for a file",                      None,         None,    2.0),
        ("",                                                  None,         "/.....",  3.0),  # fzf char loss padding
        ("",                                                  None,         "\x15nyse",  3.0),  # ctrl-u + type (fzf visible with input)
        ("",                                                  None,         "\r",      2.0),  # enter; linger on result
        ("Cursor jumps to the matched file\nPress Enter to open", None,    None,    3.0),
        ("",                                                  None,         "\r",    3.0),
        ("",                                                  None,         "q",     1.0),
        ("Open a CSV file",                                   "j Enter",    "j\r",   3.0),
    ]),

    "sparkline": F(NYSE, [
        ("Each column header has a sparkline\nshowing the value distribution", None, None, 5.0),
    ]),

    "freq": F(NYSE, [
        ("Move cursor to Exchange column",                 "l",       "l",   2.0),
        ("Open frequency count",                           "F+",      "!F+", 3.5),
        ("Select a value and press Enter\nOnly matching rows remain", "j Enter", "j\r", 4.0),
    ]),

    # heatmap: Space opens fzf cmd menu, select heatmap modes
    "heatmap": F(NYSE, [
        ("",                                              None,  " .....",          3.0),  # Space opens fzf + padding
        ("",                                              None,  "\x15Heatmap: n",  3.0),  # ctrl-u + type (fzf visible)
        ("Color numeric columns by value",                None,  "\r",              4.0),
        ("",                                              None,  " .....",          3.0),
        ("",                                              None,  "\x15Heatmap: c",  3.0),
        ("Color categorical columns by group",            None,  "\r",              4.0),
    ]),

    # plot: Space opens fzf cmd menu, select histogram
    "plot": F(NYSE, [
        ("Move cursor to a numeric column",                    "lll",   "lll",  2.0),
        ("Open command menu with Space",                       None,    None,   2.0),
        ("",                                                   None,    " .....",       3.0),  # fzf char loss padding
        ("",                                                   None,    "\x15histogram", 3.0),  # type command (fzf visible)
        ("Render a histogram with ggplot2\nPress q to close",  None,    "\r",           5.0),
        ("",                                                   None,    "q",            1.0),
    ]),

    "fzf": F(NYSE, [
        ("Press Space to open the command menu", None,       None,         2.0),
        ("",                                     None,       " .....",     3.0),  # fzf char loss padding
        ("Type to search, Enter to run",         None,       "\x15sort",  3.5),  # type (fzf visible with input)
        ("",                                     None,       "\r",        3.5),
    ]),

    "meta": F(NYSE, [
        ("Column metadata: names, types, nulls, unique counts",  "M+",    "!M+",  3.5),
        ("Select all-null columns",                              "M0",    "!M0",  2.0),
        ("Select single-value columns",                          "M1",    "!M1",  2.0),
        ("Enter hides the selected columns from the table",      "Enter", "\r",   3.5),
    ]),

    "sort": F(NYSE, [
        ("Press [ to sort ascending\nPress ] to sort descending", "[", "l[", 3.0),
        ("",                                                      None, "l]", 3.0),
        ("Press ! to pin a column as key (left)\nPress ! again to unpin", "!", "l!", 3.0),
        ("",                                                      None, "!c~",  3.0),
    ]),

    # split: send :- via socket (bypasses fzf, works in pty recording)
    "split": F("data/split_test.csv", [
        ("A table with a column containing a-b values",           None, None,                        3.0),
        ("Press : to split, type - and Enter",                     ": - Enter", "!:-",                3.0),
        ("New columns appear from the split parts",               None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      0.5),
        ("",                                                      None, "!c>",                      5.0),
    ]),

    # filter: send \expr via socket (bypasses fzf)
    "filter": F(NYSE, [
        ("Move to the Exchange column",                             "l",   "l",                         2.0),
        ("Filter rows where Exchange contains 'P'",                 "\\Exchange ~= 'P'",  "!\\Exchange ~= 'P'",  3.0),
        ("Only matching rows remain",                               None,  None,                        5.0),
    ]),

    # derive: send =expr via socket (bypasses fzf)
    "derive": F("data/numeric.csv", [
        ("A simple table with columns x, y, z",               None, None,                     3.0),
        ("Press = to derive: double = x * 2",                 "= double = x * 2 Enter", "!=double = x * 2", 3.0),
        ("The new 'double' column appears",                    None, "!c>",                   0.5),
        ("",                                                   None, "!c>",                   0.5),
        ("",                                                   None, "!c>",                   5.0),
    ]),

    # diff_compare: static side-by-side showing first, second, and diff result
    # folder sorts asc: row0=.., 1=after, 2=before, 3=first, 4=second
    "diff": F("data/diff_test/", [
        ("Table 1: first.csv",                                 "jjjj Enter", "[jjjj\r", 5.0),
        ("",                                                   None,         "!s~",     0.5),  # swap back to folder
        ("Table 2: second.csv\nbob's sales changed, bonus→rating swapped", "j Enter", "j\r", 5.0),
        ("",                                                   None,         "!s~",     0.3),  # swap folder to top
        ("",                                                   None,         "q",       0.3),  # pop folder
        ("Diff compares the two tables\nChanged columns get a Δ prefix", "s2", "!s2", 5.0),
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
                # Alt-screen-exit frames: replace with clear screen (agg doesn't track alt buffers)
                if "\x1b[?1049l" in text:
                    text = "\x1b[2J\x1b[H"  # clear screen + home cursor
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
                # Clear previous title overlay before drawing a new one
                if last_title_lines > 0 and desc:
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
                time.sleep(pause)
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
