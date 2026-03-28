import json, sys
path = sys.argv[1] if len(sys.argv) > 1 else "doc/fzf.cast"
with open(path) as f:
    lines = f.readlines()
table_seen = False
for i, line in enumerate(lines[1:], 1):
    text = json.loads(line)[2]
    if "Time" in text or "Exc" in text or "nyse" in text:
        table_seen = True
    # Clear screen is OK if it's inside alt-screen-enter (tb_init) or fzf start
    if table_seen and "\x1b[2J" in text and "\x1b[?1049h" not in text and "\x1b[?2004h" not in text:
        print("%s: FAIL frame %d clears screen after table rendered" % (path, i))
        sys.exit(1)
print("%s: OK" % path)
sys.exit(0)
