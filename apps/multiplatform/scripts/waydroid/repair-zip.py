#!/usr/bin/env python3
# Compare the assembled file with the mirror in 1 MiB blocks around given centres (+-radius MiB)
# and patch any differing block in place. Usage: file urlfile centre1,centre2,... radiusMiB
import sys, urllib.request
f, url = sys.argv[1], open(sys.argv[2]).read().strip()
centres = [int(x) for x in sys.argv[3].split(',')]; radius = int(sys.argv[4])
B = 1 << 20
with open(f, 'r+b') as fh:
    fh.seek(0, 2); size = fh.tell()
    for c in centres:
        for k in range(-radius, radius):
            off = (c // B + k) * B
            if off < 0 or off >= size: continue
            end = min(off + B, size) - 1
            remote = urllib.request.urlopen(urllib.request.Request(url, headers={'Range': f'bytes={off}-{end}'}), timeout=120).read()
            fh.seek(off); local = fh.read(len(remote))
            if local != remote:
                fh.seek(off); fh.write(remote); print('patched block at', off, flush=True)
print('done')
