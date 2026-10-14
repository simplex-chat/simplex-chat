import re, glob, sys, os, subprocess, collections
impl_root = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../../../apps/multiplatform')
src = subprocess.run(['grep', '-rhoE', r'@Test fun [A-Za-z0-9_]+', impl_root + '/common/src/uiTestShared', impl_root + '/android/src/androidTest', impl_root + '/common/src/desktopTest/kotlin/chat/simplex/app/ui'], capture_output=True, text=True).stdout
implemented = set(m.split()[-1] for m in src.split('\n') if m)
rows = []
for f in sorted(glob.glob(os.path.join(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'), '0[1-5]-*.md'))):
    if 'carryover' in f: continue
    L = open(f).read().split('\n'); col = None; need = None; sec = ''
    for i, l in enumerate(L, 1):
        if l.startswith('## Count'): break
        if l.startswith('### '): sec = l[4:].strip(); col = None; continue
        if not l.startswith('|'): continue
        c = [x.strip() for x in l.strip().strip('|').split('|')]
        if 'Test case(s)' in c:
            col = c.index('Test case(s)'); need = next(j for j, x in enumerate(c) if x.startswith('Needs')); continue
        if col is None or col >= len(c) or set(l.replace('|', '').strip()) <= set('-: '): continue
        nd = re.sub(r'\([^)]*\)', '', c[need]) if need < len(c) else ''
        codes = 'IGNORE' if nd.startswith('IGNORE') else ' '.join(sorted(set(re.findall(r'[A-Z][A-Z0-9]*|—', nd))))
        for t in re.findall(r'`([A-Za-z0-9_]+)`', c[col]):
            rows.append((os.path.basename(f)[:2], i, sec, t, codes, t in implemented))
if len(sys.argv) > 1 and sys.argv[1] == 'summary':
    c = collections.Counter((r[4], r[5]) for r in rows)
    print('total', len(rows), 'implemented', sum(r[5] for r in rows))
    for k in sorted(set(r[4] for r in rows)): print(f"{k:30s} done {c[(k, True)]:4d} todo {c[(k, False)]:4d}")
else:
    for r in rows: print('\t'.join(map(str, r)))
