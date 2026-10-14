import re,sys,glob,collections,os
files=sorted(glob.glob(os.path.join(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'), '0[1-4]-*.md')))
allnames=collections.defaultdict(list); summary={}
for f in files:
    lines=open(f).read().split('\n'); col=None; needcol=None; names=[]; ign=0; codes=collections.Counter(); sec=None; per=collections.Counter(); perign=collections.Counter(); badcells=0
    counttab=None; inCount=False
    for ln in lines:
        if ln.startswith('## Count'): inCount=True; continue
        if ln.startswith('## ') or ln.startswith('### '):
            if not inCount: sec=ln.strip('# ').strip()
        if not ln.startswith('|'): col=None if not ln.strip() else col; continue
        cells=[c.strip() for c in ln.strip().strip('|').split('|')]
        if inCount:
            m=re.match(r'\*\*Total\*\*',cells[0]) if cells else None
            if m: counttab=(cells[1].strip('*'),cells[2].strip('*') if len(cells)>2 else None)
            continue
        if 'Test case(s)' in cells: col=cells.index('Test case(s)'); needcol=next((i for i,c in enumerate(cells) if c.lower().startswith('needs')),None); continue
        if col is None or set(ln.replace('|','').strip())<=set('-: '): continue
        if col>=len(cells): continue
        cell=cells[col]
        toks=re.findall(r'`([^`]+)`',cell)
        rest=re.sub(r'`[^`]+`','',cell).replace(',','').strip()
        if rest: badcells+=1
        isign = needcol is not None and needcol<len(cells) and 'IGNORE' in cells[needcol]
        if needcol is not None and needcol<len(cells):
            for c in re.findall(r'\b(RELAY|NAMES|XFTP2?|SMP2|FIXTURE|RESTART|MCAST|SLOW|ONBOARD|SOCKS|HTTP|REC|AV|S)\b',cells[needcol]): codes[c]+=len(toks)
        for t in toks:
            if not re.match(r'^[A-Za-z_][A-Za-z0-9_]*$',t): print('BAD NAME',f,t)
            names.append(t); allnames[t].append(f.split('/')[-1]); per[sec]+=1
            if isign: ign+=1; perign[sec]+=1
    summary[f.split('/')[-1]]=dict(tests=len(names),ign=ign,counttable=counttab,badcells=badcells,secs=len(per),codes=dict(codes))
for k,v in summary.items(): print(k,v)
d=[(n,fs) for n,fs in allnames.items() if len(fs)>1]
print('duplicates:',d[:20],len(d))
print('TOTAL',sum(v['tests'] for v in summary.values()),'IGN',sum(v['ign'] for v in summary.values()),'SECS',sum(v['secs'] for v in summary.values()))
