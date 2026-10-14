import re,sys
p=sys.argv[1]; L=open(p).read().split('\n')
secs=[];col=None;needcol=None;inCount=False
for i,ln in enumerate(L):
    if ln.startswith('## Count'): inCount=True
    if inCount: continue
    if ln.startswith('### '):
        col=None
        if re.search(r'\(\d+ tests?',ln): secs.append([i,0,0])
        continue
    if not ln.startswith('|'): 
        if not ln.strip(): col=None
        continue
    c=[x.strip() for x in ln.strip().strip('|').split('|')]
    if 'Test case(s)' in c: col=c.index('Test case(s)'); needcol=next((k for k,x in enumerate(c) if x.lower().startswith('needs')),None); continue
    if col is None or set(ln.replace('|','').strip())<=set('-: ') or col>=len(c): continue
    n=len(re.findall(r'`[^`]+`',c[col])); secs[-1][1]+=n
    if needcol is not None and 'IGNORE' in c[needcol]: secs[-1][2]+=n
for i,t,g in secs:
    tail='%d tests'%t + ('' if g==0 else (', all ignored' if g==t else ', %d ignored'%g))
    L[i]=re.sub(r'\(\d+ tests?[^)]*\)\s*$','(%s)'%tail,L[i]); assert tail in L[i],L[i]
ci=L.index('## Count'); rows=[k for k in range(ci+1,len(L)) if L[k].startswith('|') and '**Total**' not in L[k] and not L[k].startswith('| Section') and not set(L[k].replace('|','').strip())<=set('-: ')]
assert len(rows)==len(secs),(len(rows),len(secs))
for k,(i,t,g) in zip(rows,secs):
    c=L[k].strip().strip('|').split('|'); L[k]='| %s | %d | %d |'%(c[0].strip(),t,g)
T=sum(s[1] for s in secs); G=sum(s[2] for s in secs)
tk=[k for k in range(ci,len(L)) if '**Total**' in L[k]][0]; L[tk]='| **Total** | **%d** | **%d** |'%(T,G)
open(p,'w').write('\n'.join(L)); print(p.split('/')[-1],'tests',T,'ignored',G,'sections',len(secs))
