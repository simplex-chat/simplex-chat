import re,collections,glob,os
tot=collections.Counter(); per={}
for f in sorted(glob.glob(os.path.join(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'), '0[1-4]-*.md'))):
    L=open(f).read().split('\n'); col=None; c2=collections.Counter(); k=f.split('/')[-1][:2]
    for l in L:
        if l.startswith('## Count'): break
        if l.startswith('### '): col=None; continue
        if not l.startswith('|'): continue
        c=[x.strip() for x in l.strip().strip('|').split('|')]
        if 'Test case(s)' in c: col=c.index('Test case(s)'); need=next(i for i,x in enumerate(c) if x.startswith('Needs')); continue
        if col is None or col>=len(c) or set(l.replace('|','').strip())<=set('-: '): continue
        n=len(re.findall(r'`[^`]+`',c[col])); nd=re.sub(r'\([^)]*\)','',c[need])
        if nd.startswith('IGNORE'): c2['IGNORE']+=n; continue
        for code in set(re.findall(r'[A-Z][A-Z0-9]*|—',nd)): c2[code]+=n
    per[k]=c2; tot+=c2
legal=set('P S SMP SMP2 XFTP XFTP2 RELAY NAMES HTTP SOCKS SRVCTL PEERCTL RESTART FIXTURE ONBOARD FD CB IMGCB URI NTF TOAST DT REC AV MCAST SLOW REALTIME — IGNORE'.split())
for k in sorted(tot): print(k,[per[x][k] for x in sorted(per)],tot[k])
print('illegal',{k:v for k,v in tot.items() if k not in legal})
