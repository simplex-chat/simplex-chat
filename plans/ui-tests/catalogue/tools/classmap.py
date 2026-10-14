import re,sys,os
D=os.path.join(os.path.dirname(os.path.abspath(__file__)), '..') + '/'
FILES={'01':'01-onboarding-chatlist-connect-profiles.md','02':'02-chat-view-messages.md','03':'03-groups-channels-business.md','04':'04-settings-database-remote.md'}
PKG={'01':'main','02':'chat','03':'groups','04':'settings'}
# section id -> list of (ClassBase, first-row selector or None). Selector = Area value (01/02/04) or first test name.
MAP={
'01':{'1':[('Onboarding',None)],'2':[('WhatsNew',None)],
      '3':[('ChatListToolbarSearch',None),('ChatListFiltersLists','area:Filters'),('ChatListRows','area:Rows'),('ChatListMenusPending','area:Contact menu'),('ChatListRequestsCards','area:Requests')],
      '4':[('ServersSummary',None)],
      '5':[('UserPicker',None),('ChatProfiles','area:Profiles'),('ProfileEdit','area:Profile edit')],
      '6':[('NewChatSheet',None)],'7':[('OneTimeLink',None)],'8':[('ConnectPlan',None)],'9':[('PreparedChat',None)],
      '10':[('PendingConnection',None)],'11':[('Address',None)],'12':[('SimplexName',None)],'13':[('ContactInfo',None)],
      '14':[('ContactPreferences',None)],'15':[('DeleteContact',None)]},
'02':{'A':[('ChatShell',None)],'B':[('Compose',None)],'C':[('LiveMessage',None)],'D':[('TimedMessage',None)],'E':[('VoiceMessage',None)],
      'F':[('SendAttachment',None)],'G':[('ReceiveFile',None)],'H':[('Reply',None)],'I':[('EditMessage',None)],'J':[('DeleteMessage',None)],
      'K':[('MultiSelect',None)],'L':[('Forward',None)],'M':[('Reaction',None)],'N':[('CopyShare',None)],'O':[('MemberReport',None)],
      'P':[('ItemInfo',None)],'Q':[('UnreadScroll',None)],'R':[('ChatSearch',None)],'S':[('Draft',None)],'T':[('ItemStatus',None)],
      'U':[('SpecialItem',None)],'V':[('Notes',None)],'W':[('ContextMenu',None)],'X':[('ChatEdgeCases',None)]},
'03':{'1':[('CreateGroup',None)],'2':[('InviteMembers',None)],'3':[('GroupInfo',None)],'4':[('GroupProfile',None)],'5':[('GroupPreferences',None)],
      '6':[('MemberAdmission',None)],'7':[('GroupLink',None)],'8':[('Joining',None)],'9':[('MemberList',None)],'10':[('MemberInfo',None)],
      '11':[('MemberContact',None)],'12':[('SupportChat',None)],'13':[('ModeratorReport',None)],'14':[('Moderation',None)],
      '15':[('MentionNotification',None)],'16':[('GroupEvents',None)],'17':[('LeaveDeleteClear',None)],
      '18':[('ChannelOwnerCreate',None),('ChannelOwnerBroadcast','test:channel_broadcastPlaceholderAndDelivery'),('ChannelOwnerManage','test:channelSubscribers_listAndCount')],
      '19':[('ChannelSubscriber',None)],'20':[('BusinessChat',None)]},
'04':{'A':[('SettingsRoot',None)],'B':[('Privacy',None)],'C':[('Lock',None)],'D':[('Appearance',None)],
      'E':[('NetworkServers',None),('NetworkProxy','area:SOCKS')],'F':[('CallSettings',None)],
      'G':[('ChatData',None),('DatabaseEncryption','area:Encryption')],'H':[('Migration',None)],'I':[('VersionDevTools',None)],
      'J':[('Help',None)],'K':[('RemoteDesktop',None)],'L':[('SupporterPerks',None)],'M':[('SettingsOutOfReach',None)]}}
rows=[];problems=[]
for cat,f in FILES.items():
    L=open(D+f).read().split('\n'); sec=None; col=None; need=None; cls=None; title=None; secs_seen=[]
    data={}  # sec -> list of (area, names, isS, isIgn)
    for l in L:
        if l.startswith('## Count'): break
        m=re.match(r'### (\w+)\. (.*?) \(\d+ tests?',l)
        if l.startswith('### '):
            col=None; sec=m.group(1) if m else None
            if m: data[sec]=[]; secs_seen.append((sec,m.group(2)))
            continue
        if sec is None or not l.startswith('|'): continue
        c=[x.strip() for x in l.strip().strip('|').split('|')]
        if 'Test case(s)' in c: col=c.index('Test case(s)'); need=next(i for i,x in enumerate(c) if x.startswith('Needs')); hasArea=c[0]=='Area'; continue
        if col is None or col>=len(c) or set(l.replace('|','').strip())<=set('-: '): continue
        names=re.findall(r'`([^`]+)`',c[col])
        if names: data[sec].append((c[0] if hasArea else None,names,bool(re.search(r'(^|[ ,])S([ ,×(]|$)',c[need])),'IGNORE' in c[need]))
    if set(data)!=set(MAP[cat]): problems.append((cat,'section mismatch',sorted(set(data)^set(MAP[cat]))))
    for sec,title in secs_seen:
        spec=MAP[cat].get(sec,[]); idx=0; per={}
        for area,names,isS,isI in data[sec]:
            for n in names:
                if idx+1<len(spec):
                    k,v=spec[idx+1][1].split(':',1)
                    if (k=='area' and area==v) or (k=='test' and n==v): idx+=1
                b=spec[idx][0]; d=per.setdefault(b,dict(first=None,t=0,i=0,s=0)); 
                if d['first'] is None: d['first']=n
                d['t']+=1; d['i']+=isI; d['s']+=isS
        for b,_ in spec:
            if b not in per: problems.append((cat,sec,b,'empty')); continue
            d=per[b]; rows.append((cat,sec,title,b,PKG[cat],d))
if problems: print('PROBLEMS',problems)
mode=sys.argv[1] if len(sys.argv)>1 else 'table'
if mode=='table':
    for cat,sec,title,b,pkg,d in rows:
        classes='`%sTest`'%b if d['s']<d['t'] else ''
        if d['s']: classes+=(', ' if classes else '')+'`%sCompatTest`, `%sCompatStableTest`'%(b,b)
        print('| %s | %s | %s | `%s` | %s | %d | %d |'%(cat,sec,title.replace('|','/'),d['first'],classes,d['t'],d['i']))
else:
    import collections
    print('classes(bases)',len(rows),'with runnable',sum(1 for r in rows if r[5]['t']>r[5]['i']),'max',max(r[5]['t'] for r in rows))
    secs=collections.defaultdict(lambda:[0,0])
    for cat,sec,title,b,pkg,d in rows: secs[(cat,sec)][0]+=d['t']; secs[(cat,sec)][1]+=d['i']
    print('sections',len(secs),'runnable sections',sum(1 for v in secs.values() if v[0]>v[1]))
    for cat,sec,title,b,pkg,d in rows:
        if d['t']>30: print('BIG',cat,sec,b,d['t'])
