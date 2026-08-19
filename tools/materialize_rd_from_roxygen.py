#!/usr/bin/env python3
"""Create a conservative Rd snapshot from the package's roxygen source.
The authoritative release workflow remains roxygen2::roxygenise().
"""
from pathlib import Path
import re
ROOT=Path(__file__).resolve().parents[1]; RDIR=ROOT/'R'; MANDIR=ROOT/'man'; MANDIR.mkdir(exist_ok=True)
ns=(ROOT/'NAMESPACE').read_text()
public=set(re.findall(r'export\(([^)]+)\)',ns))
methods=set('print.'+x for x in re.findall(r'S3method\(print,([^)]+)\)',ns))
targets=public|methods

def esc(x):
    x=x.replace('\\','\\backslash{}').replace('%','\\%').replace('{','\\{').replace('}','\\}')
    return x

def parse_roxy(lines, idx):
    j=idx-1; block=[]
    while j>=0 and lines[j].lstrip().startswith("#'"):
        block.append(lines[j]); j-=1
    block=block[::-1]
    content=[re.sub(r"^\s*#'\s?",'',x) for x in block]
    title=''; desc=[]; params=[]; ret=''; mode='desc'; cur=None
    for x in content:
        if not title and x and not x.startswith('@'):
            title=x; continue
        if x.startswith('@param '):
            mode='param'; rest=x[7:]; parts=rest.split(None,1); cur=[parts[0],parts[1] if len(parts)>1 else '']; params.append(cur); continue
        if x.startswith('@return '): mode='return'; ret=x[8:]; cur=None; continue
        if x.startswith('@') or x.startswith('\\'): mode='other';cur=None;continue
        if mode=='param' and cur is not None and x: cur[1]+=' '+x
        elif mode=='return' and x: ret+=' '+x
        elif mode=='desc' and x: desc.append(x)
    return title or 'plsSEMflow function', ' '.join(desc).strip(), params, ret

def signature(lines, idx, name):
    text=' '.join(lines[idx:idx+30])
    marker=re.escape(name)+r'\s*<-\s*function\s*\('
    m=re.search(marker,text)
    if not m:return name+'(...)'
    start=m.end()-1; depth=0; q=None;escp=False
    for k in range(start,len(text)):
        ch=text[k]
        if q:
            if escp:escp=False
            elif ch=='\\':escp=True
            elif ch==q:q=None
            continue
        if ch in ('"',"'"):q=ch;continue
        if ch=='(':depth+=1
        elif ch==')':
            depth-=1
            if depth==0:
                args=text[start+1:k]
                args=re.sub(r'\s+',' ',args).strip()
                return f'{name}({args})'
    return name+'(...)'

made=0
for f in sorted(RDIR.glob('*.R')):
    lines=f.read_text().splitlines()
    for i,l in enumerate(lines):
        m=re.match(r'\s*([A-Za-z.][A-Za-z0-9._]*)\s*<-\s*function\b',l)
        if not m or m.group(1) not in targets: continue
        name=m.group(1); title,desc,params,ret=parse_roxy(lines,i); use=signature(lines,i,name)
        fn=name.replace('.','-')+'.Rd'
        aliases=[name]
        rd=['% Conservative source snapshot; regenerate with roxygen2 before release.',f'\\name{{{name}}}']
        for a in aliases:rd.append(f'\\alias{{{a}}}')
        rd += [f'\\title{{{esc(title)}}}', '\\usage{', esc(use), '}']
        if params:
            rd.append('\\arguments{')
            for n,d in params:rd.append(f'  \\item{{{esc(n)}}}{{{esc(d or "See package vignette and source documentation.")}}}')
            rd.append('}')
        rd.append(f'\\value{{{esc(ret or "See the function documentation and integrated tutorial.")}}}')
        rd.append(f'\\description{{{esc(desc or title)}}}')
        rd.append('\\details{For worked agronomic examples, see the quick-start, API example catalog, and foundations-to-advanced vignettes. The roxygen source in R/ is authoritative and should regenerate this help topic before release.}')
        (MANDIR/fn).write_text('\n'.join(rd)+'\n')
        made+=1
print(f'Materialized {made} conservative Rd topics in {MANDIR}')
