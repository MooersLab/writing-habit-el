"""Differential probe of the Python weekly table model; see run.sh."""
import sys, hashlib, copy
from writing_habit.gui.weekly_table import WeeklyTable, split_due_date
from writing_habit import name as namemod
def h(t): return hashlib.sha1(t.encode()).hexdigest()[:12]
def b(x): return "T" if x else "F"
def ev(t): return ";".join(f"{e.offset},{e.start},{e.end},{e.letter},{e.section}" for e in t.events())
def secs(t): return ";".join(f"{i}:{r.section}" for i,r in enumerate(t.rows) if r.kind=="block")
out=[]
def p(*a): out.append("|".join(str(x) for x in a))
for f in sys.argv[1:]:
    text=open(f,encoding="utf-8").read()
    fresh=lambda: WeeklyTable(text, path=f)
    t=fresh()
    p(f,"rt",h(t.to_text()),b(t.to_text()==text))
    p(f,"kinds",",".join(r.kind for r in t.rows))
    p(f,"events",ev(t))
    p(f,"legend",";".join(f"{c}={d}/{r}" for c,(d,r) in t.legend().items()))
    c,prob=t.code_or_problem(); p(f,"code",c if c else "ERR")
    tot=t.totals()
    for k in ("day","project","category"): p(f,"tot",k,";".join(f"{a}={v}" for a,v in tot[k].items()))
    p(f,"ovl","\n".join(t.overlaps() and __import__('writing_schedule.overlap',fromlist=['x']).overlap_lines(t.overlaps()) or []))
    p(f,"conf",";".join(f"{r},{c}" for r,c in sorted(t.conflicting_cells())))
    p(f,"used",",".join(t.used_codes()),t.next_free_code())
    for r in t.block_rows: p(f,"clear",r,",".join(map(str,t.rows_clear_of(r))))
    for i,r in enumerate(t.rows):
        if r.kind in ("block","section") and t.columns:
            for above in (True,False):
                s,e=t.suggest_times(i,above)
                u=fresh()
                try:
                    at=u.insert_block(i,above,s,e); p(f,"ins",i,b(above),s,e,at,h(u.to_text()),secs(u))
                except ValueError as x: p(f,"ins",i,b(above),s,e,"ERR")
        if r.kind=="block":
            for up in (True,False):
                tg=t.move_target(i,up)
                if tg is None: p(f,"mv",i,b(up),"None"); continue
                u=fresh(); at=u.move_block(i,up); p(f,"mv",i,b(up),tg,at,h(u.to_text()),secs(u),u.code_or_problem()[0] or "ERR")
        if r.kind=="legend":
            for up in (True,False):
                tg=t.legend_move_target(i,up)
                if tg is None: p(f,"lmv",i,b(up),"None"); continue
                u=fresh(); at=u.move_legend(i,up); p(f,"lmv",i,b(up),tg,at,h(u.to_text()))
            for above in (True,False):
                u=fresh(); code=u.next_free_code()
                try: at=u.insert_legend(i,above,code,"New project, Oct 3","speculative"); p(f,"lins",i,b(above),code,at,h(u.to_text()))
                except ValueError: p(f,"lins",i,b(above),"ERR")
            u=fresh(); code=r.parsed[0]
            ch=u.set_legend(i,code,"Renamed thing", "safe"); p(f,"setleg",i,b(ch),h(u.to_text()))
    u=fresh(); code=u.next_free_code()
    if code:
        at=u.insert_legend(None,False,code,"",None); p(f,"lins-end",code,at,h(u.to_text()))
    if t.block_rows and t.columns:
        r0=t.block_rows[0]; col=t.columns[0][0]
        u=fresh(); ch=u.set_cell(r0,col,"q"); p(f,"set",b(ch),h(u.to_text()))
        p(f,"sync1",b(u.sync_legend()),h(u.to_text()))
        u.set_cell(r0,col,""); p(f,"sync2",b(u.sync_legend()),h(u.to_text()))
        u=fresh(); ch=u.set_cell(r0,col,"Zebra"); p(f,"setlong",b(ch),h(u.to_text()))
    rows,probs=t.legend_check(); p(f,"lcheck",";".join(f"{a},{bb},{c},{d},{e}" for a,bb,c,d,e in rows),",".join(probs))
    p(f,"dups",";".join(f"{k}={'/'.join(v)}" for k,v in t.duplicate_legend_codes().items()))
    p(f,"stray",";".join(f"{k}={v}" for k,v in t.stray_risk_tags()))
    p(f,"unk",",".join(t.unknown_sections()))
    for code in t.legend():
        i=t.project_info(code); p(f,"info",code,i["name"],i["due"],i["risk"])
print("\n".join(out))
