/ NBBO loader - upsert to disk

\cd /home/dh/repo/Tc

T:"JSSFFJFFJSSSSSSSFFJSFFJSSSSJJS"

d:`:data/kdb/2025.07.01/nbbo/
total:0
hdr:1b
C:()

-1 "Loading...";

cb:{
  t:$[hdr; [(T;enlist "|") 0: x]; [flip C!(T;"|") 0: x]];
  t:.Q.id t;
  if[hdr; C::cols t; hdr::0b];
  t:.Q.en[`:data/kdb] t;
  d upsert t;
  total+::count t;
  -1 string total;
  }

.Q.fs[cb] `$":data/EQY_US_ALL_NBBO_20250701"

-1 "Done: ",string total;
\\
