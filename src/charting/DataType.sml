structure DataType = struct

type measurement = {
  rss   : int,
  size  : int,
  data  : int,
  stk   : int,
  exe   : int,
  gcn   : int,                 (* total gc count *)
  majgcn: int,                 (* major gc count *)
  gc    : real,                (* total gc time *)
  majgc : real,                (* major gc time *)
  sys   : real,
  user  : real,
  real  : real
}

type line = {
  cname    : string,           (* compiler name *)
  cversion : string,           (* compiler version *)
  date     : Date.date,        (* date of comp/run *)
  mach     : string,           (* machine identifier (cpu, os, arch) *)
  pname    : string,           (* program name *)
  plen     : int,              (* program length (lines) *)
  ctime    : real,             (* compile time *)
  binsz    : int,              (* size of binary executable (in kb) *)
  runs     : measurement list, (* the runs *)
  err      : string            (* err string ("": no errors) *)
}

end
