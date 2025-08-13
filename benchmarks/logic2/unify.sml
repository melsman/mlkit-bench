(* unify.sml *)

structure Unify =
struct
  local
    open Term Trail
    fun same_ref (r, REF r') = (r = r')
      | same_ref _ = false

    fun occurs_check r t =
	let
	    fun oc (STR(_,ts)) = ocs ts
	      | oc (REF r') =
		(case !r' of
                     NON => r <> r'
		   | s => oc s)
	      | oc (CON _) = true
	      | oc (INT _) = true
              | oc NON = raise Fail "oc: impossible"
	    and ocs nil = true
	      | ocs (t::ts) = oc t andalso ocs ts
	in
	    oc t
	end
    fun deref (t as (REF(x))) =
	(case !x of
             NON => t
	   | s => deref s)
      | deref t = t
    fun unify' (REF r, t) sc = unify_REF (r,t) sc
      | unify' (s, REF r) sc = unify_REF (r,s) sc
      | unify' (STR(f,ts), STR(g,ss)) sc =
	if (f = g)
	    then unifys (ts,ss) sc
	else ()
      | unify' (CON(f), CON(g)) sc =
	if (f = g) then
	    sc ()
	else
	    ()
      | unify' (INT{i=f}, INT{i=g}) sc =
	if (f = g) then
	    sc ()
	else
	  ()
      | unify' (_, _) sc = ()
    and unifys (nil, nil) sc = sc ()
      | unifys (t::ts, s::ss) sc =
	unify' (deref t, deref s)
	(fn () => unifys (ts, ss) sc)
      | unifys _ sc = ()
    and unify_REF (r, t) sc =
	if same_ref (r, t)
	    then sc ()
	else if occurs_check r t
		 then ( bind(r, t) ; sc () )
	     else ()
  in
    val deref = deref
    fun unify (s, t) = unify' (deref(s), deref(t))
  end (* local *)
end (* Unify *)
