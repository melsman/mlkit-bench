local
  fun split (v:int array) : int array * int array =
      let val sz = Array.length v
          val sza = sz div 2
          val szb = sz - sza
      in (Array.tabulate(sza, fn i => Array.sub(v,i)),
          Array.tabulate(szb, fn i => Array.sub(v,sza+i)))
      end

fun merge (a:int array,b:int array) : int array =
    let val sza = Array.length a
        val szb = Array.length b
        val sz = sza + szb
    in if sz = 0 then Array.fromList []
       else let val e = if sza > 0 then Array.sub(a,0)
                        else Array.sub(b,0)
                val arr = Array.array(sz,e)
                fun copyRest (a, i, k) =
                    if k >= sz then ()
                    else ( Array.update(arr,k,Array.sub(a,i))
                         ; copyRest(a, i+1, k+1) )
                fun m (i,j,k) =
                    if i >= sza then copyRest(b,j,k)
                    else if j >= szb then copyRest(a,i,k)
                    else let val e_a = Array.sub(a,i)
                             val e_b = Array.sub(b,j)
                         in if e_a < e_b
                            then ( Array.update(arr,k,e_a)
                                 ; m (i+1,j,k+1) )
                            else ( Array.update(arr,k,e_b)
                                 ; m (i,j+1,k+1) )
                         end
            in m (0,0,0)
             ; arr
            end
    end

fun smsort v =
    if Array.length v <= 1 then v
    else let val (l,r) = split v
         in merge (smsort l, smsort r)
         end

fun pmsort p (v:int array) : int array =
    if Array.length v <= 1 then v
    else if p <= 1 then smsort v
    else let val q = p div 2
             val (v1,v2) = split v
             val (v1,v2) = ForkJoin.par (fn () => pmsort q v1,
                                         fn () => pmsort q v2)
         in merge (v1,v2)
         end

fun rand (a,b) s =
    let val s = ((s+13) * 16807) mod 23423 (* be careful with MLton Overflow *)
    in (a+s mod (b-a), s)
    end

fun randArr (a:int,b:int) (n:int) : int array =
    let val seed = ref 0
    in Array.tabulate(n, fn _ =>
                            let val (r,newseed) = rand (a,b) (!seed)
                            in seed := newseed
                             ; r
                            end)
    end

fun prArr a =
    ( Array.app (fn x => print(Int.toString x ^ " ")) a
    ; print "\n")

fun chk (a:int array) (i:int)=
    if Array.length a <= 1 orelse i > Array.length a - 2 then "OK: sorted\n"
    else if Array.sub(a,i) <= Array.sub(a,i+1) then chk a (i+1)
    else "ERR: result not sorted\n"

in

val N = CommandLineArgs.parseInt "N" 1000000
val P = CommandLineArgs.parseInt "P" 40
val () =
    let val () = print ("Generating input of size " ^ Int.toString N ^ "...\n")
        val v = randArr (10,10000) N
        val () = print "Starting merge sort...\n"
        val endTiming = Timing.start "Sorting"
        val res = pmsort P v
        val () = endTiming()
        val () = print "Checking result...\n"
        val () = if Array.length res = Array.length v then print (chk res 0)
                 else print "ERR: result length differ from arg\n"
    in print "Goodbye.\n"
    end
end
