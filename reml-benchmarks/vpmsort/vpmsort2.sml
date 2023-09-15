local

structure AS = ArraySlice
type 'a slice = 'a AS.slice

type PG = ForkJoin.gcs
val G   = 4096*8         (* grain *)

(* Parallel allocate with initialisation - O(1) *)
fun palloc P n e =
    let val a = ForkJoin.alloc n e
    in (*ForkJoin.parfor' (P,G) (0,n) (fn i => Array.update(a,i,e))*)
     () ; a
    end

fun alloc n e = ForkJoin.alloc n e

in

(* Constant-time slice split - O(1) *)
fun splitSlice (sl:'a slice) : 'a slice * 'a slice =
    let val n = AS.length sl div 2
    in (AS.subslice(sl,0,SOME n),
        AS.subslice(sl,n,NONE))
    end

(* Sequantial merge - O(n+m) *)
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

(* Sequential slice merge - O(n+m) *)
fun mergeSlice (a:int slice) (b:int slice) (sl:int slice) : unit =
    let val sza = AS.length a
        val szb = AS.length b
        val sz = sza + szb
        fun copyRest (a, i, k) =
            if k >= sz then ()
            else ( AS.update(sl,k,AS.sub(a,i))
                 ; copyRest(a, i+1, k+1) )
        fun m (i,j,k) =
            if i >= sza then copyRest(b,j,k)
            else if j >= szb then copyRest(a,i,k)
            else let val e_a = AS.sub(a,i)
                     val e_b = AS.sub(b,j)
                 in if e_a < e_b
                    then ( AS.update(sl,k,e_a)
                         ; m (i+1,j,k+1) )
                    else ( AS.update(sl,k,e_b)
                         ; m (i,j+1,k+1) )
                 end
    in m (0,0,0)
    end

(* Sequential slice copying - O(n) - exomorphic *)
fun copySeq (s:'a AS.slice) (t:'a AS.slice) : unit =
    let val (d,di,_) = AS.base t
    in AS.copy {src=s,dst=d,di=di}
    end

(* Parallel copying - O(n) work, O(log n) span *)
fun copy P (s:'a AS.slice) (t:'a AS.slice) : unit =
    let val szs = AS.length s
    in if szs < G orelse P <= 1
       then copySeq s t
       else let val i = szs div 2
                val s1 = AS.subslice(s,0,SOME i)
                val s2 = AS.subslice(s,i,NONE)
                val t1 = AS.subslice(t,0,SOME i)
                val t2 = AS.subslice(t,i,NONE)
            in ForkJoin.par (fn () => copy (P div 2) s1 t1,
                             fn () => copy (P div 2) s2 t2)
             ; ()
            end
    end

(* Parallel slice merge that goes sequential on lack of parallel resources *)
fun pmergeSlice P (a:int slice) (b:int slice) (sl:int slice) : unit =
    if P <= 1 orelse AS.length sl < G then mergeSlice a b sl
    else if AS.length a = 0 then
      copy P b sl
    else if AS.length b = 0 then
      copy P a sl
    else (* split *)
      let val n = AS.length a div 2    (* number of elements in resulting left division of a *)
          val pivot = AS.sub(a,n)
          val a1 = AS.subslice(a,0,SOME n)
          val a2 = AS.subslice(a,n,NONE)
          val m = BinarySearch.search Int.compare b pivot
          val b1 = AS.subslice(b,0,SOME m)
          val b2 = AS.subslice(b,m,NONE)
          val k = n + m
          val sl1 = AS.subslice(sl,0,SOME k)
          val sl2 = AS.subslice(sl,k,NONE)
      in ForkJoin.par (fn () => pmergeSlice (P div 2) a1 b1 sl1,
                       fn () => pmergeSlice (P div 2) a2 b2 sl2)
       ; ()
      end

fun pmerge P (a:int array,b:int array) : int array =
    let val sza = Array.length a
        val szb = Array.length b
        val sz = sza + szb
    in if sz = 0 then Array.fromList nil
       else let val e = if sza > 0 then Array.sub(a,0)
                        else Array.sub(b,0)
                val arr = palloc P sz e
            in pmergeSlice P (AS.full a) (AS.full b) (AS.full arr)
             ; arr
            end
    end

fun toarray P (sl: 'a slice) : 'a array =
    case AS.length sl of
        0 => Array.fromList nil
      | 1 => Array.array (1,AS.sub(sl,0))
      | sz => let val a = palloc P sz (AS.sub(sl,0))
              in copy P sl (AS.full a)
               ; a
              end

fun toarraySeq (sl: 'a slice) : 'a array =
    case AS.length sl of
        0 => Array.fromList nil
      | 1 => Array.array (1,AS.sub(sl,0))
      | sz => let val a = alloc sz (AS.sub(sl,0))
              in copySeq sl (AS.full a)
               ; a
              end

(* Sequantial exomorphic merge sort *)
fun smsort (sl:int slice) : int array =
    if AS.length sl <= 1 then toarraySeq sl
    else let val (l,r) = splitSlice sl
         in merge (smsort l, smsort r)
         end

(* pmsort(sl): Endomorphic function returning an array in a region
   different from the regions holding the argument *)
fun pmsort P (sl:int slice) : int array =
    let val sz = AS.length sl
    in if sz <= 1 then toarraySeq sl
       else if P <= 1 orelse sz < G
       then smsort sl
       else let val Q = P div 2
                val (l: int slice,r:int slice) = splitSlice sl
                val (l,r) = ForkJoin.par (fn () => pmsort Q l,
                                          fn () => pmsort Q r)
                val x = pmerge P (l,r)
            in x
            end
    end

local

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

fun chk (a:int array) (i:int) =
    (Array.length a <= 1 orelse i > Array.length a - 2) orelse
    (Array.sub(a,i) <= Array.sub(a,i+1) andalso chk a (i+1))

val () =
    let
      val N = CommandLineArgs.parseInt "N" 1000000
      val P = CommandLineArgs.parseInt "P" 50
      val () = print ("Generating input of size " ^ Int.toString N ^ "...\n")
      val v = randArr (10,10000) N

      val () = Timing.run "merge sort"
                          (fn {endtiming} =>
                              let val res = pmsort P (AS.full v)
                                  val () = endtiming()
                              in Array.length res = N andalso chk res 0
                              end)
    in print "Goodbye.\n"
    end
in
end
end
