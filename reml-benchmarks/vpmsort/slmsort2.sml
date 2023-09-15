local

structure A = Array
structure AS = ArraySlice
type 'a slice = 'a AS.slice

type PG = ForkJoin.gcs

val G   = 4096*8         (* grain *)
val G = CommandLineArgs.parseInt "G" G

val par = ForkJoin.par

(* Parallel allocate with initialisation - O(1) *)
fun palloc p n e =
    let val a = ForkJoin.alloc n e
    in (*ForkJoin.parfor' (P,G) (0,n) (fn i => Array.update(a,i,e))*)
     () ; a
    end

fun alloc n e = ForkJoin.alloc n e

in

(* Constant-time slice split - O(1) *)
fun split n sl =
    (AS.subslice(sl,0,SOME n),
     AS.subslice(sl,n,NONE))

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
fun mergeSl (a:int slice) (b:int slice) (sl:int slice) : unit =
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
            else let val x = AS.sub(a,i)
                     val y = AS.sub(b,j)
                 in if x < y then
                     ( AS.update(sl,k,x)
                     ; m (i+1,j,k+1) )
                    else  ( AS.update(sl,k,y)
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
    if AS.length s < G orelse P <= 1 then copySeq s t
    else let val i = AS.length s div 2
             val (s1,s2) = split i s
             val (t1,t2) = split i t
         in par (fn () => copy (P div 2) s1 t1,
                 fn () => copy (P div 2) s2 t2)
          ; ()
         end

(* Parallel slice merge that goes sequential on small slices *)
fun pmergeSl P (a:int slice) (b:int slice) (t:int slice) : unit =
    if P <= 1 orelse AS.length t < G then mergeSl a b t
    else if AS.length a = 0 then copy P b t
    else if AS.length b = 0 then copy P a t
    else let val n = AS.length a div 2
             val pivot = AS.sub(a,n)
             val m = BinarySearch.search Int.compare b pivot
             val (a1,a2) = split n a
             val (b1,b2) = split m b
             val (t1,t2) = split (n+m) t
         in par (fn () => pmergeSl (P div 2) a1 b1 t1,
                 fn () => pmergeSl (P div 2) a2 b2 t2)
          ; ()
         end

fun pmerge P (a:int array, b:int array) : int array =
    let val sza = A.length a
        val sz = sza + A.length b
    in if sz = 0 then A.fromList nil
       else let val e = if sza > 0 then A.sub(a,0)
                        else A.sub(b,0)
                val tgt = palloc P sz e
            in pmergeSl P (AS.full a) (AS.full b) (AS.full tgt)
             ; tgt
            end
    end

fun pmerge' P (a:int array, b:int array) : int array =
    let val sza = A.length a
        val sz = sza + A.length b
    in if sza = 0 then b
       else if sz = 0 then A.fromList nil
       else let val e = if sza > 0 then A.sub(a,0)
                        else A.sub(b,0)
                val tgt = palloc P sz e
            in pmergeSl P (AS.full a) (AS.full b) (AS.full tgt)
             ; tgt
            end
    end

fun toarray (sl: int slice) : int array =
    case AS.length sl of
        0 => A.fromList nil
      | 1 => A.array (1,AS.sub(sl,0))
      | sz => let val a = alloc sz (AS.sub(sl,0))
              in copySeq sl (AS.full a)
               ; a
              end

(* Sequential exomorphic merge sort *)
fun msort (sl:int slice) : int array =
    if AS.length sl <= 1 then toarray sl
    else let val (l,r) = split (AS.length sl div 2) sl
         in merge (msort l, msort r)
         end

(* Parallel exomorphic merge sort *)
fun pmsort P (sl:int slice) : int array =
    let val sz = AS.length sl
    in if sz <= 1 then toarray sl
       else if sz < G orelse P <= 1 then msort sl
       else pmerge P let val (l,r) = split (sz div 2) sl
                     in par (fn () => pmsort (P div 2) l,
                             fn () => pmsort (P div 2) r)
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
