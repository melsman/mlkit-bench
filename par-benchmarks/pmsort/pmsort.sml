local
fun split vs =
    let fun sp (x::y::vs,xs,ys) = sp (vs,x::xs,y::ys)
          | sp ([x],xs,ys) = (x::xs,ys)
          | sp (nil,xs,ys) = (xs,ys)
    in sp (vs,nil,nil)
    end

fun revAcc nil acc = acc
  | revAcc (x::xs) acc = revAcc xs (x::acc)

fun merge (a,b) =
    let fun m (xs,ys,acc) =
            case (xs,ys) of
                (nil,_) => revAcc ys acc
              | (_,nil) => revAcc xs acc
              | (x::xs',y::ys') =>
                if x < y then m (xs',ys, x::acc)
                else m (xs,ys',y::acc)
    in rev(m(a,b,nil))
    end

fun smsort nil = nil
  | smsort [x] = [x]
  | smsort xs = let val (l,r) = split xs
                in merge (smsort l, smsort r)
                end

fun pmsort p [] : int list = []
  | pmsort p [x] = [x]
  | pmsort p xs =
    if p <= 1 then smsort xs
    else let val q = p div 2
             val (xs1,xs2) = split xs
             val (xs1,xs2) = ForkJoin.par (fn () => pmsort q xs1,
                                           fn () => pmsort q xs2)
         in merge(xs1,xs2)
         end

fun rand (a,b) p = a + ((p+13) * 16807) mod (b-a)

fun randlist (a,b) n p =
    let fun gen (0,p,acc) = acc
          | gen (n,p,acc) =
            let val r = rand (a,b) p
            in gen (n-1,r,r::acc)
            end
    in gen (n,p,nil)
    end

fun prList xs =
    List.app (fn x => print(Int.toString x)) xs

fun chk (x::(ys as y::xs)) = if x<=y then chk ys else "ERR: result not sorted\n"
  | chk _ = "OK: sorted\n"

in

val N = CommandLineArgs.parseInt "N" 500000
val P = CommandLineArgs.parseInt "P" 8
val () =
    let val () = print ("Generating input of size " ^ Int.toString N ^ "...\n")
        val xs = randlist (10,10000) N 0
        val () = print "Starting merge sort...\n"
        val endTiming = Timing.start "Sorting"
        val res = pmsort P xs
        val () = endTiming()
        val () = print "Checking result...\n"
        val () = if length res = length xs then print (chk res)
                 else print "ERR: result length differ from arg\n"
    in print "Goodbye.\n"
    end
end
