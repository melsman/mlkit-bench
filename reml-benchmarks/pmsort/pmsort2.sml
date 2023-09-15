val par = ForkJoin.par

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

fun merge (nil,ys) = ys
  | merge (xs,nil) = xs
  | merge (x::xs,y::ys) = if x < y then x :: merge (xs,y::ys)
                          else y :: merge (x::xs,ys)

fun smsort nil = nil
  | smsort xs = let val (l,r) = split xs
                in merge (smsort l, smsort r)
                end

fun pmsort (P:int) (xs:int list) =
    case xs of
        nil => nil
      | _ => if P <= 1 then smsort xs
             else merge let val (l,r) = split xs
                        in par (fn () => pmsort (P div 2) l,
                                fn () => pmsort (P div 2) r)
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

fun chk (x::(ys as y::xs)) = x<=y andalso chk ys
  | chk _ = true

in

val N = CommandLineArgs.parseInt "N" 1000000
val P = CommandLineArgs.parseInt "P" 50
val () =
    let val () = print ("Generating input of size " ^ Int.toString N ^ "...\n")
        val xs = randlist (10,10000) N 0

        val () = Timing.run "merge sort"
                            (fn {endtiming} =>
                                let val res = pmsort P xs
                                    val () = endtiming()
                                in length res = N andalso chk res
                                end)
    in print "Goodbye.\n"
    end
end
