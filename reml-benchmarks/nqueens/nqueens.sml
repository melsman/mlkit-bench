(* Benchmark functionality copied from MaPLe (MPL)
   https://github.com/MPLLang/mpl/blob/master/examples/nqueens/nqueens.sml
   Modified for use with MLKit
 *)

local
val N = CommandLineArgs.parseInt "N" 13
val D = CommandLineArgs.parseInt "D" 3

type board = (int * int) list

fun threatened (i,j,[]) = false
  | threatened (i,j,((x,y)::Q)) =
    i = x orelse j = y orelse i-j = x-y orelse i+j = x+y
    orelse threatened (i,j,Q)

structure Seq = FuncSequence

fun copy nil = nil
  | copy ((x,y)::xs) = (x,y)::copy xs

fun countSol n =
  let
    fun search i b =
      if i >= n then 1 else
      let
        fun tryCol j =
          if threatened (i, j, b) then 0 else search (i+1) ((i,j)::(*copy*) b)
      in
        if i >= D then
          (* if we're already a few levels deep, then just go sequential *)
          Seq.iterate op+ 0 (Seq.tabulate tryCol n)
        else
          Seq.reduce op+ 0 (Seq.tabulate tryCol n)
      end
  in
    search 0 []
  end

in
val () = Timing.run ("Counting number of " ^
                     Int.toString N ^ "x" ^ Int.toString N ^ " solutions")
                    (fn {endtiming} =>
                        let val res = countSol N
                            val () = endtiming()
                        in res = 73712 andalso N = 13
                        end)
end
