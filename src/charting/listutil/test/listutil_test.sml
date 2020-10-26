(* Copyright 2017, Philip Munksgaard, MIT-license *)

structure L = Listutil

fun println s = (print s; print "\n")

fun listString xs = "[" ^ String.concatWith "," (map Int.toString xs) ^ "]"

fun listPrint s xs = print (s ^ ": " ^ listString xs ^ "\n")

fun printOpt s NONE = print (s ^ ": NONE\n")
  | printOpt s (SOME n) = print (s ^ ": SOME(" ^ Int.toString n ^ ")\n")

fun even n = n mod 2 = 0

val testList = [0,1,2,3,4,5,6,7,8,9]

(* filtermap tests *)
val () = listPrint "filtermap1"
                   (L.filtermap (fn n => if even n then SOME (n + 2) else NONE)
                                testList)

val () = listPrint "filtermap2"
                   (L.filtermap (fn n => if even n then SOME (n + 2) else NONE)
                                [])

val () = listPrint "filtermap3"
                   (L.filtermap (fn n => NONE)
                                testList)

val () = print "\n"

(* findKey tests *)
val () = printOpt "findKey1" (L.findKey [(1,10), (2,20)] 1)
val () = printOpt "findKey2" (L.findKey [(1,10), (2,20)] 3)
val () = printOpt "findKey3" (L.findKey [] 0)

val () = print "\n"

(* appi tests *)
val () = L.appi (fn (x, i) => print ("appi: " ^ Int.toString x ^ ":" ^
                                     Int.toString i ^ "\n"))
                testList

val () = L.appi (fn (x, i) => print ("appi: " ^ Int.toString x ^ ":" ^
                                     Int.toString i ^ "\n"))
                []              (* Prints nothing *)

val () = print "\n"

(* mapi tests *)
val () = listPrint "mapi1" (L.mapi (fn (x, i) => x + i) testList)
val () = listPrint "mapi2" (L.mapi (fn (x, i) => x + i) [])

val () = print "\n"

(* map tests *)
val () = listPrint "map1" (L.map (fn x => x + 1) testList)
val () = listPrint "map2" (L.map (fn x => x + 1) [])

val () = print "\n"

(* mapPartial tests *)
val () = listPrint "mapPartial1" (L.mapPartial (Option.filter even) testList)
val () = listPrint "mapPartial2" (L.mapPartial (Option.filter even) [])

val () = print "\n"

(* contains tests *)
val () = print ("contains1: " ^ Bool.toString (L.contains (testList, 5)) ^ "\n")
val () = print ("contains2: " ^ Bool.toString (L.contains (testList, 10)) ^ "\n")
val () = print ("contains3: " ^ Bool.toString (L.contains ([], 0)) ^ "\n")

val () = print "\n"

(* dedup tests *)
val () = listPrint "dedup1" (L.dedup (testList @ testList))
val () = listPrint "dedup2" (L.dedup testList)
val () = listPrint "dedup3" (L.dedup [])

val () = print "\n"

(* lookup tests *)
val () = print ("lookup1: " ^
                L.lookup (fn () => "FAIL")
                         [("a","0"), ("b", "1")]
                         "a" ^
                "\n")
val () = print ("lookup2: " ^
                L.lookup (fn () => "FAIL")
                         [("a","0"), ("b", "1")]
                         "b" ^
                "\n")
val () = print ("lookup3: " ^
                L.lookup (fn () => "Ok")
                         [("a","0"), ("b", "1")]
                         "c" ^
                "\n")
val () = print ("lookup4: " ^
                L.lookup (fn () => "Ok") [] "a" ^
                "\n")

val () = print "\n"

(* lookupOpt tests *)
val () = printOpt "lookupOpt1"
                  (L.lookupOpt [("a",0), ("b", 1)] "a")
val () = printOpt "lookupOpt2"
                  (L.lookupOpt [("a",0), ("b", 1)] "b")
val () = printOpt "lookupOpt3"
                  (L.lookupOpt [] "a")
val () = printOpt "lookupOpt4"
                  (L.lookupOpt [("b", 0), ("c", 1)] "a")

val () = print "\n"

(* replace tests *)
val () = print ("replace1: " ^
                Bool.toString (L.replace [("a", 0), ("b", 1)]
                                         "a" ("c", 2) =
                               SOME [("c", 2), ("b", 1)]) ^
                "\n")
val () = print ("replace2: " ^
                Bool.toString (L.replace [("a", 0), ("b", 1)]
                                         "b" ("c", 2) =
                               SOME [("a", 0), ("c", 2)]) ^
                "\n")
val () = print ("replace3: " ^
                Bool.toString (L.replace [("a", 0), ("b", 1)]
                                         "c" ("c", 2) =
                               NONE) ^
                "\n")

val () = print "\n"

(* chopN tests *)
val () = L.chopN 4 testList (listPrint "chopN1")
val () = L.chopN 4 [] (listPrint "chopN2") (* Doesn't print anything *)

val () = print "\n"

(* cross tests *)
val () = listPrint "cross1" (L.cross op+ testList)
val () = listPrint "cross2" (L.cross op+ [1])
val () = listPrint "cross3" (L.cross op+ [])

val () = print "\n"

(* mapPrev tests *)
val () = listPrint "mapPrev1" (L.mapPrev op+ testList)
val () = listPrint "mapPrev2" (L.mapPrev op+ [1])
val () = listPrint "mapPrev3" (L.mapPrev op+ [])

val () = print "\n"

(* dropWhile tests *)
val () = listPrint "dropWhile1" (L.dropWhile even testList)
val () = listPrint "dropWhile2" (L.dropWhile (fn _ => false) testList)
val () = listPrint "dropWhile3" (L.dropWhile even [])

val () = print "\n"

(* takeWhile tests *)
val () = listPrint "takeWhile1" (L.takeWhile even testList)
val () = listPrint "takeWhile2" (L.takeWhile even [0,2,4,6])
val () = listPrint "takeWhile3" (L.takeWhile even [])
val () = listPrint "takeWhile4" (L.takeWhile even [1,3,5,7])

val () = print "\n"

(* splitAt tests *)
val () = let val (left, right) = (L.splitAt even [1,3,5,6,7,8,9])
             val () = listPrint "splitAt1.1" left
             val () = listPrint "splitAt1.2" right
         in () end
val () = let val (left, right) = (L.splitAt even [0,1,3,5,6,7,8,9])
             val () = listPrint "splitAt2.1" left
             val () = listPrint "splitAt2.2" right
         in () end
val () = let val (left, right) = (L.splitAt even [])
             val () = listPrint "splitAt3.1" left
             val () = listPrint "splitAt3.2" right
         in () end

val () = println ""

val () =
    let val input = [(1, 2), (1, 3), (2, 1), (1, 4), (2, 2)]
        val expected = [(1, [2, 3, 4]), (2, [1, 2])]
        val sort = Listsort.sort (Misc.toCompare Int.compare #1) o
                   map (fn (id, xs) => (id, Listsort.sort Int.compare xs))
        val actual = sort (L.groupBy' Misc.id input)
    in if actual = expected then
           println ("groupBy' test 1: Ok")
       else
           println ("groupBy' test 1: failed")
    end

val () = println ""

val () =
    let val input = [(1, 2), (1, 3), (2, 1), (1, 4), (2, 2)]
        val expected = [(1, [(1, 2), (1, 3), (1, 4)]),
                        (2, [(2, 1), (2, 2)])]
        val sort = Listsort.sort (Misc.toCompare Int.compare #1) o
                   map (fn (id, xs) => (id, Listsort.sort (Misc.toCompare Int.compare #2) xs))
        val actual = sort (L.groupBy #1 input)
    in if actual = expected then
           println ("groupBy test 1: Ok")
       else
           println ("groupBy test 1: failed")
    end
