(* Copyright 2015, Martin Elsman, MIT-license *)

structure Listutil :> LISTUTIL = struct

fun filtermap0 f nil acc = List.rev acc
  | filtermap0 f (x::xs) acc = filtermap0 f xs (case f x of
                                                    SOME v => v::acc
                                                  | NONE => acc)

fun filtermap f xs = filtermap0 f xs nil

fun findKey nil k = NONE
  | findKey ((x,f)::rest) k = if k=x then SOME f else findKey rest k

fun appi f xs =
    let fun ai f i nil = ()
          | ai f i (x::xs) = (f(x,i);ai f (i+1) xs)
    in ai f 0 xs
    end

fun mapi f xs =
    let fun mi f i nil acc = rev acc
          | mi f i (x::xs) acc = mi f (i+1) xs (f(x,i)::acc)
    in mi f 0 xs nil
    end

fun map f xs =
    let fun mymap0 (nil, ys) = ys   (* here for performance reasons (tail-recursion) *)
          | mymap0 (x::xs, ys) =
            let val y = f x
            in mymap0 (xs, y::ys)
            end
    in rev (mymap0 (xs, nil))
    end

fun mapPartial f =
  let fun aux acc [] = acc
        | aux acc (x :: xs) =
          case f x of
              SOME res => aux (res :: acc) xs
            | NONE => aux acc xs
  in rev o aux [] end

infix contains
fun [] contains  _ = false
  | (x :: xs) contains y = if x = y then true else xs contains y

fun dedup xs =
    let fun aux acc [] = acc
          | aux acc (x :: xs)  =
            if List.exists (fn y => x = y) acc then
                aux acc xs
            else aux (x :: acc) xs
    in aux [] xs end

fun dedupBy f = (* : ('a -> ''b) -> 'a list -> 'a list *)
    let fun aux acc [] = map #2 acc
          | aux acc (x :: xs) =
            let val x' = f x
            in if List.exists (fn (k, _) => x' = k) acc then
                   aux acc xs
               else
                   aux ((x', x) :: acc) xs
            end
    in aux [] end

fun lookup f [] s = f ()
  | lookup f ((k, v) :: xs) s = if k = s then v else lookup f xs s

fun lookupOpt [] s = NONE
  | lookupOpt ((k, v) :: xs) s = if k = s then SOME v else lookupOpt xs s

fun replace l s new =
    let fun loop nil acc = NONE
          | loop ((k,v) :: xs) acc = if k = s then SOME (List.revAppend (new::acc, xs))
                                     else loop xs ((k,v) :: acc)
    in loop l nil
    end

fun removeOpt l key =
    let fun loop nil acc = NONE
          | loop ((k,v) :: xs) acc = if k = key then SOME (List.revAppend (acc, xs))
                                     else loop xs ((k,v) :: acc)
    in loop l nil
    end

fun remove l key =
    let fun loop nil acc = List.revAppend (acc, nil)
          | loop ((k,v) :: xs) acc = if k = key then List.revAppend (acc, xs)
                                     else loop xs ((k,v) :: acc)
    in loop l nil
    end

fun chopN N ss f =
    let fun tk 0 xs acc = (rev acc,xs)
          | tk n nil acc = (rev acc,nil)
          | tk n (x::xs) acc = tk (n-1) xs (x::acc)
    in case tk N ss nil of
           (nil,nil) => ()
         | (acc, nil) => f acc
         | (acc,rest) => (f acc; chopN N rest f)
    end

fun chopNacc N ss a f =
    let fun tk 0 xs acc = (rev acc,xs)
          | tk n nil acc = (rev acc,nil)
          | tk n (x::xs) acc = tk (n-1) xs (x::acc)
    in case tk N ss nil of
           (nil,nil) => a
         | (c,nil) => f (c,a)
         | (c,rest) => chopNacc N rest (f(c,a)) f
    end

fun cross f xs =
  List.concat (map (fn x => map (fn y => f (x, y)) xs) xs)

fun mapPrev _ [] = []
  | mapPrev f (xs as (x :: rest)) =
    ListPair.map f (rest, xs)

fun splitAt p =
  let fun aux acc [] = (rev acc, [])
        | aux acc (x :: xs) =
          if p x then (rev acc, x :: xs)
          else aux (x :: acc) xs
  in aux [] end

fun dropWhile p [] = []
  | dropWhile p (x :: xs) = if p x then dropWhile p xs else (x :: xs)

fun takeWhile p =
    let fun takeWhile' acc [] = rev acc
          | takeWhile' acc (x :: xs) =
            if p x then takeWhile' (x :: acc) xs
            else rev acc
    in takeWhile' [] end

fun splitAt p =
  let fun splitAt' acc [] = (rev acc, [])
        | splitAt' acc (x :: xs) = if p x then (rev acc, x :: xs)
                                   else splitAt' (x :: acc) xs
  in splitAt' [] end

fun groupBy discriminator =
  let fun insert x [] = [(discriminator x, [x])]
        | insert x ((y, ys) :: rest) =
          if discriminator x = y then
              (y, x :: ys) :: rest
          else (y, ys) :: insert x rest
      fun aux acc [] = acc
        | aux acc (x :: xs) = aux (insert x acc) xs
  in aux [] end

fun groupBy' discriminator =
    let fun insert x [] =
            let val (id, elem) = discriminator x
            in [(id, [elem])] end
          | insert x ((y, ys) :: rest) =
            let val (id, elem) = discriminator x
            in if id = y then
                   (y, elem :: ys) :: rest
               else (y, ys) :: insert x rest
            end
      fun aux acc [] = acc
        | aux acc (x :: xs) = aux (insert x acc) xs
  in aux [] end

fun locate elem =
  let fun aux _ [] = NONE
        | aux n (x :: xs) = if x = elem then SOME n
                            else aux (n + 1) xs
  in aux 0 end

fun mem x nil = false
  | mem x (y::ys) = x = y orelse mem x ys

fun listToString xs = "[" ^ String.concatWith ", " xs ^ "]"

fun update (k, v) xs =
    case replace xs k (k, v) of
        SOME res => res
      | NONE => (k, v) :: xs

fun last xs =
    SOME (List.last xs)
    handle Empty => NONE

fun head [] = NONE
  | head (x :: _) = SOME x

fun maximumBy f [] = NONE
  | maximumBy f (x :: xs) =
    SOME (foldl (fn (x, acc) =>
                    case f (x, acc) of
                        GREATER => x
                      | _ => acc)
                x xs)

fun concatMap f xs =
    rev (foldl (fn (x, b) => foldl op:: b (f x)) [] xs)

fun nth ([], _) = NONE
  | nth (x :: _, 0) = SOME x
  | nth (_ :: xs, n) = nth (xs, n-1)

fun withPct N data = (* returns pct in third values, which in absolute values will add up to 100.0 *)
    let val total = List.foldl (fn ((_,v),a) => Real.abs v + a) 0.0 data
    in if Real.==(total,0.0) then nil
       else let val data = map (fn (s,v) => (s,v,Real.floor (Real.abs v * real N / total))) data
                val total2 = List.foldl (fn ((_,_,i),a) => i+a) 0 data
                val to_distribute = N - total2
                val data = List.map (fn (k,v,i) => (k,v,i,Real.abs v - real i)) data
                val data = Listsort.sort (fn (x,y) => Real.compare (#4 x,#4 y)) data
                val (data,_) = List.foldl (fn ((k,v,i,d),(acc,z)) => if z > 0 then ((k,v,i+1,d)::acc,z-1)
                                                                     else ((k,v,i,d)::acc,z)) (nil, to_distribute) data
            in List.map (fn (k,v,i,_) => (k,v,100.0 * real i / real N)) data
            end
    end

end
