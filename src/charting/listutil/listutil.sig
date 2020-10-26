(* Copyright 2015, Martin Elsman, MIT-license *)

signature LISTUTIL = sig

  val filtermap : ('a -> 'b option) -> 'a list -> 'b list

  val findKey   : (''a * 'b) list -> ''a -> 'b option

  val appi : ('a * int -> unit) -> 'a list -> unit
  val mapi : ('a * int -> 'b) -> 'a list -> 'b list

  (* This is a tail recursive version of map *)
  val map : ('a -> 'b) -> 'a list -> 'b list
  val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

  val contains : ''a list * ''a -> bool

  val dedup : ''a list -> ''a list

  val dedupBy : ('a -> ''b) -> 'a list -> 'a list

  val lookup: (unit -> 'v) -> (''k * 'v) list -> ''k -> 'v

  val lookupOpt: (''k * 'v) list -> ''k -> 'v option

  val replace : (''k * 'v) list -> ''k -> ''k * 'v -> (''k * 'v) list option

  val removeOpt : (''k * 'v) list -> ''k -> (''k * 'v) list option

  val remove : (''k * 'v) list -> ''k -> (''k * 'v) list

  val chopN : int -> 'a list -> ('a list -> unit) -> unit

  val chopNacc : int -> 'a list -> 'b -> ('a list * 'b -> 'b) -> 'b

  val cross : ('a * 'a -> 'b) -> 'a list -> 'b list

  val mapPrev : ('a * 'a -> 'b) -> 'a list -> 'b list

  val dropWhile : ('a -> bool) -> 'a list -> 'a list

  val takeWhile : ('a -> bool) -> 'a list -> 'a list

  val splitAt : ('a -> bool) -> 'a list -> 'a list * 'a list

  val groupBy : ('a -> ''b) -> 'a list -> (''b * 'a list) list

  val groupBy' : ('a -> ''b * 'c) -> 'a list -> (''b * 'c list) list

  val locate : ''a -> ''a list -> int option
  val mem : ''a -> ''a list -> bool

  val listToString : string list -> string

  val update : ''k * 'v -> (''k * 'v) list -> (''k * 'v) list

  val last : 'a list -> 'a option
  val head : 'a list -> 'a option

  val maximumBy : ('a * 'a -> order) -> 'a list -> 'a option

  val concatMap : ('a -> 'b list) -> 'a list -> 'b list

  val nth : 'a list * int -> 'a option

  val withPct : int -> ('a * real) list -> ('a * real * real) list

end
