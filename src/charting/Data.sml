

structure Data = struct

(* similar to definitions in mlkit_bench/Bench.sml *)

fun die s = raise Fail ("Data: " ^ s)

type kb = int
type time = Time.time

type report = {  (* MemTime.report *)
  rss   : int,
  size  : int,
  data  : int,
  stk   : int,
  exe   : int,
  sys   : real,
  user  : real,
  real  : real
}

type line = {
  cname    : string,           (* compiler name *)
  cversion : string,           (* compiler version *)
  date     : ISODate.t,        (* date of comp/run *)
  mach     : string,           (* machine identifier (cpu, os, arch) *)
  pname    : string,           (* program name *)
  plen     : int,              (* program length (lines) *)
  ctime    : real,             (* compile time *)
  binsz    : kb,               (* size of binary executable *)
  runs     : report list,      (* the runs *)
  err      : string            (* err string ("": no errors) *)
}

local

  open Json

  fun lookS obj x =
      case objLook obj x of
          SOME (STRING s) => s
        | SOME _ => die ("value associated with " ^ x ^ " is not a string")
        | NONE => die ("no value associated with " ^ x)

  fun getI s = case Int.fromString s of
                   SOME i => i
                 | NONE => die ("getI: " ^ s)

  fun isodateOfString s =   (* "%Y-%m-%d %H:%M" *)
      case ISODate.fromString (String.extract(s,0,SOME 10)) of
          SOME d => d
        | NONE => die ("the string " ^ s ^ " does not start with an iso-date")

  fun lookD obj x =
      let val s = lookS obj x
      in isodateOfString s
      end

  fun lookT obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          (case Time.fromString s of
               SOME t => t
             | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s))
        | SOME _ => die ("value associated with " ^ x ^ " is not a number")
        | NONE => die ("no value associated with " ^ x)

  fun lookI obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          let fun getInt s =
                  case Int.fromString s of
                      SOME i => i
                    | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s)
          in case String.fields (fn c => c = #".") s of
                 [s] => getInt s
               | [a,b] => (case Int.fromString b of
                               SOME 0 => getInt a
                             | _ => die ("Number " ^ s ^ " associated with " ^ x ^ " is not an integer"))
               | _ => die ("Wrong type of number value for " ^ x)
          end
        | SOME _ => die ("value associated with " ^ x ^ " is not a string")
        | NONE => die ("no value associated with " ^ x)

  fun lookR obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          (case Real.fromString s of
               SOME r => r
             | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s))
        | SOME _ => die ("value associated with " ^ x ^ " is not a string")
        | NONE => die ("no value associated with " ^ x)

  fun toRun (OBJECT obj) : report =
      {rss  = lookI obj "rss",
       size = lookI obj "size",
       data = lookI obj "data",
       stk  = lookI obj "stk",
       exe  = lookI obj "exe",
       sys  = lookR obj "sys",
       user = lookR obj "user",
       real = lookR obj "real"}
    | toRun _ = die "toRun expects an object"

  fun toLine (OBJECT obj) : line =
      {cname    = lookS obj "cname",
       cversion = lookS obj "cversion",
       date     = lookD obj "datetime",
       mach     = lookS obj "mach",
       pname    = lookS obj "pname",
       plen     = lookI obj "plen",
       ctime    = lookR obj "ctime",
       binsz    = lookI obj "binsz",
       runs     = (case objLook obj "runs" of
                       SOME (ARRAY rs) => map toRun rs
                     | SOME _ => die "value associated with 'runs' is not an array"
                     | NONE => die "no value associated with 'runs'"),
       err      = lookS obj "err"}
    | toLine _ = die "toLine expects an object"

in
fun fromJsonString (s:string) : line list =
    Json.foldlArrayJson (fn (t,ls) => toLine t :: ls) nil s
end


(* https://api.github.com/repos/ *)
structure X = Js.XMLHttpRequest

fun getUrl url (f:string->unit) : unit =
    let val r = X.new()
    in X.openn r {method="GET", url=url, async=true}
     ; X.onStateChange r (fn () =>
                             case X.state r of
                                 4 =>
                                 (case X.response r of
                                      SOME s => f s
                                    | NONE => raise Fail ("no response from " ^ url))
                              | _ => ())
     ; X.send r NONE
    end

infix $> ?>
fun (Json.OBJECT obj) ?> s =
    (case Json.objLook obj s of
         SOME t => t
       | NONE => die ("?> couldn't find '" ^ s ^ "' in object"))
  | _ ?> s = die "?> expects left argument to be an object"

fun j $> s =
    case j ?> s of
        Json.STRING s => s
      | _ => die ("$> expects string for '" ^ s ^ "' in object")

fun getReports (f : string list -> unit) : unit =
    getUrl "https://api.github.com/repos/melsman/mlkit-bench/contents/reports"
           (fn c =>
               let val l =
                       Json.foldlArrayJson (fn (j,ls) => (j $> "download_url") :: ls) nil c
               in f l
               end)

fun processLink x (f:line list -> unit) : unit =
    getUrl x (fn s => f (fromJsonString s))

fun processLinks links (f:(string*line list) list -> unit) : unit =
    let fun loop xs f =
            case xs of
                nil => f nil
              | x::xs =>
                loop xs (fn ds =>
                            processLink x (fn d => f ((x,d)::ds)))
    in loop links f
    end

type git_tag_data = {tag: string,
                     date : (string -> unit) -> unit}

fun cache_cc (f : (string -> unit) -> unit) : (string -> unit) -> unit =
    let val xr : string option ref = ref NONE
    in fn g => case !xr of
                   SOME x => g x
                 | NONE => f (fn x => (xr := SOME x; g x))
    end


fun getTagDate url (f : string -> unit) : unit =
    getUrl url (fn c =>
                   let val j = Json.fromString c
                       fun findDate obj =
                           case Json.objLook obj "date" of
                               SOME (Json.STRING d) => d
                             | _ => die "getTagDate.no date object"
                       val d = case j of
                                   Json.OBJECT obj =>
                                   (case Json.objLook obj "author" of
                                        SOME (Json.OBJECT obj) => findDate obj
                                      | _ =>
                                        (case Json.objLook obj "tagger" of
                                             SOME (Json.OBJECT obj) => findDate obj
                                           | _ => die "getTagDate.no tagger or author object"))
                                 | _ => die "getTagDate.expecting object"
                   in f d
                   end)


fun getMLKitTags (f : git_tag_data list -> unit) : unit =
    getUrl "https://api.github.com/repos/melsman/mlkit/git/refs/tags"
           (fn c =>
               let val l =
                       Json.foldlArrayJson
                           (fn (j,ls) =>
                               let val reference = j $> "ref"
                                   val tag = case String.tokens (fn c => c = #"/") reference of
                                                 ["refs","tags",tag] => tag
                                               | _ => die "getMLKitTags.wrong format in ref"
                                   val url  = j ?> "object" $> "url"
                               in {tag=tag,
                                   date=cache_cc(getTagDate url)} :: ls
                               end) nil c
               in f l
               end)


end
