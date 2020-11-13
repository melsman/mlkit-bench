

structure Data = struct

open DataType  (* shared with mlkit-bench *)

fun die s = raise Fail ("Data: " ^ s)

type kb = int
type time = Time.time

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
      in ISODate.toDate(isodateOfString s)
      end

  fun lookT obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          (case Time.fromString s of
               SOME t => t
             | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s))
        | SOME _ => die ("value associated with " ^ x ^ " is not a number")
        | NONE => die ("no value associated with " ^ x)

  local
    fun lookInt x s =
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
  in
    fun lookI obj x =
        case objLook obj x of
            SOME (NUMBER s) => lookInt x s
          | SOME _ => die ("value associated with " ^ x ^ " is not a string")
          | NONE => die ("no value associated with " ^ x)
    fun lookI0 obj x =
        case objLook obj x of
            SOME (NUMBER s) => lookInt x s
          | SOME _ => die ("value associated with " ^ x ^ " is not a string")
          | NONE => 0
  end

  fun lookR obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          (case Real.fromString s of
               SOME r => r
             | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s))
        | SOME _ => die ("value associated with " ^ x ^ " is not a string")
        | NONE => die ("no value associated with " ^ x)

  fun lookR0 obj x =
      case objLook obj x of
          SOME (NUMBER s) =>
          (case Real.fromString s of
               SOME r => r
             | NONE => die ("wrong type of value associated with " ^ x ^ " - found number " ^ s))
        | SOME _ => die ("value associated with " ^ x ^ " is not a string")
        | NONE => 0.0

  fun toRun (OBJECT obj) : measurement =
      {rss   = lookI obj "rss",
       size  = lookI obj "size",
       data  = lookI obj "data",
       stk   = lookI obj "stk",
       exe   = lookI obj "exe",
       sys   = lookR obj "sys",
       user  = lookR obj "user",
       real  = lookR obj "real",
       gc    = lookR0 obj "gc",
       majgc = lookR0 obj "majgc",
       gcn   = lookI0 obj "gcn",
       majgcn= lookI0 obj "majgcn"
      }
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


end
