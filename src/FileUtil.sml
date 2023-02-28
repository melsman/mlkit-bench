structure FileUtil = struct

fun deleteFile (f:string):unit = OS.FileSys.remove f handle _ => ()

fun readFile f =
    let val is = TextIO.openIn f
    in (TextIO.inputAll is before TextIO.closeIn is)
       handle X => (TextIO.closeIn is; raise X)
    end

fun trimWS s =
    if size s = 0 then s
    else if Char.isSpace (String.sub(s,size s - 1)) then
      trimWS (String.extract(s,0,SOME(size s - 1)))
    else s

fun systemOut cmd =
    let val f = OS.FileSys.tmpName()
        val cmd = cmd ^ " > " ^ f
    in if OS.Process.isSuccess(OS.Process.system cmd) then
         readFile f before deleteFile f
       else raise Fail ("FileUtil.systemOut.Failed executing the command '" ^ cmd ^ "'")
    end

local
fun sourcesMlb mlbfile =
    let val s = readFile mlbfile
        val ts = String.tokens Char.isSpace s
        (* eliminate tokens with $ in them *)
        val ts = List.filter (not o (CharVector.exists (fn c => c = #"$"))) ts
        (* include only files with sml/sig/mlb-extensions *)
        val ts = List.filter (fn t =>
                                 case OS.Path.ext t of
                                     SOME e => e = "sml" orelse e = "sig" orelse e = "mlb"
                                   | NONE => false) ts
    in ts
    end
in
fun linesOfFile f =
    case OS.Path.ext f of
        SOME ext =>
        if ext = "sig" orelse ext = "sml" then
          (length (String.fields (fn c => c = #"\n") (readFile f))
           handle _ => 0)
        else if ext = "mlb" then
          let val fs = sourcesMlb f
              val dir = OS.Path.dir f
              val fs = map (fn f => OS.Path.concat (dir,f)) fs
          in foldl (op +) 0 (map linesOfFile fs)
          end
        else 0
      | _ => 0
end
end
