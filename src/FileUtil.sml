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

end
