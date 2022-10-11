
fun main () =
    case CommandLine.arguments() of
        [arg] =>
        let val n = FileUtil.linesOfFile arg
        in print (Int.toString n ^ "\n")
        end
      | _ => print "Usage: smlloc file.{sml,sig,mlb}\n"

val () = main ()
