(*
 * Adapted from the MPL examples: https://github.com/MPLLang/mpl/tree/master/examples
 *
 * This module was rewritten from scratch, although with the same
 * signature.  The MPL version is parallel, while this is entirely
 * sequential.  I don't think parallel IO is a good idea anyway.
 *)

structure ReadFile:
sig
  val contents: string -> string
  val contentsSeq: string -> char Seq.t
  val contentsBinSeq: string -> Word8.word Seq.t
end =
struct

  fun contentsSeq filename =
      let val file = TextIO.openIn filename
          val s = TextIO.inputAll file
          val () = TextIO.closeIn file
      in
        ArraySlice.full(Array.tabulate(String.size s,
                                       fn i => String.sub(s, i)))
      end

  fun contentsBinSeq filename =
      let val file = BinIO.openIn filename
          val s = BinIO.inputAll file
          val () = BinIO.closeIn file
      in
        ArraySlice.full(Array.tabulate(Word8Vector.length s,
                                       fn i => Word8Vector.sub(s, i)))
      end

  fun contents filename =
    let val file = TextIO.openIn filename
    in
      TextIO.inputAll file before TextIO.closeIn file
    end

end
