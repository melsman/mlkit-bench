signature COMPILE = sig
  val compile : {flags:string,     (* compile time flags *)
                 src: string       (* src file *)
                } -> string option

  val version : unit -> string     (* return compiler version *)
end

(**

[compile {src,flags}] returns SOME f on success, where f is an
executable file; returns NONE on error.

[version ()] returns the compiler version as a string key; the exact
format is specific to the actual compiler.
*)
