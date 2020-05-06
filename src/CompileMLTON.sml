structure CompileMLTON : COMPILE =
  struct
    fun compile {flags:string, src:string} =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val target = base ^ "_mlton.exe"
	  fun comp src =
	    let val cmd = "mlton -output " ^ target ^ " " ^ flags ^ " " ^ src
	    in  print ("Executing: " ^ cmd ^ "\n")
	      ; if OS.Process.isSuccess(OS.Process.system cmd) then SOME target
		else NONE
	    end
      in case ext
	   of SOME "sml" => comp src
	    | SOME "mlb" => comp src
	    | _ => NONE
      end

    fun version () =
        FileUtil.trimWS(FileUtil.systemOut "mlton")
        handle X => ("Failed to extract mlton version; " ^ exnMessage X)
  end
