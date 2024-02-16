structure CompileSMLNJ : COMPILE =
  struct
    fun compile {env:(string*string)list,flags:string, src:string} =
      let val {base,ext} = OS.Path.splitBaseExt src
          val {dir,file} = OS.Path.splitDirFile base
	  val cmsrc = file ^ ".cm"                    (* assume existence of a cm-file *)
	  fun comp src =
	      let val cmd = "(cd " ^ dir ^ "; /usr/local/smlnj/bin/ml-build " ^ cmsrc ^ " Main.main)"   (* ignore flags and environment *)
                  val target = "/usr/local/smlnj/bin/sml @SMLload=" ^ base ^ ".amd64-darwin"
	      in  print ("Executing: '" ^ cmd ^ "'\n")
	        ; if OS.Process.isSuccess(OS.Process.system cmd) then SOME target
		  else NONE
	      end
      in case ext
	   of SOME "sml" => comp src
	    | SOME "mlb" => comp src
	    | _ => NONE
      end

    fun version () =
        FileUtil.trimWS(FileUtil.systemOut "/usr/local/smlnj/bin/sml @SMLversion")
        handle X => ("Failed to extract smlnj version; " ^ exnMessage X)
  end
