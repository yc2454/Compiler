structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) islast n =
     let 
        val _ = print ("emit " ^ (Symbol.name (MipsFrame.name frame)) ^ "\n")
        (* val _ = Printtree.printtree(out,body); *)
        val stms = Canon.linearize body
        (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        (* val _ = app (fn s => Printtree.printtree(out,s)) stms'; *)
        val instrs' = List.concat(map (MipsGen.codegen frame) stms')

        val instrs'' = MipsFrame.procEntryExit2(frame, instrs')
        val {prolog, body=instrs''', epilog} = MipsFrame.procEntryExit3(frame, instrs'')

        val (instrs, allocation) = RegAlloc.alloc(instrs''', frame)

        fun printTemp temp = 
          case (Temp.Table.look (allocation, temp)) of
            SOME reg => reg
          | NONE => Temp.makestring temp

        val format0 = Assem.format(printTemp)

      in 
        (TextIO.output(out, prolog); 
         app (fn i => TextIO.output(out, format0 i)) instrs;
         TextIO.output(out, epilog);
         if islast then TextIO.output(out, "\n.data\n") else ())
     end
     (* end *)
    | emitproc out (F.STRING(lab, s)) islast n = TextIO.output(out, F.string(lab, s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    (* handle e => (TextIO.closeOut out; raise e) *)
       end 

   fun appendLibraries (fin, fout) = 
     let
       val ins = TextIO.openIn fin
       val out = TextIO.openAppend fout
       fun loop ins = 
         case TextIO.inputLine ins of 
          SOME line => (TextIO.output(out, line); loop ins)
        | NONE => ()
     in
       loop ins before TextIO.closeIn ins
     end

   fun isProc (F.PROC{body, frame}) = true
     | isProc (F.STRING (_, _)) = false

   fun orderFrag frags = 
    let
      val funs = List.filter (fn x => isProc x) frags
      val strings = List.filter (fn x => (not (isProc x))) frags
    in
      funs @ strings
    end

   fun removeSuffix (#"."::t) acc = rev acc
     | removeSuffix (h::t) acc = removeSuffix t (h::acc)

   fun compile filename = 
       let val absyn = Parse.parse filename
           val frags = (orderFrag (Semant.transProg absyn))
           val output_filename = (String.implode (removeSuffix (String.explode filename) [])) ^ ".s"
           fun emitting (out, frags, n) = 
             case frags of
                 [] => TextIO.output(out, "")
               | h :: [] => emitproc out h true n
               | (h as F.PROC{body, frame}) :: F.STRING(lab, s) :: t => 
                 (emitproc out h true n; emitting (out, (F.STRING(lab, s)::t), n+1))
               | h :: t => (emitproc out h false n; emitting (out, t, n+1))
        in 
            (withOpenFile output_filename (fn out => emitting (out, frags, 0));
             appendLibraries ("runtime-le.s", output_filename);
             appendLibraries ("sysspim.s", output_filename))
       end

end
