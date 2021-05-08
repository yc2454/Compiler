structure Main : sig val typeCheck : string -> Translate.frag list end =
struct
  fun typeCheck filename = 
    let
      val exp = Parse.parse filename
      val frags = Semant.transProg exp
      fun usePrintTree (channel, flist) = 
        case flist of
           MipsFrame.PROC {body, frame} :: t => (Printtree.printtree (channel, body); usePrintTree (channel, t))
         | _ :: t => usePrintTree (channel, t)
         | [] => print "END\n"
    in
      (usePrintTree (TextIO.stdOut, frags); frags)
    end
  
  (* fun typeCheck filename = 
    let
      val exp = Parse.parse filename
    in
      Semant.transProg exp
    end *)
end

(* PrintAbsyn.print(TextIO.stdOut, Parse.parse "simple.tig"); *)