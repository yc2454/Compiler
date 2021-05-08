structure MakeGraph : 
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
    (* exception UnknownNode *)
end = 
struct
  fun instrs2graph instrs = 
    let
      val graph = Graph.newGraph()
      
      fun dealInstr1 (nl, use, def, ismove, []) = (rev nl, use, def, ismove)
        | dealInstr1 (nl, use, def, ismove, (Assem.OPER {assem, dst, src, jump} :: t)) = 
            let
              val n = Graph.newNode graph
              val new_use = Graph.Table.enter(use, n, src)
              val new_def = Graph.Table.enter(def, n, dst)
              val new_ismove = Graph.Table.enter(ismove, n, false)
            in
              dealInstr1 ((n::nl), new_use, new_def, new_ismove, t)
            end
        | dealInstr1 (nl, use, def, ismove, (Assem.MOVE {assem, dst, src} :: t)) = 
            let
              val n = Graph.newNode graph
              val new_use = Graph.Table.enter(use, n, [src])
              val new_def = Graph.Table.enter(def, n, [dst])
              val new_ismove = Graph.Table.enter(ismove, n, true)
            in
              dealInstr1 ((n::nl), new_use, new_def, new_ismove, t)
            end
        | dealInstr1 (nl, use, def, ismove, (Assem.LABEL {assem, lab} :: t)) = 
            let
              val n = Graph.newNode graph
              val new_use = Graph.Table.enter(use, n, [])
              val new_def = Graph.Table.enter(def, n, [])
              val new_ismove = Graph.Table.enter(ismove, n, false)
            in
              dealInstr1 ((n::nl), new_use, new_def, new_ismove, t)
            end
      
      val (nodelist, use, def, ismove) = 
        dealInstr1 ([], Graph.Table.empty, Graph.Table.empty, Graph.Table.empty, instrs)
      
      val instr_node_list = ListPair.zip (instrs, nodelist)
      
      exception UnknownNode

      fun sequentialize [] = ()
        | sequentialize ((i, n)::[]) = ()
        | sequentialize ((i1, n1)::(i2, n2)::[]) = 
          Graph.mk_edge {from=n1, to=n2}
        | sequentialize ((i1, n1)::(i2, n2)::t) = 
          case i1 of
             Assem.OPER {jump=SOME j,...} => sequentialize ((i2, n2)::t)
           | _ => (Graph.mk_edge {from=n1, to=n2}; sequentialize ((i2, n2)::t))

      fun findJumpTargets (n, jumps) = 
        case jumps of
          NONE => ()
        | SOME [] => ()
        | SOME (lab :: ll) => 
          let
            fun findLabel (Assem.LABEL {assem, lab=l}, _) = (Symbol.name lab) = (Symbol.name l)
              | findLabel _ = false
            val target_node = 
              case List.find findLabel instr_node_list of
                SOME (_, lab_node) => SOME lab_node
              | NONE => NONE
          in
            case target_node of
               SOME m => (Graph.mk_edge {from=n, to=m}; findJumpTargets (n, SOME ll))
             | NONE => findJumpTargets (n, SOME ll)
          end

      fun makeJumps (Assem.OPER {assem, dst, src, jump}, n) = 
        findJumpTargets (n, jump)
        | makeJumps _ = ()
      
      val _ = (sequentialize instr_node_list; app makeJumps instr_node_list)
      
      (* fun printSuccs [] = ()
        | printSuccs (n::nn) = 
        let
          val succs = Graph.succ n
          val num_succ = List.length succs
        in
          (print ("num of succ: " ^ (Int.toString num_succ) ^ "\n"); printSuccs nn)
        end

      val _ = printSuccs nodelist *)

    in
      (Flow.FGRAPH{control=graph, def=def, use=use, ismove=ismove}, nodelist)
    end
end