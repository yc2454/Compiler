structure RegAlloc :> REG_ALLOC =
struct
  
  type allocation = MipsFrame.register Temp.Table.table 
  val k = MipsFrame.numRegs

  (* structure A = DynamicArrayFn(
    struct 
        open Array
        type elem = Temp.temp
        type vector = Temp.temp vector
        type array = Temp.temp array
    end) *)

  structure NodeKey : ORD_KEY =
    struct
      type ord_key = Graph.node
      val compare = Graph.compare
    end
  
  structure NodeSet = RedBlackSetFn(NodeKey)

  structure TempKey : ORD_KEY =
    struct
      type ord_key = Temp.temp
      val compare = Temp.compare
    end
  
  structure TempSet = RedBlackSetFn(TempKey)
  
  structure TempMap = RedBlackMapFn(TempKey)

  val precolored = MipsFrame.regs
  val precolored_set = TempSet.fromList precolored

  fun flatten xs = List.foldr op@ [] xs;

  (* structure NodeStack = Stack(NodeKey) *)

  fun rewriteProgram (instrs, spilledNodes, frame) = 
    let

      fun createTemps (v, (t :: tt), old, new) = 
        if Temp.eq (v, t) then 
        let
          val vi = Temp.newtemp()
          val newold = map (fn t => if Temp.eq (v, t) then vi else t) old
        in
          createTemps (v, tt, newold, vi :: new)
        end
        else createTemps (v, tt, old, new)
        | createTemps (v, [], old, new) = (old, new)

      fun insertStores (defs, prog, temp_mem_map, location) =
        let
          fun allocMem cur_map vi = 
            (MipsGen.codegen (frame) (Tree.MOVE(location, Tree.TEMP vi)), 
              TempMap.insert(cur_map, vi, location))
          val stores = flatten (map (fn vi => #1 (allocMem temp_mem_map vi)) defs)
          val new_map = List.foldl (fn (vi, cur_map) => #2 (allocMem cur_map vi)) temp_mem_map defs
        in
          (prog @ stores, new_map)
        end

      fun insertFetches (uses, prog, temp_mem_map, location) =
        let
          fun readMem vi = 
              MipsGen.codegen (frame) (Tree.MOVE(Tree.TEMP vi, location))
          val fetches = flatten (map readMem uses)
        in
          fetches @ prog
        end

      fun scanInstrs ((Assem.OPER {assem, dst, src, jump} :: t), v, temp_mem_map, newprog, newtemps) = 
        let
          val acc = MipsFrame.allocLocal(frame)(true)
          val location = MipsFrame.exp acc (Tree.TEMP MipsFrame.fp)
          val (new_src, new_uses) = createTemps (v, src, src, [])
          val (new_dst, new_defs) = createTemps (v, dst, dst, [])
          val (with_stores, new_map) = 
            insertStores (new_defs, [Assem.OPER {assem=assem, dst=new_dst, src=new_src, jump=jump}], temp_mem_map, location)
          val with_fetches = insertFetches (new_uses, with_stores, new_map, location)
        in 
          scanInstrs (t, v, new_map, newprog @ with_fetches, new_uses @ new_defs @ newtemps)
        end
        | scanInstrs ((Assem.MOVE {assem, dst, src} :: t), v, temp_mem_map, newprog, newtemps) = 
        let
          val acc = MipsFrame.allocLocal(frame)(true)
          val location = MipsFrame.exp acc (Tree.TEMP MipsFrame.fp)
          val (new_srcs, new_uses) = createTemps (v, [src], [src], [])
          val (new_dsts, new_defs) = createTemps (v, [dst], [dst], [])
          fun getNew t [] = t
            | getNew t (x :: xs) = x
          val new_dst = getNew dst new_dsts
          val new_src = getNew src new_srcs
          val (with_stores, new_map) = 
            insertStores (new_defs, [Assem.MOVE {assem=assem, dst=new_dst, src=new_src}], temp_mem_map, location)
          val with_fetches = insertFetches (new_uses, with_stores, new_map, location)
        in 
          scanInstrs (t, v, new_map, newprog @ with_fetches, new_uses @ new_defs @ newtemps)
        end
        | scanInstrs ((_ :: t), v, temp_mem_map, newprog, newtemps) = 
          scanInstrs (t, v, temp_mem_map, newprog, newtemps)
        | scanInstrs ([], v, temp_mem_map, new_prog, newtemps) = (new_prog, temp_mem_map, newtemps)

    in 
      #1 (TempSet.foldl (fn (v, (prog, table, temps)) => 
                      scanInstrs (instrs, v, table, prog, temps)) 
                    ([], TempMap.empty, []) spilledNodes)
    end
  
  fun alloc (instrs, frame) = 
    let

      val (interference, node2live) = 
        Liveness.interferenceGraph (MakeGraph.instrs2graph instrs)

      val (Liveness.IGRAPH{graph=graph, tnode=tnode, gtemp=gtemp, moves=moves}) = 
        interference

      val all_nodes = Graph.nodes graph

      fun isPrecolored n = List.exists (fn x => Temp.eq(x, n)) precolored

      val initial = List.filter (fn n => not (isPrecolored (gtemp n))) all_nodes

      fun printNodes [] = print "\n"
        | printNodes (h :: t) = (print ((Temp.makestring (gtemp h) ^ " ")); printNodes t)

      (* val _ = printNodes initial *)

      val simplifyWorklist = ref []
      val spillWorklist = ref []

      fun initMoveList (moves_tbl, []) = moves_tbl
        | initMoveList (moves_tbl, n :: nn) = 
          initMoveList (Graph.Table.enter 
                         (moves_tbl, n, ref NodeSet.empty), nn)
      
      val moveList = initMoveList (Graph.Table.empty, initial)

      fun findTempSet tbl n = 
        case Graph.Table.look (tbl, n) of
          NONE => ref TempSet.empty
        | SOME s => s

      fun findNodeSet tbl n = 
        case Graph.Table.look (tbl, n) of
          NONE => ref NodeSet.empty
        | SOME s => s

      fun makeMoveList [] = ()
        | makeMoveList ((n1, n2)::t) = 
          let
            val n1_moves = findNodeSet moveList n1
            val n2_moves = findNodeSet moveList n2
          in
            (n1_moves := NodeSet.add(!n1_moves, n2); 
             n2_moves := NodeSet.add(!n1_moves, n1);
             makeMoveList t)
          end
    
      val _ = makeMoveList moves
    
      (* Incomplete implementation *)
      fun nodeMoves n = 
        case Graph.Table.look (moveList, n) of
           NONE => NodeSet.empty
         | SOME s => !s
    
      fun moveRelated n = 
        not (NodeSet.isEmpty(nodeMoves n))

      val degrees = List.map (fn n => (n, ref (Graph.deg n))) initial

      fun findDegree n = 
        case List.find (fn (x, _) => Graph.eq (x, n)) degrees of
          NONE => ref 0
        | SOME (_, d) => d
        
      fun makeWorklist [] = ()
        | makeWorklist (n :: nn) = 
          let
            val degree = !(findDegree n)
          in
            if degree < k then 
            (simplifyWorklist := (n :: (!simplifyWorklist)); makeWorklist nn)
            else (spillWorklist := (n :: (!spillWorklist)); makeWorklist nn)
          end

      val selectStack = ref []

      fun getAlias n = n

      (* fun enableMoves nodes *)

      fun decrementDegree m = 
        if TempSet.exists (fn t => Temp.eq ((gtemp m), t)) precolored_set then () 
        else
          let
            val degree_ref = findDegree m
            val degree = !degree_ref
            (* val _ = if degree <> 0 then print ((Int.toString degree) ^ "\n") else () *)
          in
            (degree_ref := degree - 1; 
            if degree = k then 
              (
                spillWorklist := (List.filter (fn n => not (Graph.eq(n, m))) (!spillWorklist));
                (* print ("DD: " ^ (Temp.makestring (gtemp m)) ^ "\n"); *)
                simplifyWorklist := (m :: (!simplifyWorklist))
                )
            else ()
            (* if degree <> 0 then print ((Temp.makestring (gtemp m)) ^ " degree: " ^ (Int.toString degree) ^ "\n") else () *)
            )
          end

      fun adjacent n = 
        List.filter (fn x => 
                      not (List.exists (fn y => Graph.eq(x, y)) (!selectStack)))
                    (Graph.adj n)

      fun simplify() = 
        let
          val n = List.hd (!simplifyWorklist)
        in
          (simplifyWorklist := List.tl (!simplifyWorklist); 
           selectStack := (n :: (!selectStack));
           app decrementDegree (adjacent n))
        end
      
      fun spillCost (n : Graph.node) : int = 1

      fun selectSpill() = 
        let
          val m = List.hd (!spillWorklist)
        in
          (spillWorklist := (List.filter (fn n => not (Graph.eq(n, m))) (!spillWorklist));
          simplifyWorklist := (m :: (!simplifyWorklist)))
        end

      fun assignColors {interference: Liveness.igraph, 
                        initial: allocation,
                        spillCost: Graph.node -> int, 
                        registers: MipsFrame.register list} =
        let

          val okColors = ref []

          val nodes = Graph.nodes graph

          val n = ref (List.hd nodes)
          val n_temp = ref (Temp.newtemp())

          val coloredNodes = ref TempSet.empty
          val spilledNodes = ref TempSet.empty
          
          val color = ref initial

          exception UnknownTemp

          fun elimColors w = 
            let
              val w_alias = getAlias w
              val w_temp = gtemp w_alias
              val target = TempSet.union (!coloredNodes, precolored_set)
              val w_color = 
                case Temp.Table.look (!color, w_temp) of
                  NONE => "not assigned"
                | SOME c => c
            in
              if TempSet.exists (fn x => Temp.eq (w_temp, x)) target
              then 
                okColors := (List.filter (fn x => (not (MipsFrame.regEq (x, w_color)))) (!okColors)) 
              else ()
            end  
          
          val _ =
            while (not (List.null (!selectStack))) 
            do (n := List.hd (!selectStack);
                n_temp := gtemp (!n);
                if TempSet.exists (fn x => Temp.eq (!n_temp, x)) precolored_set then () else
                (selectStack := List.tl (!selectStack);
                okColors := registers;
                (* print "neighbors: ";
                printNodes (Graph.adj(!n)); *)
                app elimColors (Graph.adj(!n));
                (* print ("num neighbors: " ^ (Int.toString (List.length (Graph.adj(!n)))) ^ "\n"); *)
                (if !okColors = [] then 
                  (spilledNodes := (TempSet.add (!spilledNodes, !n_temp))) else 
                  (coloredNodes := (TempSet.add (!coloredNodes, !n_temp));
                   color := Temp.Table.enter (!color, !n_temp, List.hd (!okColors)))
                )))
        in
          (!color, !spilledNodes)
        end

      val _ = (makeWorklist initial;
              while (not (List.null (!simplifyWorklist) andalso (List.null (!spillWorklist))))
              do 
                (if not (List.null (!simplifyWorklist)) then simplify()
                  else if not (List.null (!spillWorklist)) then selectSpill()
                  else ()))

      val (color_alloc, spills) = 
         assignColors {interference=interference, 
                       initial=MipsFrame.tempMap,
                       spillCost=spillCost,
                       registers=MipsFrame.registers}

    in
      if TempSet.isEmpty spills then (instrs, color_alloc) else
        alloc (rewriteProgram(instrs, spills, frame), frame)
    end
  
end