structure Color :> COLOR =
struct

  type allocation = MipsFrame.register Temp.Table.table

  structure NodeKey : ORD_KEY =
    struct
      type ord_key = Graph.node
      val compare = Graph.compare
    end
  
  structure NodeSet = RedBlackSetFn(NodeKey)

  structure NodeStack = Stack(NodeKey)

  structure TempKey : ORD_KEY =
    struct
      type ord_key = Temp.temp
      val compare = Temp.compare
    end
  
  structure TempSet = RedBlackSetFn(TempKey)

  val precolored = MipsFrame.regs

  fun color: {interference: Liveness.igraph, 
              initial: allocation,
              spillCost: Graph.node -> int, 
              registers: MipsFrame.register list} =
    let
      val {graph, tnode, gtemp, moves} = interference

      val okColors = ref []

      val nodes = Graph.nodes = graph

      val n = ref (List.hd nodes)

      val coloredNodes = ref TempSet.empty
      val precolored_set = TempSet.fromList precolored
      val color = ref initial

      exception UnknownTemp

      fun elimColors w = 
        let
          val w_alias = RegAlloc.getAlias w
          val w_temp = gtemp w_alias
          val target = TempSet.union (coloredNodes, precolored_set)
          val w_color = 
            case Temp.Table.look (!color, w_temp) of
               NONE => raise UnknownTemp
             | SOME c => c
        in
          if TempSet.exists (fn x => Temp.eq (w_temp, x)) target
          then 
            okColors := (List.filter (fn x => (not Temp.eq (x, w_color))) !okColors) 
          else ()
        end
        
      val _ =
        while (not NodeStack.isEmpty (!(RegAlloc.selectStack))) 
        do (n := NodeStack.top (!(RegAlloc.selectStack));
            RegAlloc.selectStack := NodeStack.pop (!(RegAlloc.selectStack));
            okColors := registers;
            app elimColors Graph.adj(!n);
            (if !okColors = [] then () else 
              coloredNodes := (TempSet.add (!coloredNodes, !n));
              color := Temp.Table.enter (!color, !n, List.hd (!okColors))))
    in
      (!color, [])
    end
end