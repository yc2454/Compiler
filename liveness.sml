structure Liveness: 
sig
    structure IGraph : 
    sig
        type graph = Graph.graph
        type node = Graph.node
    end

    datatype igraph =
        IGRAPH of {graph: IGraph.graph,
                   tnode: Temp.temp -> IGraph.node,
                   gtemp: IGraph.node -> Temp.temp,
                   moves: (IGraph.node * IGraph.node) list}

    val interferenceGraph : Flow.flowgraph * Graph.node list ->
                            igraph * (Graph.node -> Temp.temp list)

    (* val show : TextIO.outstream * igraph -> unit *)

end = 
struct

  structure IGraph = 
    struct
      type graph = Graph.graph
      type node = Graph.node
    end

  structure TempKey : ORD_KEY =
    struct
      type ord_key = Temp.temp
      val compare = Temp.compare
    end
  
  structure TempSet = RedBlackSetFn(TempKey)
  
  datatype igraph =
    IGRAPH of {graph: IGraph.graph,
               tnode: Temp.temp -> IGraph.node,
               gtemp: IGraph.node -> Temp.temp,
               moves: (IGraph.node * IGraph.node) list}
  
  
  type liveSet = unit Temp.Table.table * Temp.temp list 
  type liveMap = liveSet Graph.Table.table

  fun interferenceGraph ((Flow.FGRAPH({control, use=u_table, def=d_table, ismove=i_table}) : Flow.flowgraph), instrs)= 
    let
      
      fun initialize (live_in, live_out, []) = (live_in, live_out)
        | initialize (live_in, live_out, n :: nn) = 
          initialize (Graph.Table.enter (live_in, n, ref TempSet.empty), 
                      Graph.Table.enter (live_out, n, ref TempSet.empty), nn)

      val (in_table, out_table) = 
        initialize (Graph.Table.empty, Graph.Table.empty, instrs)
      
      val ((in_table'), out_table') = 
        initialize (Graph.Table.empty, Graph.Table.empty, instrs)

      fun buildIn (use, out_set, def) = 
        TempSet.union (use, TempSet.difference (out_set, def))

      exception UnknownNode
      
      fun findSet tbl n = 
        case Graph.Table.look (tbl, n) of
          NONE => raise UnknownNode
        | SOME s => s

      fun findList tbl n = 
        case Graph.Table.look (tbl, n) of
          NONE => []
        | SOME l => l

      fun buildOut ([], acc) = acc
        | buildOut (s::ss, acc) = 
        let
          val in_set = findSet in_table s
        in
          buildOut (ss, TempSet.union (acc, !in_set))
        end
      
      fun checkTableChange (old, new, []) = true
        | checkTableChange (old, new, n::nn) = 
        let
          val old_set = findSet old n
          val new_set = findSet new n
        in
          TempSet.equal (!old_set, !new_set) andalso 
          checkTableChange (old, new, nn)
        end

      fun printTemp t = print ((MipsFrame.printTemp t) ^ " ")
      
      fun update [] = ()
        (* print "-----------------------------\n" *)
        | update (n::nn) =
          let
            val in_n = findSet in_table n
            val in_n' = findSet in_table' n
            val out_n = findSet out_table n
            val out_n' = findSet out_table' n
            val use_n = TempSet.fromList(findList u_table n)
            val def_n = TempSet.fromList(findList d_table n)
            (* val _ = (print "use: "; TempSet.app printTemp use_n; print "   ";
                      print "def: "; TempSet.app printTemp def_n; print "   ") *)
            val succ = Graph.succ n
            (* val _ = print ("num of succ: " ^ (Int.toString (List.length succ))) *)
          in
            (in_n' := !in_n; out_n' := !out_n;
             in_n := buildIn (use_n, !out_n, def_n);
             out_n := buildOut (succ, TempSet.empty);
             (* print "   in: "; TempSet.app printTemp (!in_n); print "  ";
             print "out: "; TempSet.app printTemp (!out_n); print "\n"; *)
             update nn)
          end
      
      fun check () = 
        checkTableChange (in_table, in_table', instrs) andalso 
        checkTableChange (out_table, out_table', instrs)

      val _ = 
        (update instrs; 
         while (not (check ())) do 
            (update instrs))

      val itf_graph = Graph.newGraph()

      fun temp2nodes (n, [], acc) = acc
        | temp2nodes (n, t::tt, acc) = 
        case List.find (fn (it, d) => Temp.eq(d, t)) acc of
           NONE => temp2nodes (n, tt, (((Graph.newNode itf_graph), t) :: acc))
         | SOME _ => temp2nodes (n, tt, acc)
      
      fun findMove n = 
        case Graph.Table.look (i_table, n) of 
          NONE => false
        | SOME b => b
      
      fun addNodes ([], acc) = acc
        | addNodes (n::nn, acc) = 
        let
          val def_n = findList d_table n
          val use_n = findList u_table n
        in
          addNodes (nn, (temp2nodes (n, def_n, temp2nodes (n, use_n, acc))))
        end
      
      val temp_node_list : (Graph.node * Temp.temp) list = 
          addNodes (instrs, [])

      fun tnode t = 
        case List.find (fn (it, d) => Temp.eq(d, t)) temp_node_list of
           NONE => (print ("cannot find node for temp: " ^ (Temp.makestring t)); raise UnknownNode)
         | SOME (it, _) => it

      fun addEdge n t = 
        let
          val m = tnode t
        in
          if Graph.eq (n, m) then () else Graph.mk_edge {from=n, to=m}
        end
        
      fun buildIGraph [] = ()
        | buildIGraph (i :: ii) = 
          let
            val defs = findList d_table i
            val outs = findSet out_table i
            fun connectLive d = TempSet.app (addEdge (tnode d)) (!outs)
          in
            (app connectLive defs; buildIGraph ii)
          end

      (* fun buildIGraph [] = ()
        | buildIGraph (i :: ii) = 
          let
            val defs = findList d_table i
            val outs = findSet out_table i
          in
            case defs of
               [] => buildIGraph ii
             | d :: dd => (TempSet.app (addEdge (tnode d)) (!outs); buildIGraph ii)
          end *)

      val _ = buildIGraph instrs
      
      fun buildMoveList [] acc = acc
        | buildMoveList (n::nn) acc = 
          if findMove n then 
          buildMoveList nn 
            ((tnode (List.hd (findList u_table n)), 
              tnode (List.hd (findList d_table n))) :: acc)
          else buildMoveList nn acc

      val moves = buildMoveList instrs []

      fun gtemp n = 
        case List.find (fn (it, d) => Graph.eq(it, n)) temp_node_list of
           NONE => raise UnknownNode
         | SOME (_, d) => d
      
      fun mapping n = 
        TempSet.toList (!(findSet out_table n))
    in
      (IGRAPH {graph=itf_graph, tnode=tnode, gtemp=gtemp, moves=moves}, 
        mapping)
    end

end