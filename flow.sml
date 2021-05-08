signature FLOW =
sig
    (* structure Graph : GRAPH *)
    datatype flowgraph = 
      FGRAPH of {control : Graph.graph,
                 def : Temp.temp list Graph.Table.table,
                 use : Temp.temp list Graph.Table.table,
                 ismove : bool Graph.Table.table}
end

structure Flow :> FLOW =
struct
  datatype flowgraph = 
      FGRAPH of {control : Graph.graph,
                 def : Temp.temp list Graph.Table.table,
                 use : Temp.temp list Graph.Table.table,
                 ismove : bool Graph.Table.table}
end