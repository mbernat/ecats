open Graphs
open Space

type t = {
    graph: listGraph,
    selectedNode: option(Node.t(NodeId.t, with_pos(unit)))
};