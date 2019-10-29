open Graphs
open Etymology;

module L = Layout.WithGraph(NodeId, EdgeId, ListGraph);
let root = word_to_id("h₂ŕ̥tḱos");
let graph = L.FromRoot.layout(L.FR.{root: root}, bear_graph)

type t('a, 'b) = {
    graph: ListGraph.t(Space.with_pos('a), 'b),
    selectedNode: option(Node.t(NodeId.t, Space.with_pos('a)))
};

module Space = Space.MkSpace(NodeId, EdgeId, ListGraph);