open Graphs
open Etymology;
module SimpleLayout = Layout.Random(NodeId, EdgeId, ListGraph);
let graph = SimpleLayout.layout(bear_graph)

type t('a, 'b) = {
    graph: ListGraph.t(Space.with_pos('a), 'b),
    selectedNode: option(Node.t(NodeId.t, Space.with_pos('a)))
};

module Space = Space.MkSpace(NodeId, EdgeId, ListGraph);