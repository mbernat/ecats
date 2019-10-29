/*

here we'd like to describe word etymologies
when done properly, a word data structure should carry a lot of information, essentialy the worth of a Wiktionary page

It's not clear how to model this well, since words are so fluid.
Perhaps it'd be best to have an underlying dictionary that's just strings and built the information structure
on top of that.

*/

type word_info = {
    word: string,
    language: string,
    meaning: list(string)
}

open Graphs

let pre_words = [
    ("h₂ŕ̥tḱos", "Proto-Indo-European"),
    ("ari", "Albanian"),
    ("Hŕ̥tḱos", "Proto-Anatolian"),
    ("ḫartaggaš", "Hittite"),
    ("arǰ", "Armenian"),
    ("artos", "Proto-Celtic"),
    ("arth", "Welsh"),
    ("árktos", "Ancient Greek"),
    ("Hŕ̥ćšas", "Proto-Indo-Iranian"),
    ("Hŕ̥ṭṣas", "Proto-Indo-Aryan"),
    ("rīch", "Hindi"),
    ("Hŕ̥šah", "Proto-Iranian"),
    ("xers", "Persian")
];

module Ids = {
    let ids = List.map((((a, _)) => a), pre_words)
}
module NodeId = MkIdFromList(Ids, ());
module EdgeId = MkIntId ()
module ListGraph = MkListGraph(NodeId, EdgeId)

let word_id_list = List.map(w => (w, NodeId.allocate()), Ids.ids);
let word_to_id = w => List.find(((w', _)) => w == w', word_id_list) |> Util.snd;

let make_bear = ((w, l)) => Graphs.Node.{
    id: word_to_id(w),
    data: ()/*{
        word: w,
        language: l,
        meaning: ["bear"]
    }*/
}



let child_parent_list = [
    ("h₂ŕ̥tḱos", "h₂ŕ̥tḱos"),
    ("ari", "h₂ŕ̥tḱos"),
    ("Hŕ̥tḱos", "h₂ŕ̥tḱos"),
    ("ḫartaggaš", "Hŕ̥tḱos"),
    ("arǰ", "h₂ŕ̥tḱos"),
    ("artos", "h₂ŕ̥tḱos"),
    ("arth", "artos"),
    ("árktos", "h₂ŕ̥tḱos"),
    ("Hŕ̥ćšas", "h₂ŕ̥tḱos"),
    ("Hŕ̥ṭṣas", "Hŕ̥ćšas"),
    ("rīch", "Hŕ̥ṭṣas"),
    ("Hŕ̥šah", "Hŕ̥ćšas"),
    ("xers", "Hŕ̥šah")
];

let add_word = (g, (w, l)) => ListGraph.add_node(make_bear((w, l)), g)
let pre_bear_graph = List.fold_left(add_word, ListGraph.empty, pre_words);

let add_edge = (g, (c, p)) => {
    let s = word_to_id(c);
    let t = word_to_id(p);
    let id = EdgeId.allocate();
    ListGraph.add_edge({id: id, source: s, target: t, data: ()}, g)
}
let bear_graph = List.fold_left(add_edge, pre_bear_graph, child_parent_list)