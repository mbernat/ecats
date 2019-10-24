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
module EdgeId = MkIntId()
module ListGraph = MkListGraph(StringId, EdgeId)

let make_bear = ((w, l)) => Graphs.Node.{
    id: StringId.id_of_string(w),
    data: {
        word: w,
        language: l,
        meaning: ["bear"]
    }
}

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
]

type etymology_graph = ListGraph.t(word_info, unit);

let add_word = (g, (w, l)) => ListGraph.add_node(make_bear((w, l)), g)
let pre_bear_graph: etymology_graph = List.fold_left(add_word, ListGraph.empty, pre_words);

let add_edge = (g, (c, p)) => {
    let s = StringId.id_of_string(c);
    let t = StringId.id_of_string(p);
    let id = EdgeId.id_of_string("");
    ListGraph.add_edge({id: id, source: s, target: t, data: ()}, g)
}
let bear_graph = List.fold_left(add_edge, pre_bear_graph, child_parent_list)