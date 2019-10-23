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

let makeBear = ((w, l)) => {word: w, language: l, meaning: ["bear"]}

let preWords = [
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

let childParentList = [
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

open Graphs
module StringGraph = ListBased(StringId, StringId)
type etymologyGraph = StringGraph.t(word_info, unit);

let addWord = (g, (w, l)) => StringGraph.addNode(StringId.id_of_string(w), {data: makeBear((w, l))}, g)
let preBearGraph: etymologyGraph = List.fold_left(addWord, StringGraph.empty, preWords);

let addEdge = (g, (c, p)) => {
    open StringId;
    let s = id_of_string(c);
    let t = id_of_string(p);
    StringGraph.addEdge(s, {source: s, target: t, data: ()}, g)
}
let bearGraph = List.fold_left(addEdge, preBearGraph, childParentList)