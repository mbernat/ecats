let style = Revery.UI.Style.[
    position(`Absolute),
    left(10),
    bottom(10),
    width(200)
];

let string_of_time = tm => {
    open Unix;
    let h = string_of_int(tm.tm_hour);
    let m = string_of_int(tm.tm_min);
    let s = string_of_int(tm.tm_sec);
    String.concat(":", [h, m, s]);
}