open Rationale.Function;
open Rationale.Option.Infix;

let dirname = [%bs.node __dirname];
let maybeLines = dirname >>= Util.readInputLinesFromDir;

switch (maybeLines) {
| None => print_endline("Could not read input")
| Some(lines) => Js.log(lines)
};