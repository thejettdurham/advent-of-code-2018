open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let inputLines = dirname >>= Util.readInputLinesFromDir;

/* TODO implement Parser */
let parseInstruction: string => list((int, int)) = x => [(0, 0)];

/* Use pairs from parse as Map keys, and values are the number of claims for that coordinate */
/* part1 = count of map values > 1 */

let part1impl = x => {};
let part2impl = x => {};

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) =>
    Js.log2("part 1", part1impl(lines));
    Js.log2("part 2", part2impl(lines));
  };