open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let input =
  dirname
  >>= Util.readInputLinesFromDir
  <$> List.hd
  <$> Js.String.split("")
  <$> Array.to_list;

let part1impl = Js.log;

let solution =
  switch (input) {
  | None => print_endline("Could not read input")
  | Some(lines) => Js.log2("part 1", part1impl(lines))
  /* Js.log2("part 2", part2impl(lines)); */
  };