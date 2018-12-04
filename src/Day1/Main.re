open Rationale.Option.Infix;

let dirname = [%bs.node __dirname];
let inputLines = dirname >>= Util.readInputLinesFromDir;
let startingFreq = 0;

let instructionReducer = (acc, str) => {
  let op = Js.String.slice(~from=0, ~to_=1, str);
  let v = Js.String.sliceToEnd(~from=1, str)->int_of_string;
  switch (op) {
  | "+" => acc + v
  | "-" => acc - v
  | _ => acc
  };
};

let part1impl = List.fold_left(instructionReducer, 0);

/* Not idiomatic FP, but it's nice that Reason gives you an escape hatch for imperative programming */
let part2impl = (lines: array(Js.String.t)) => {
  let firstMatch = ref(None);
  let lineIdx = ref(0);
  let acc = ref(startingFreq);
  let visitedValues = ref(Belt.Set.Int.fromArray([|startingFreq|]));
  let numLines = Array.length(lines);

  while (firstMatch^ == None) {
    let line = lines[lineIdx^];
    acc := instructionReducer(acc^, line);
    if (Belt.Set.Int.has(visitedValues^, acc^)) {
      firstMatch := Some(acc^);
    } else {
      visitedValues := Belt.Set.Int.add(visitedValues^, acc^);
      lineIdx := succ(lineIdx^) mod numLines;
    };
  };

  Rationale.Option.toExn("couldn't find any match", firstMatch^);
};

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) =>
    Js.log2("part 1", part1impl(lines));
    Js.log2("part 2", part2impl(Array.of_list(lines)));
  };