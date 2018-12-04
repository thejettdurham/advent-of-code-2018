open Rationale.Option.Infix;
open Rationale.Function;

let readFileAsString = f =>
  switch (Node.Fs.readFileAsUtf8Sync(f)) {
  | item => Some(item)
  | exception _ => None
  };

let readLinesFromFile = f => readFileAsString(f) <$> Js.String.split("\n");
let readInputLinesFromDir = dir =>
  readLinesFromFile(dir ++ "/input.txt") <$> Array.to_list;

let pairOfStringArray = ([|a, b|]) => (
  int_of_string(a),
  int_of_string(b),
);
let stringOfPair = ((a, b)) => string_of_int(a) ++ "," ++ string_of_int(b);
let pairOfString = Js.String.split(",") ||> pairOfStringArray;

module PairComparator =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = ((a0, a1), (b0, b1)) =>
      switch (compare(a0, b0)) {
      | 0 => compare(a1, b1)
      | c => c
      };
  });