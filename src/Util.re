open Rationale.Option.Infix;

let readFileAsString = f =>
  switch (Node.Fs.readFileAsUtf8Sync(f)) {
  | item => Some(item)
  | exception _ => None
  };

let readLinesFromFile = f => readFileAsString(f) <$> Js.String.split("\n");
let readInputLinesFromDir = dir => readLinesFromFile(dir ++ "/input.txt");