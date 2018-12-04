open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let inputLines = dirname >>= Util.readInputLinesFromDir;

let makeIdentifier =
  Js.String.split("")
  ||> Array.fold_left(
        (acc, c) =>
          Rationale.Dict.has(c, acc) ?
            Rationale.Dict.evolve([(c, succ)], acc) :
            Rationale.Dict.set(c, 1, acc),
        [],
      );

let identifierHasTwoOfLetter =
  Rationale.RList.any(((_char, count)) => count == 2);
let identifierHasThreeOfLetter =
  Rationale.RList.any(((_char, count)) => count == 3);

let part1impl =
  List.fold_left(
    (acc, s) => {
      let (l, r) = acc;
      let identifier = makeIdentifier(s);
      (
        identifierHasTwoOfLetter(identifier) ? succ(l) : l,
        identifierHasThreeOfLetter(identifier) ? succ(r) : r,
      );
    },
    (0, 0),
  )
  ||> (x => fst(x) * snd(x));

let diffStrings = (a, b) => {
  let ma = Js.String.split("", a)->Array.to_list;
  let mb = Js.String.split("", b)->Array.to_list;
  List.fold_left2(
    (acc, x, y) =>
      x != y ? (fst(acc) + 1, snd(acc)) : (fst(acc), [x, ...snd(acc)]),
    (0, []),
    ma,
    mb,
  );
};

let part2impl = lines =>
  List.fold_left(
    (foundMatch1, a) =>
      foundMatch1 ?
        foundMatch1 :
        List.fold_left(
          (foundMatch2, b) =>
            if (foundMatch2 || a == b) {
              foundMatch2;
            } else {
              let diff = diffStrings(a, b);
              fst(diff) == 1 ?
                {
                  let fmtChars = List.fold_left(Js.String.concat, "");
                  Js.log(fmtChars(snd(diff)));
                  true;
                } :
                false;
            },
          false,
          lines,
        ),
    false,
    lines,
  );

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) =>
    Js.log2("part 1", part1impl(lines));
    Js.log2("part 2", part2impl(lines));
  };