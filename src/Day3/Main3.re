open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let inputLines = dirname >>= Util.readInputLinesFromDir;

let logMe = x => {
  Js.log(x);
  x;
};
let parseRawId = Js.String.sliceToEnd(~from=1) ||> int_of_string;
let parseRawCoord =
  Js.String.slice(~from=0, ~to_=-1)
  ||> Js.String.split(",")
  ||> Util.pairOfStringArray;
let parseRawSize = Js.String.split("x") ||> Util.pairOfStringArray;

let makePoints = (~size, ~startingCoord) => {
  let (width, height) = size;
  let (x, y) = startingCoord;
  let widthRange = Array.init(width, identity);
  let heightRange = Array.init(height, identity);
  Array.fold_left(
    (acc, w) => {
      let coords =
        Array.map(h => (x + w, y + h), heightRange)->Array.to_list;
      List.append(acc, coords);
    },
    [],
    widthRange,
  );
};

module Instruction = {
  type t = {
    id: int,
    points: list((int, int)),
  };
  let parse: string => t =
    x => {
      let parts = Js.String.split(" ", x)->Array.to_list;
      let [idRaw, _, startCoordRaw, sizeRaw] = parts;
      let id = parseRawId(idRaw);
      let startCoord = parseRawCoord(startCoordRaw);
      let size = parseRawSize(sizeRaw);
      let points = makePoints(size, startCoord);

      {id, points};
    };
  let pointsMatchForInstruction = (points: list((int, int)), i: t) => {
    let givenPoints =
      Belt.Set.fromArray(
        Array.of_list(points),
        ~id=(module Util.PairComparator),
      );
    let iPoints =
      Belt.Set.fromArray(
        Array.of_list(i.points),
        ~id=(module Util.PairComparator),
      );

    Belt.Set.eq(givenPoints, iPoints);
  };
};

module ClaimSpec = {
  type t = {
    id: int,
    startCoord: (int, int),
    size: (int, int),
  };
  let parse: string => t =
    x => {
      let parts = Js.String.split(" ", x)->Array.to_list;
      let [idRaw, _, startCoordRaw, sizeRaw] = parts;
      let id = parseRawId(idRaw);
      let startCoord = parseRawCoord(startCoordRaw);
      let size = parseRawSize(sizeRaw);

      {id, startCoord, size};
    };
  /* Make point set out of each & compare */
  let claimsIntersect: (t, t) => bool =
    (a, b) => {
      let aPoints =
        Belt.Set.fromArray(
          Array.of_list(makePoints(a.size, a.startCoord)),
          ~id=(module Util.PairComparator),
        );
      let bPoints =
        Belt.Set.fromArray(
          Array.of_list(makePoints(b.size, b.startCoord)),
          ~id=(module Util.PairComparator),
        );

      Belt.Set.intersect(aPoints, bPoints) |> Belt.Set.size |> (x => x != 0);
    };
};

module Grid = {
  let init = Belt.Map.Dict.empty;
  let addIdToKey = (k, id, d) => {
    let vals =
      Belt.Map.Dict.getWithDefault(d, k, [], ~cmp=Util.PairComparator.cmp);
    Belt.Map.Dict.set(d, k, [id, ...vals], ~cmp=Util.PairComparator.cmp);
  };
  let countKeysWithValuePred = (w, d) =>
    Belt.Map.Dict.reduce(d, 0, (acc, _, v) => w(v) ? succ(acc) : acc);
  let foldLeft = (r, i, d) => Belt.Map.Dict.reduce(d, i, r);
};

let calculateClaims = {
  let initialGrid = Grid.init;

  List.fold_left(
    (gridO, line) => {
      let instruction = Instruction.parse(line);

      List.fold_left(
        (acc, p) => Grid.addIdToKey(p, instruction.id, acc),
        gridO,
        instruction.points,
      );
    },
    initialGrid,
  );
};

let part1impl =
  calculateClaims ||> Grid.countKeysWithValuePred(x => List.length(x) > 1);
let part2impl = lines =>
  List.fold_left(
    (accO, lO) =>
      if (accO != 0) {
        accO;
      } else {
        let claim = ClaimSpec.parse(lO);

        let hasIntersect =
          List.fold_left(
            (accI, lI) =>
              if (accI || lO == lI) {
                accI;
              } else {
                /* Js.log3(lO, lI, lO == lI); */
                let testClaim = ClaimSpec.parse(lI);
                ClaimSpec.claimsIntersect(claim, testClaim);
              },
            false,
            lines,
          );

        hasIntersect ? 0 : claim.id;
      },
    0,
    lines,
  );

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) =>
    Js.log2("part 1", part1impl(lines));
    Js.log2("part 2", part2impl(lines));
  };