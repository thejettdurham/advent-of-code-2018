open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let input =
  dirname >>= Util.readInputLinesFromDir <$> List.hd <$> Util.charArrayOfString;

let removeUnitFromPolymer = (u, p) =>
  Array.fold_left(
    (acc, xu) => u == Char.lowercase(xu) ? acc : Array.append(acc, [|xu|]),
    [||],
    p,
  );

let isEven = n => n mod 2 == 0;
let unitsReact = (a, b) =>
  !(a == b || Char.lowercase(a) != Char.lowercase(b));

let reactPolymer = p => {
  let pUnits = ref(Array.copy(p));
  let i = ref(0);
  let j = ref(1);

  while (j^ < Array.length(pUnits^)) {
    let uL = pUnits^[i^];
    let uR = pUnits^[j^];

    unitsReact(uL, uR) ?
      {
        let left = Array.sub(pUnits^, 0, i^);
        let right =
          Array.sub(pUnits^, j^ + 1, Array.length(pUnits^) - (1 + j^));

        pUnits := Array.append(left, right);
        i := i^ - (i^ == 0 ? 0 : 1);
        j := j^ - (j^ == 1 ? 0 : 1);
      } :
      {
        i := i^ + 1;
        j := j^ + 1;
      };
  };

  pUnits^;
};

let units = "abcdefghijklmnopqrstuvwxyz" |> Util.charArrayOfString;
let part1impl = reactPolymer ||> Array.length;
let part2impl = l =>
  Array.fold_left(
    (acc, u) => {
      let p = removeUnitFromPolymer(u, l);
      Js.log2("part 2: running without", u);
      let len = part1impl(p);
      len < acc ? len : acc;
    },
    Array.length(l),
    units,
  );

let solution =
  switch (input) {
  | None => print_endline("Could not read input")
  | Some(input) =>
    Js.log2("part 1", part1impl(input));
    Js.log2("part 2", part2impl(input));
  };