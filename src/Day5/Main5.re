open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let input = dirname >>= Util.readInputLinesFromDir <$> List.hd;

let removeUnitFromPolymer = (u, p) =>
  Js.String.split("", p)
  |> Array.fold_left(
       (acc, xu) => u == Js.String.toLowerCase(xu) ? acc : acc ++ xu,
       "",
     );
let isEven = n => n mod 2 == 0;
let unitsReact = (a, b) =>
  !(a == b || Js.String.toLowerCase(a) != Js.String.toLowerCase(b));
let reactPolymer = p => {
  open Js.String;
  let pUnits = ref(split("", p));
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
        i := i^ - 1;
        i := i^ < 0 ? 0 : i^;
        j := j^ - 1;
        j := j^ < 1 ? 1 : j^;
      } :
      {
        i := i^ + 1;
        j := j^ + 1;
      };
  };

  Array.fold_left(concat, "", pUnits^);
};

let units = "abcdefghijklmnopqrstuvwxyz";
let part1impl = reactPolymer ||> Js.String.length;
let part2impl = l =>
  Array.fold_left(
    (acc, u) => {
      let p = removeUnitFromPolymer(u, l);
      Js.log2("part 2: running without", u);
      let len = part1impl(p);
      len < acc ? len : acc;
    },
    Js.String.length(l),
    Js.String.split("", units),
  );

let solution =
  switch (input) {
  | None => print_endline("Could not read input")
  | Some(input) =>
    Js.log2("part 1", part1impl(input));
    Js.log2("part 2", part2impl(input));
  };