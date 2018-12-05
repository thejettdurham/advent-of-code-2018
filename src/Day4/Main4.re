open Rationale.Option.Infix;
open Rationale.Function;

let dirname = [%bs.node __dirname];
let inputLines = dirname >>= Util.readInputLinesFromDir;

module Entry = {
  type action =
    | BeginShift
    | FallAsleep
    | WakeUp;
  type guardId = int;
  type t = {
    timestamp: int,
    guardId,
    action,
  };
  type sleepRecord = {
    guardId: int,
    month: int,
    day: int,
    minuteAsleep: int,
    minuteAwake: int,
    timeAsleep: int,
  };
  let processRawEntries: list((int, string)) => list(t) =
    l => {
      let guardIdRef = ref(0);
      List.map(
        ((timestamp, str)) =>
          switch (str) {
          | "falls asleep" => {
              timestamp,
              guardId: guardIdRef^,
              action: FallAsleep,
            }
          | "wakes up" => {timestamp, guardId: guardIdRef^, action: WakeUp}
          | _ =>
            let parts = Js.String.split(" ", str);
            let [|_, idPart, _, _|] = parts;
            guardIdRef :=
              Js.String.sliceToEnd(~from=1, idPart) |> int_of_string;

            {timestamp, guardId: guardIdRef^, action: BeginShift};
          },
        l,
      );
    };

  let findGuardSleepingMinutes: list(t) => list(sleepRecord) =
    xs => {
      let lastTs = ref(0);
      let lastGuardId = ref(0);
      let guardSleepTime = ref(0);
      let agg = ref([]);

      /* Update this to return sleep records */
      List.iter(
        x =>
          switch (x.action) {
          | BeginShift =>
            /* Assuming a guard always wakes up before the next's shift starts */
            agg := [(lastGuardId^, guardSleepTime^), ...agg^];

            lastGuardId := x.guardId;
            lastTs := x.timestamp;
            guardSleepTime := 0;
          | FallAsleep => lastTs := x.timestamp
          | WakeUp =>
            guardSleepTime := guardSleepTime^ + (x.timestamp - lastTs^) / 60;
            lastTs := x.timestamp;
          },
        xs,
      );

      List.sort(
        (a, b) => {
          let (_, aMin) = a;
          let (_, bMin) = b;
          bMin - aMin;
        },
        agg^,
      );
    };
};

let formatRawSequencable = l => {
  let [|dateR, action|] = Js.String.split("] ", l);
  let date =
    "2000-"
    ++ Js.String.sliceToEnd(~from=6, dateR)
    |> (
      s =>
        MomentRe.momentWithFormat(s, "YYYY-MM-DD HH:mm")
        |> MomentRe.Moment.toUnix
    );
  (date, action);
};
let sortSequencable =
  List.sort((a, b) => {
    let (aSeq, _) = a;
    let (bSeq, _) = b;

    aSeq - bSeq;
  });

let part1impl =
  List.map(formatRawSequencable)
  ||> sortSequencable
  ||> Entry.processRawEntries
  ||> Entry.findGuardSleepingMinutes
  ||> Array.of_list;
let part2impl = identity;

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) => Js.log2("part 1", part1impl(lines))
  /* Js.log2("part 2", part2impl(lines)); */
  };