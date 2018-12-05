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
    individualMinutesAsleep: array(int),
    minutesAsleep: int,
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

  let entryToSleepRecords: list(t) => list(sleepRecord) =
    xs => {
      let lastTs = ref(0);
      let lastGuardId = ref(0);
      let guardSleepTime = ref(0);
      let agg = ref([]);

      List.iter(
        x =>
          switch (x.action) {
          | BeginShift =>
            lastGuardId := x.guardId;
            lastTs := x.timestamp;
            guardSleepTime := 0;
          | FallAsleep => lastTs := x.timestamp
          | WakeUp =>
            let t0 = MomentRe.momentWithFormat(string_of_int(lastTs^), "X");
            let t1 =
              MomentRe.momentWithFormat(string_of_int(x.timestamp), "X");
            let month = MomentRe.Moment.month(t1);
            let day = MomentRe.Moment.day(t1);
            let minuteAsleepStart = MomentRe.Moment.minute(t0);
            let minuteAsleepEnd = MomentRe.Moment.minute(t1);
            let individualMinutesAsleep =
              Array.init(minuteAsleepEnd - minuteAsleepStart, i =>
                i + minuteAsleepStart
              );
            let minutesAsleep = minuteAsleepEnd - minuteAsleepStart;
            let sleepRecord = {
              guardId: lastGuardId^,
              month,
              day,
              individualMinutesAsleep,
              minutesAsleep,
            };

            agg := [sleepRecord, ...agg^];
            guardSleepTime := guardSleepTime^ + (x.timestamp - lastTs^) / 60;
            lastTs := x.timestamp;
          },
        xs,
      );

      agg^;
    };

  let indexSleepRecordsByGuard =
    List.fold_left(
      (acc, r) => {
        let rs = Belt.Map.Int.get(acc, r.guardId);
        switch (rs) {
        | None => Belt.Map.Int.set(acc, r.guardId, [r])
        | Some(x) => Belt.Map.Int.set(acc, r.guardId, List.append(x, [r]))
        };
      },
      Belt.Map.Int.empty,
    );
  let sleepRecordsOfGuardWithMostSleep =
    indexSleepRecordsByGuard
    ||> (
      Belt.Map.Int.toList
      ||> List.map(((guardId, rs)) =>
            (
              guardId,
              rs,
              List.fold_left((acc, r) => acc + r.minutesAsleep, 0, rs),
            )
          )
      ||> List.sort((a, b) => {
            let (_, _, aTime) = a;
            let (_, _, bTime) = b;
            bTime - aTime;
          })
    );
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
let sortSequencable = List.sort((a, b) => fst(a) - fst(b));
let aggregateOccurrencesReducer =
  Array.fold_left((acc, a) =>
    Belt.Map.Int.get(acc, a)
    |> (
      x =>
        switch (x) {
        | None => Belt.Map.Int.set(acc, a, 1)
        | Some(v) => Belt.Map.Int.set(acc, a, v + 1)
        }
    )
  );

let aggregateOcurrences = initial =>
  List.fold_left(aggregateOccurrencesReducer, initial)
  ||> Belt.Map.Int.toList
  ||> List.sort((a, b) => snd(b) - snd(a));

let bothPartsImpl =
  List.map(formatRawSequencable)
  ||> sortSequencable
  ||> Entry.processRawEntries
  ||> Entry.entryToSleepRecords;

let part1impl =
  bothPartsImpl
  ||> Entry.sleepRecordsOfGuardWithMostSleep
  ||> List.hd
  ||> (
    ((guardId, records, _)) => (
      guardId,
      List.map((r: Entry.sleepRecord) => r.individualMinutesAsleep, records)
      |> aggregateOcurrences(Belt.Map.Int.empty)
      |> List.hd
      |> fst,
    )
  )
  ||> (((a, b)) => a * b);

let part2impl =
  bothPartsImpl
  ||> Entry.indexSleepRecordsByGuard
  ||> Belt.Map.Int.toList
  ||> List.map(((guardId, rs)) =>
        List.map((r: Entry.sleepRecord) => r.individualMinutesAsleep, rs)
        |> aggregateOcurrences(Belt.Map.Int.empty)
        |> List.hd
        |> (((minute, amt)) => (guardId, minute, amt))
      )
  ||> List.sort((a, b) => {
        let (_, _, aAmt) = a;
        let (_, _, bAmt) = b;
        bAmt - aAmt;
      })
  ||> List.hd
  ||> (((a, b, _)) => a * b);

let solution =
  switch (inputLines) {
  | None => print_endline("Could not read input")
  | Some(lines) =>
    Js.log2("part 1", part1impl(lines));
    Js.log2("part 2", part2impl(lines));
  };