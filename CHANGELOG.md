# Changelog

Each entry says whether a change can move a number in an existing result.

## 2026-11-12 (documentation)

**No numbers move.** A project `CLAUDE.md` points to `PLAN.md` as the source of background and lists how to run, test, and log changes.

## 2026-11-10 (documentation)

**No numbers move.** PLAN.md gains a "Fit flags" section stating the poor-fit and vote-split rules in full: the two regressions, the thresholds and how they were chosen, where each kind of race gets its prediction, and the limits. The comment on `fit_flag()` in `R/diagnostics.R` now matches it.

## 2026-11-10

**No numbers move** in the general-election results: `results.csv`, `candidate_effects.csv`, `model_summary.csv`, and `leave_one_contest_out.csv` for 2024, 2022–2024, and 2016–2024 are byte-for-byte identical to those of the first commit. New results are added (below).

- Code reorganized into modules under `R/` (loaded by `R/setup.R`), with paths and constants in `R/config.R` and the election registry in `R/elections.R`. The leave-one-contest-out refits run in parallel.
- Tests of the documented rules: `Rscript tests/run_tests.R` (44 tests, base R, made-up inputs). Each test was checked by breaking the code it covers.
- Data files moved to `data/` and named `<date>_<type>.<csv|txt>` after their actual format; tab-separated files now end in `.txt`.
- Eleven secondary elections (statewide primaries 2014–2026, the 2015 and 2025 State Senate special primaries, the November 2019 district election, the November 2025 statewide special) and the 2019 and 2021 supervisor vacancy elections downloaded and registered with `role = "secondary"`; layouts added for the party-split 2020 and 2024 presidential primaries.
- Second stage (`Rscript run.R --secondary`, `R/secondary.R`): race skews of secondary elections against the precinct leans of the main fit, with a shift and a slope per election. New outputs `secondary_skews.csv` and `secondary_summary.csv`.
- Candidate links confirmed by hand carry over to new rows with the same ballot name; Catherine Blakespear linked across 2022 and 2026.
- Every contested county supervisor race of 2016–2026 listed in `nonpartisan.csv`; 19 designated D and R. Designated races held in secondary elections are estimated with the second stage. `nonpartisan_skews.csv` gains `candidates`, `designated_vote_share`, `vote_split`, and `lean_source`. The skews of the three races designated before this change are unchanged.
- Every statewide proposition tested automatically as a two-choice race, with the D side chosen from the data (`R/measures.R`). New output `measure_skews.csv`.
- Fixed: elections of different layouts are combined by column name (`read_elections()`); before, combining a party-split layout with another would have misaligned columns. No earlier run combined them, so no earlier result was affected.
