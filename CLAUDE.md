# Election Results Model

A candidate-skew model of Orange County election results, 2016–2026, written in R.

Read `PLAN.md` before starting work. It states the goal, the data and its file layouts, the model specification, the code layout, the documented rules (candidate links, designated nonpartisan races, fit flags, statewide propositions, secondary elections), the results of each run, and the next steps. When a change alters a rule or a result that `PLAN.md` states, update `PLAN.md` in the same change.

- Run: `Rscript run.R` (usage at the top of `run.R`). Write the console log of long runs to a folder outside OneDrive, which has truncated logs written inside the project folder.
- Test: `Rscript tests/run_tests.R`.
- Paths and constants are in `R/config.R`; the election registry is in `R/elections.R`.
- Raw data in `data/` is kept out of git; `PLAN.md` says where to download it.
- Every change gets a `CHANGELOG.md` entry that says whether it can move a number in an existing result.
