# Election Results Model: findings and plan

Findings and plan for the Orange County skew model, begun in a claude.ai chat and continued in Claude Code on November 9, 2026.

**Status (November 9, 2026):** the model is fitted to the November general elections of 2016–2024 with `Rscript run.R` (about 9 minutes). 2012 and 2014 wait for hand-entered party labels. The next milestone is the November 3, 2026 election; see "Adding the 2026 election".

## Goal

Fit a mixed-effects model of how far each candidate skews D or R relative to their party, using Orange County precinct-level results from November general elections 2012–2024. The candidate effects are the point: they show which candidates over- or under-performed their party. The precinct effects are incidental.

## Data

The data are the plain-text extracts (`media.zip`) from OC Vote's archived statements of votes: https://ocvote.gov/data/election-results-archives/archived-statement-of-votes

The files are kept in `data/`, named `<election date>_<type>.<csv|txt>` after their actual format (for example `data/2024-11-05_general.txt`), and registered in `R/elections.R`. The seven November general elections of 2012–2024 parse cleanly, and the parsed row counts match the line counts. Eleven secondary elections were downloaded on November 10, 2026 (see "Secondary elections").

### File layouts

| Years | How to read | Vote columns | Notes |
|---|---|---|---|
| 2012–2018 | Comma-separated, UTF-8 encoding (accented names such as `SÁNCHEZ` are UTF-8; reading them as Latin-1 garbles them) | `Absentee_votes`, `Early_votes`, `Election_Votes` | Precinct IDs have 7 digits in 2012 and 2014 (`0002001`) and 5 digits from 2016 on (`02001`). Candidate rows have `Candidate_Type` `C`; write-ins have `W`. |
| 2020 | Comma-separated, skip the first line (`#FormatVersion 1`), Latin-1 encoding; write-ins have choice ID 0 | `Total Votes`, plus columns for Early Vote Center, Election Day Vote Center, and Vote by Mail; overvotes and undervotes | First header is `#Precinct`. The `Precinct Reporting` value has trailing spaces. |
| 2022–2024 | **Tab-separated** (`.txt`, as the county publishes them) | `Total Votes`, plus columns for Vote Center and Vote by Mail; overvotes and undervotes | First header is `.Precinct`. In 2024 the candidate column is named `Choice Name1`, not `Choice Name`. Plain ASCII: accented letters are printed as a hyphen (`LINDA T. S-NCHEZ`). Write-ins have choice ID 0. |

`data.table::fread` leaves the doubled quotes of quoted CSV fields undecoded (`BERNARD ""BERNIE"" SANDERS`); the loader restores them, and `candidates.csv` is read with base R for the same reason.

The 2020 and 2024 presidential primaries split registration and turnout by party (`DEM Voters`, `DEM Ballots Cast`, and so on for seven groups); their layouts (`csv_2020_party_split`, `tab_2024_party_split`) take registered voters and ballots as the sums. In the 2024 presidential primary, the party presidential candidates carry choice ID 0, the write-in marker of that layout; those contests are party ballots and never enter the model. In the 2016 presidential primary, registered voters and ballots differ between contests of one precinct (each party ballot reports its own); the model uses only votes.

In 2020–2024, every "No" choice has a negative choice ID (for example -13023 for "No" on Prop 3). This is harmless as long as choice IDs are treated as labels, not numbers.

### Size

| Year | Rows | Precincts | Contests |
|---|---|---|---|
| 2012 | 126,568 | 1,977 | 156 |
| 2014 | 166,397 | 1,863 | 175 |
| 2016 | 120,701 | 1,668 | 195 |
| 2018 | 152,007 | 1,546 | 204 |
| 2020 | 118,962 | 1,795 | 181 |
| 2022 | 200,640 | 2,043 | 214 |
| 2024 | 119,664 | 2,168 | 212 |

Precinct boundaries and numbers change between elections, so the same precinct number in two years is not the same place.

### Pseudo-precinct 99999 (2022 and 2024)

Precinct `99999` is not a real precinct. It appears in about 156 contests, and its registered-voter and ballot counts change from contest to contest: 81 different pairs in 2022 and 90 in 2024. It looks like a pool of small precincts assembled separately for each contest. In 2024 several of its rows show more ballots than registered voters (for example 33 ballots against 32 registered in Newport Beach's council districts), probably an artifact of the pooling.

Consequences:
- Drop it from the model. A precinct random effect is meaningless for a pool that differs by contest, and it holds only a few hundred ballots per contest.
- County-wide registration or turnout totals must be computed within a contest, not by taking one row per precinct.

In all other precincts, registered voters and ballots cast are the same on every row of that precinct.

### Party labels

Count of contests by the party labels in the files (candidate rows only):

| Year | Contests with any party label | Exactly 1 D + 1 R | Same party |
|---|---|---|---|
| 2012 | 1 | 1 (President only) | 0 |
| 2014 | 0 | 0 | 0 |
| 2016 | 18 | 16 | 2 |
| 2018 | 26 | 22 | 2 |
| 2020 | 17 | 16 | 1 |
| 2022 | 30 | 27 | 2 |
| 2024 | 19 | 19 | 0 |

Party labels are almost entirely missing for 2012 and 2014. They must be entered by hand for the congressional, legislative, and statewide races (roughly 20 contests per year), from the PDF statements of votes or from the California Secretary of State. Labels for the other years should be spot-checked too.

Other combinations seen: 2018 Insurance Commissioner has only a DEM label (the other candidate ran with no party preference); 2018 Assembly District 69 is DEM vs LIB; 2022 Assembly District 59 has only a REP label. None of these is a D-vs-R contest.

## Model

### Unit of observation

One row per precinct × contest, restricted to contests with exactly one Democrat and one Republican. The response is the two-party split: *y* = Democratic votes out of *n* = Democratic + Republican votes. Third-party and write-in votes are excluded from *n*.

### Specification

For precinct *i* in contest *j*, held in election *e*:

```
y_ij ~ Binomial(n_ij, pi_ij)
logit(pi_ij) = beta_e + u_i + d_D(j) - r_R(j) + eps_ij
```

| Term | Meaning | Distribution |
|---|---|---|
| beta_e | County intercept, **one per election** (fixed) | — |
| u_i | Precinct partisan lean, precinct nested in election (random) | N(0, sigma²_p) |
| d_k | Overperformance of Democratic candidate *k* (random) | N(0, sigma²_D) |
| r_k | Overperformance of Republican candidate *k* (random) | N(0, sigma²_R) |
| eps_ij | Precinct × contest residual, the observation-level random effect (random) | N(0, sigma²_eps) |

**Decisions made:**
- **One intercept per election** (confirmed by Michael), not a single county intercept. Otherwise statewide and national swings, such as 2018, would show up as candidate effects.
- **Observation-level random effect** (or a beta-binomial family instead). Each cell holds hundreds of votes, so a pure binomial understates the variance badly, gives far-too-small standard errors, and lets the largest precincts dominate the candidate estimates.
- **Separate variances for Democratic and Republican candidates.** Republican candidates enter the software as an ordinary random intercept that pushes the Democratic share up, so an underperforming Republican gets a positive estimate. Flip the sign afterwards to get r_k.

A candidate's effect is how far their two-party share deviates from what their precincts' lean and that election's county baseline predict. Each candidate is measured only against the precincts where they appeared on the ballot. Near a 50% share, 0.1 in log-odds is about 2.5 percentage points.

### What is identified

Within one contest the data identify only the difference d_D − r_R, the contest-level skew. Treat candidates as nodes and contests as edges joining them; within each connected component of that graph, adding one constant to every d and every r leaves every skew unchanged. The data therefore identify differences between candidates of the same party in the same component (for example, Young Kim's 2022 and 2024 challengers compared with each other), and never the split between the parties within a component, which comes from the variance ratio. A component grows when candidates run more than once against different opponents (incumbents in Congress, the legislature, and statewide offices), so candidate comparisons become broad only as elections are added.

Report:
- **Contest-level skew** (d − r) for every contest, which is fully identified.
- **Candidate effects** with their component, compared within party and component, with conditional variances.

### Optional additions

- Fixed effects for incumbency and office type, if the goal is overperformance beyond incumbency. Without them, candidate effects largely sort incumbents from challengers. Incumbency competes with the candidate effects; see methodology improvement 1.
- Candidate × precinct slopes, to capture candidates whose appeal is concentrated in particular places. The residual term absorbs this only on average.

## Connecting the data to the model

- **Precinct key:** `election:precinct`. Because precincts are nested within elections, no geographic crosswalk is needed. The cost is that precinct effects can't be compared across years.
- **Candidate key:** a hand-built lookup table mapping each (election, contest, choice) to a stable person ID and party. Choice IDs change every election and names are formatted inconsistently (for example `EDMUND G. "JERRY" BROWN`; presidential tickets stored as one string like `BARACK OBAMA JOSEPH BIDEN`). Links across years are what identify the candidate effects, so this table is the main piece of work.
- **Vote counts:** for 2012–2018, add `Absentee_votes + Early_votes + Election_Votes`. For 2020–2024, use `Total Votes` and check that it equals the sum of the vote-method columns.
- **Excluded contests:** same-party contests (California's top-two system), contests with a no-party candidate, nonpartisan offices, and ballot measures.
- **Excluded rows:** precinct 99999; rows with *n* = 0; write-ins and third-party candidates (they drop out of *n*).
- **Caveat for reporting:** many congressional and legislative districts extend into Los Angeles, Riverside, or San Diego counties. The model measures each candidate's performance within Orange County only.

## Software

Prepare the data in either language; fit in R.

- **`glmmTMB` (main tool):**
  `cbind(D, R) ~ 0 + election + (1 | election:precinct) + (1 | candD) + (1 | candR) + (1 | obs)` with `family = binomial(link = "logit")`, or drop `(1 | obs)` and use `family = betabinomial(link = "logit")`. Scales better to the 100k+ rows expected.
- **`lme4::glmer`:** same formula, `control = glmerControl(optimizer = "bobyqa")`; fit first with `nAGQ = 0` for speed, then refit with `nAGQ = 1`. Candidate estimates from `ranef(fit, condVar = TRUE)`.
- **`brms`:** only if full posteriors are wanted (for example, rank uncertainty). Slow at this size. Its `mm()` syntax can express the ±1 multi-membership directly.
- **Python:** `statsmodels`' `BinomialBayesMixedGLM` (variational) handles this many observation-level levels poorly; `Bambi`/`PyMC` works but is slow.

## Code

All in R (`data.table`, `glmmTMB`). Run from the project folder:

- `Rscript run.R` fits every election marked `ready` in `R/elections.R`; `Rscript run.R 2022-11-08 2024-11-05` fits the elections named. `--cores N` sets the worker processes for the leave-one-contest-out refits (default: all cores but one). A full 2016–2024 run takes about 9 minutes.
- `Rscript tests/run_tests.R` runs the tests of the documented rules (base R, made-up inputs, no election data; about 10 seconds). Each test was checked by breaking the code it covers and confirming the test fails.

| File | Role |
|---|---|
| `R/setup.R` | Loads the packages and sources the modules below; scripts start with `source("R/setup.R")`. |
| `R/config.R` | Paths (`data/`, `output/`, the two lookup files) and shared constants: the lookup key, the dropped pseudo-precinct, the fit-flag thresholds, the number of cores. |
| `R/layouts.R` | `LAYOUTS`: how to read each file family, including the party-split variants of the 2020 and 2024 presidential primaries. |
| `R/elections.R` | `ELECTIONS`: date, type, file, layout, column renames, `role`, and `ready` for each election. `role = "main"` marks the November generals fitted by `run.R`; `role = "secondary"` marks primaries, odd-year, and special elections, which never enter the main fit. 2012 and 2014 are registered with `ready = FALSE` until their party labels are entered. |
| `R/load.R` | `read_election()`: one file into the standard long table (election, precinct, contest, choice, party, write-in flag, votes, registered, ballots). |
| `R/lookup.R` | Reading, writing, and merging the hand-edited lookup files, so that hand edits survive reruns. |
| `R/candidates.R` | `update_candidates()` (the candidate lookup `candidates.csv`), default person IDs, display names, and the components described under "What is identified". |
| `R/build.R` | `build_dvr()`: the precinct × contest table of D and R votes. |
| `R/fit.R` | `fit_skew()` with either candidate or contest effects, either an observation-level effect or beta-binomial, and (contest model) one pooled skew variance or a separate one per election; `fit_all()` and the main fit; effects and precinct leans. |
| `R/diagnostics.R` | The leave-one-contest-out check (refits in parallel) and the fit flag. |
| `R/nonpartisan.R` | Skews of nonpartisan races designated D and R by hand in `nonpartisan.csv` (see below). |
| `R/report.R` | The output tables and the console summary. |
| `run.R` | Fits the chosen elections and writes five CSV files to `output/<years>/`, replacing the previous run's: `results.csv` (one row per race: Democratic share, race skew with standard error and in percentage points, each candidate's effect, and the race's leave-one-contest-out slope and R² with a fit flag), `candidate_effects.csv` (one row per candidate, with component), `model_summary.csv` (convergence, AIC, intercepts, variance components), `leave_one_contest_out.csv`, and `nonpartisan_skews.csv`. Effects are on the logit scale and positive when the candidate ran ahead of the party baseline. |

Adding an election: put its file in `data/` as `<date>_<type>.<csv|txt>`, add a layout to `LAYOUTS` if needed and an entry to `ELECTIONS`, run it once so that `update_candidates()` adds its candidates to `candidates.csv`, and fill in `party` and `person_id` by hand where the file lacks them.

### Candidate links

Default person IDs come from the ballot name (uppercased, accents transliterated, punctuation removed). Links that the default misses, confirmed by Michael on November 9, 2026, are recorded in the `note` column of `candidates.csv`:

- Steven S. Choi (2016–2018) and Steven "Steve" Choi (2020–2024): `STEVEN_CHOI`.
- Linda T. Sánchez, printed `S-NCHEZ` in 2022–2024: `LINDA_T_SANCHEZ`. Her sister Loretta Sanchez is a different person (her 2016 and 2020 races are not in the model).
- Bob J. Archuleta (2018) and Bob Archuleta (2022): `BOB_ARCHULETA`.
- Diedre Nguyen (2020) and Diedre Thu-Ha Nguyen (2022): `DIEDRE_THU_HA_NGUYEN`.
- Presidential tickets are linked by presidential nominee: `DONALD_J_TRUMP` (2016, 2020, 2024), `HILLARY_CLINTON`, `JOSEPH_R_BIDEN`, `KAMALA_D_HARRIS` (2024 only).

Kept separate: Alan and Josh Lowenthal; Linda and Kate Sanchez; Janet and Diedre Nguyen; Phillip, Lanhee, and Jay Chen.

### Designated nonpartisan races

`nonpartisan.csv` lists nonpartisan races to estimate after each main fit, one row per candidate, with one candidate marked `DEM` and one `REP` (others blank; they drop out of n, as do write-ins). `add_nonpartisan(long, election, contest_id)` appends a race's candidates for marking. These races do not enter the main fit. Each precinct's lean (election intercept plus precinct effect) comes from the contest model, and the race's skew is the intercept of a quasi-binomial regression of the two-candidate share on that lean as an offset, so it is on the same scale as the partisan race skews. A second regression with a free slope reports how closely the race follows partisanship; a low R² means the D/R reading explains little of the race. The standard error excludes the uncertainty of the leans and of the election baseline, so it is smaller than the standard errors of the partisan race skews.

Treating Congressional District 45 in 2024 (Tran vs. Steel) as a designated race, with it removed from the main fit, reproduces its leave-one-contest-out result (skew 0.049, slope 0.95, R² 0.805), which checks the method.

Races that one precinct lean predicts poorly carry `fit_flag` = `poor partisan fit` in `results.csv` and `nonpartisan_skews.csv`: R² after a single shift below 0.75, or slope outside 0.75–1.33 (`POOR_FIT` in `R/fit.R`). For a partisan race the slope and R² come from the leave-one-contest-out check. A flagged race's skew averages over a pattern that the D/R reading fits poorly.

Current list: 2022 Supervisor District 2 (Vicente Sarmiento as D, Kim Bernice Nguyen as R); 2022 Supervisor District 5 (Katrina Foley as D, Pat Bates as R); 2024 Supervisor District 1 (Frances Marquez as D, Janet Nguyen as R). Donald Wagner vs. Farrah Khan (2024 Supervisor District 3) was decided in the March 2024 primary and is not in the November files.

## 2024 demonstration results

Run of `Rscript run.R 2024-11-05` on the 2024 general election file: 7,984 precinct × contest rows, 19 contests, 1,494 precincts (the other 673 cast no ballots), about 900 two-party votes per row.

- **Variance components** (logit SD): precinct lean 0.42, contest skew 0.13, observation-level residual 0.006. The beta-binomial fit agrees (phi about 73,000). Within one election there is almost no overdispersion beyond binomial noise.
- **One lean per precinct carries across contests.** Leaving each contest out, the precinct lean learned from the other 18 explains 81–99% of the precinct-level variance of the held-out contest after a single shift (`output/2024/leave_one_contest_out.csv`). Slopes range from 0.90 (President) to 1.19 (Assembly District 68), near the value 1 the model assumes. The weakest fit is Congressional District 45 (Tran vs. Steel, R² 0.81).
- **Contest skews are well separated.** They range from +0.21 (Correa vs. Pan) to −0.23 (Jones vs. Dixon), against a standard error of about 0.03, most of which is uncertainty in the shared baseline. Skews are relative to the average 2024 contest.
- **Candidate effects are not identified in 2024 alone.** Every candidate runs in one contest (Schiff and Garvey run twice, against each other), so the fit splits each skew evenly (d = −r) and estimates identical Democratic and Republican variances; the beta-binomial candidate fit has a non-positive-definite Hessian. The candidate standard errors in `output/2024/candidate_effects.csv` (up to 4.5 on the logit scale, proportional to the size of the effect) reflect that the variance ratio, and with it the split, is unknown. Separating candidates needs more elections.
- The two Senate contests (full and partial term) are near-duplicates and count Schiff and Garvey twice.
- **The skews are not an artifact of the presidential contest.** Refitting without it changes the contest SD from 0.127 to 0.123 and moves no skew by more than 0.02.

## 2022 and 2024 combined

Run of `Rscript run.R 2022-11-08 2024-11-05` (`output/2022_2024/`). 2022 loads with the 2024 layout and its vote-method sums check out. It adds 15,831 rows, 27 contests, and 1,386 precincts (about 690 two-party votes per row). Its party labels look correct; Assembly District 59 (Chen against a candidate with no party label), Assembly District 71 (two Republicans), and the Board of Equalization (two Democrats) drop out.

- **Variance components** (logit SD): precinct lean 0.47, contest skew 0.09, observation-level residual effectively 0. The precinct lean carries across contests in 2022 as well: leave-one-contest-out R² is 0.85–0.99, lowest for State Senate District 36 (Carr vs. Nguyen, 0.85) and Assembly District 67 (Quirk-Silva vs. Yoo, 0.88).
- **2022 skews are smaller than 2024 skews.** The 2022 statewide contests all lie within ±0.05 except Controller (−0.13, Cohen vs. Lanhee Chen). The four repeat matchups all moved: Valencia vs. Tardif 0.04 → 0.13, Sánchez vs. Ching 0.03 → 0.14, Pacheco vs. Ortiz 0.09 → 0.13, Duncan vs. Davies −0.02 → −0.04; so did repeat incumbents (Correa 0.10 → 0.20, Dixon −0.06 → −0.23). A separate skew variance for each election may fit better than one pooled variance.
- **Candidate components stay small.** Eleven people ran twice against different opponents, but the largest component has four candidates (20 components have two, 7 have three, 2 have four). The candidate model has a worse AIC than the contest model (182,611 vs. 182,569), and the beta-binomial candidate fit does not converge. Candidate effects need the earlier elections to link into larger components.

## 2016–2024 combined

Run of `Rscript run.R` on 2016–2024 (`output/2016_2018_2020_2022_2024/`, 665 seconds; latest run November 9, 2026). Contests with exactly one Democrat and one Republican: 16 (2016), 22 (2018), 16 (2020), 27 (2022), 19 (2024); 49,465 precinct × contest rows in all.

- **Variance components** (logit SD, contest model with pooled skew variance): precinct lean 0.53, contest skew 0.12, observation-level residual effectively 0. The contest model has a better AIC than the candidate model (369,429 vs. 369,609).
- **Skew spread differs by election, and the main contest model now has a separate skew variance for each election** (`diag(0 + election | contest_key)`). Estimated skew SD: 0.19 (2016), 0.11 (2018), 0.14 (2020), 0.06 (2022), 0.13 (2024). It improves AIC over one pooled variance by 18.8 for four extra parameters (369,410 vs. 369,429) and gives `race_skew` in `results.csv` and the precinct leans for the nonpartisan races. On 2022–2024 alone, the change moved no contest skew by more than 0.004 (correlation 0.9999 with the pooled fit); the precision of each race keeps shrinkage small. The 2016 variance may be inflated by the Assembly District 69 outlier (below).
- **The precinct lean carries across contests in every election.** Median leave-one-contest-out R² is 0.94–0.98 per election and slopes run 0.90–1.19, with one exception below.
- **2016 Assembly District 69 (Tom Daly vs. Ofelia Velarde-Garcia) does not follow partisanship.** Its slope is 0.30 and its leave-one-contest-out R² is −3.0, while Daly's 2020 race (vs. Jon Paul White) has slope 1.06 and R² 0.98. The label and the data are correct: Michael checked the party labels on Ballotpedia (https://ballotpedia.org/California_State_Assembly_District_69), and the file's totals match Ballotpedia's exactly in 2016 (Daly 69,640, Velarde-Garcia 32,324) and 2020 (Daly 99,731, White 37,065); the district lies wholly in Orange County. The race is a genuine outlier: its two-candidate share varies across precincts at less than a third of the rate partisanship predicts, so its average skew (−0.03) summarizes a race that the D/R reading fits poorly.
- **Candidate components are larger.** The largest has 13 candidates (others 9, 8, 7, and 6); 38 components still have only 2. Among candidates in three or more contests, the largest effects are Correa (+0.17), Laurie Davies (+0.14), Alan Lowenthal (+0.13), Katie Porter (+0.12), and Choi (+0.12); Donald Trump, across 2016–2024, is −0.13 (standard error 0.06), behind other Orange County Republicans. These compare candidates within party and component only.
- **Fit flag.** Of the 100 partisan races, only 2016 Assembly District 69 is flagged `poor partisan fit`.
- **Designated nonpartisan races** (`nonpartisan_skews.csv`), all three flagged:

  | Race | Skew (logit) | Points | Slope | R² |
  |---|---|---|---|---|
  | 2022 Supervisor District 2: Sarmiento (D) vs. Kim Bernice Nguyen (R) | −0.31 | −7.8 | 0.71 | 0.49 |
  | 2022 Supervisor District 5: Foley (D) vs. Bates (R) | +0.20 | +4.9 | 0.70 | 0.65 |
  | 2024 Supervisor District 1: Marquez (D) vs. Janet Nguyen (R) | −0.30 | −7.1 | 0.89 | 0.32 |

  Points are percentage points of the two-candidate share toward the designated D. Nonpartisan races follow partisanship less closely than partisan races (slopes near 0.7, against about 1 for partisan races), as expected without party labels on the ballot, so the flag thresholds catch them all; their skews remain averages over the race's precincts. Thresholds specific to nonpartisan races, or the per-race slope of methodology improvement 2, would separate these from genuinely poor fits.

## Adding the 2026 election

The November 3, 2026 results are due to be certified by December 3, 2026.

1. Download the plain-text extract from the OC Vote archive (https://ocvote.gov/data/election-results-archives/archived-statement-of-votes) and unzip it into `data/` as `2026-11-03_general.txt` (or `.csv`, after its actual format). Each election's extract is the "Plain-text extract for data analysis (media.zip)" link on that page; the URL pattern varies by year.
2. Inspect the header and separator. 2022 and 2024 are tab-separated with headers `.Precinct`, `Choice Name` (2024: `Choice Name1`), `Total Votes`, `Vote Center Votes`, and `Vote by Mail Votes`. If 2026 matches, add `"2026-11-03" = list(type = "general", file = "2026-11-03_general.txt", layout = "tab_2022", ready = TRUE)` to `ELECTIONS` in `R/elections.R`, with a `rename` for any renamed column; otherwise add a layout. `read_election()` stops with the names of any missing columns, and the layout's check stops if the total votes differ from the sum of the vote-method columns.
3. Check the encoding of accented names and whether pseudo-precinct `99999` appears again (it is dropped by default).
4. Run `Rscript run.R 2026-11-03` once to add the 2026 candidates to `candidates.csv`. Review the new rows: party labels (`party`), and links to earlier candidates (`person_id`; candidates who reappear under a different ballot name, such as a new middle initial or nickname). Ask Michael about any ambiguous match (for example, the Sánchez sisters).
5. Designate nonpartisan races of interest (for example, the 2026 supervisor races) in `nonpartisan.csv` with `add_nonpartisan()`, marking one candidate `DEM` and one `REP`.
6. Run `Rscript run.R` for all elections; results go to `output/2016_2018_2020_2022_2024_2026/`. Run long jobs with the log outside OneDrive (OneDrive has truncated logs written inside this folder while R was writing them).
7. Run `Rscript tests/run_tests.R`; if a new layout was added, add a test for it there. Read `model_summary.csv` (convergence, per-election skew SD), the `fit_flag` column of `results.csv`, and `nonpartisan_skews.csv`, and add a results section here.

The 2026 midterm has statewide races; under the top-two primary some may be two candidates of the same party, which drop out of the model.

## Proposed: secondary elections (primaries, odd-year, and special elections)

Proposed on November 10, 2026. The extracts are downloaded and load; the second stage is not implemented.

**Downloaded extracts** (November 10, 2026, from the OC Vote archive), registered with `role = "secondary"` in `R/elections.R`; all load, and every vote-total check passes. One-D-one-R contests are those with exactly one Democrat, one Republican, and no candidate without a party; multi-candidate contests have a Democrat and a Republican but more than one of either.

| Election | File | Precincts | One-D-one-R contests | Multi-candidate D-and-R contests | Best anchor general (share of ballots in matching precinct numbers; registration correlation) |
|---|---|---|---|---|---|
| June 3, 2014 primary | `2014-06-03_primary.csv` | 1,856 | 0 (no party labels; 26 partisan-office contests) | 0 | Nov 2014 (0.89; 0.99) |
| March 17, 2015 State Senate 37 special primary | `2015-03-17_special-primary.csv` | 248 | 0 (two Republicans) | 0 | Nov 2014 (0.40; 0.80) |
| June 7, 2016 primary | `2016-06-07_primary.csv` | 1,597 | 4 | 7 | Nov 2016 (0.89; 0.94) |
| June 5, 2018 primary | `2018-06-05_primary.csv` | 1,561 | 5 | 11 | Nov 2018 (0.80; 0.79); Nov 2016 (0.84; 0.86) |
| Nov 5, 2019 district election | `2019-11-05_general-district.csv` | 84 | 0 (local measures and offices) | 0 | Nov 2018 (0.53; 0.70) |
| March 3, 2020 primary | `2020-03-03_primary.csv` | 1,703 | 4 | 9 | Nov 2020 (0.995; 0.97) |
| June 7, 2022 primary | `2022-06-07_primary.txt` | 2,076 | 5 | 15 | Nov 2020 (0.96; 0.98); Nov 2022 (0.82; 0.86) |
| March 5, 2024 primary | `2024-03-05_primary.txt` | 2,101 | 8 | 7 | Nov 2024 (1.00; 0.98) |
| Feb 25, 2025 State Senate 36 special primary | `2025-02-25_special-primary.txt` | 662 | 0 | 1 | Nov 2024 (1.00; 1.00) |
| Nov 4, 2025 statewide special | `2025-11-04_special.txt` | 2,174 | 0 (Proposition 50 only) | 0 | Nov 2024 (1.00; 1.00) |
| June 2, 2026 primary | `2026-06-02_primary.txt` | 2,259 | 13 | 16 | Nov 2024 (0.85; 0.89), until Nov 2026 is available |

In all, 39 one-D-one-R contests (2016–2026 primaries) can enter the second stage as proposed, and 66 multi-candidate contests would need a multinomial extension. Among the one-D-one-R contests is 2016 Assembly District 69 (Daly vs. Velarde-Garcia), the race flagged in the 2016 general. Not downloaded: the supervisor vacancy elections (2015, 2019, 2021), the 2021 recall, and the June 10, 2025 special election.

**Peculiarities to factor out.**
1. A different electorate: lower, older, more partisan turnout shifts the D share at a given precinct lean, and can make partisanship steeper or flatter.
2. Few partisan contests: a special election with one or two D-vs-R contests cannot estimate its own precinct leans, so a lone contest's skew is confounded with the election intercept.
3. Several candidates per party in top-two primaries: the two-candidate share exists only where exactly one Democrat and one Republican run. Presidential primaries are party ballots and are not D-vs-R contests.

**Model.** Estimate secondary elections in a second stage, against the precinct leans of the main fit, as is done for nonpartisan races. Each secondary election e′ is linked to an anchor general election g (the same year's November general for a primary; the nearest general for an odd-year or special election). For precinct i and contest j:

```
logit(pi_ij) = a_e' + b_e' * lean_i^g + s_j + eps_ij
```

- lean_i^g = beta_g + u_i from the main fit, held fixed;
- a_e′, a shift per secondary election (electorate composition and baseline);
- b_e′, a slope per secondary election (how strongly partisanship sorts that electorate);
- s_j, the race skew, with a variance per secondary election where it has enough contests and pooled by election type otherwise;
- eps_ij, the observation-level effect.

Fitted with glmmTMB as `cbind(D, R) ~ 0 + election + election:lean + (1 | contest_key) + (1 | obs)` over all secondary elections chosen. Their skews are then on the scale of the general-election skews, net of each election's shift and slope. This makes the secondary elections optional (`role = "secondary"` in `ELECTIONS`, included only when asked), gives them zero weight in the precinct leans and variances of the main fit, and factors out their peculiarities through a_e′, b_e′, and their own skew variances. Contest selection is the existing rule (exactly one Democrat and one Republican); multi-candidate primary contests could later enter through a multinomial model.

**Weighting into candidate effects.** Once candidate effects are re-specified (methodology improvement 1), secondary-election skews can enter the candidate model as additional observations with a weight below 1 (glmmTMB `weights`), so that a candidate's primary or special-election performance counts, but less than a general election.

**Joint alternative.** A single model with a precinct effect shared by a primary and its general, scaled per election, is expressible with glmmTMB's reduced-rank term `rr(0 + election | precinct, d = 1)`, with case weights below 1 for secondary elections. It is more efficient but couples the main fit to the secondary data; it is worth trying after the two-stage version works.

**Precondition: precinct identity.** The second stage joins precincts by number between a secondary election and its anchor general. The match table above shows that same-year matches are near complete for 2014, 2016, 2020, 2024, and 2025, but only about 80% for June 2018 and June 2022 (redistricting took effect between the June 2022 primary and the November general), and that the 2015 special primary and 2019 district election match poorly. A shared precinct number with similar registration (correlation near 1, median ratio near 1) is evidence, not proof, of the same place; a crosswalk from precinct boundary files would settle the weaker pairs. The `Precinct ID` column of the 2020–2026 files is a different number in each election (precinct 02001 is 102346 in November 2024 and 107767 in June 2026), so it cannot link precincts.

**Also possible.** A partisan ballot measure (for example, the November 4, 2025 statewide measure) can be read as a designated D-vs-R race, Yes against No, and estimated like the nonpartisan races.

## Methodology improvements

In order of expected value. None is implemented; all are to be revisited with the 2026 results.

1. **Persistent candidate effects, race-specific terms, and incumbency, designed together.** Model each skew as d_D − r_R + c_j, with c_j a race-specific term with a per-election variance; the ratio of candidate to race variance measures how much overperformance follows a person from race to race. The current candidate model forces all skew into persistent effects, which is likely why its AIC is worse than the contest model's. The largest skews mostly favor incumbents, so an incumbency (and open-seat) term is the natural addition, but it competes with the persistent candidate effects:
   - **Confounding.** Most incumbents hold office in every race observed here (for example Correa, Linda Sánchez, and Alan Lowenthal), so the data cannot separate the person from the office for them. Because candidate effects are random and shrunk toward zero, an incumbency term absorbs the average incumbent's skew, and each incumbent's candidate effect then means "relative to other incumbents".
   - **Selection.** Incumbents won before, so they tend to be strong candidates. Incumbency is then correlated with the candidate effect, contrary to the random-effects assumption; the incumbency coefficient absorbs candidate quality and overstates the value of the office.
   - **Remedy.** Identify incumbency from candidates whose status changes (challenger or open-seat candidate in one race, incumbent in a later one), for example by adding each candidate's mean incumbency as a covariate, so that the incumbency coefficient measures only within-candidate change (the sophomore-surge design of the incumbency-advantage literature). Count such transitions when incumbency is coded; the 2016–2024 window may hold few.
   - **Question first.** For "who ran ahead of their party", incumbency is part of what a candidate earned and need not be removed; for "who is a strong candidate apart from the office", it must be. Reporting both versions answers both.
2. **A random slope on precinct lean for each race** (logit = beta_e + (1 + s_j) u_i + skew_j + ...). This fits races such as 2016 Assembly District 69 (slope 0.30) and the President (0.90 in 2024) instead of flagging them, and defines each skew at the average precinct.
3. **Uncertainty of nonpartisan skews.** Propagate the uncertainty of the precinct leans and election baseline, by simulation from the main fit or by fitting designated races inside the model with fixed effects.
4. **Precinct demographics.** The races that partisanship fits worst (Marquez vs. Nguyen, R² 0.32; Tran vs. Steel; Carr vs. Nguyen) suggest candidate-specific geographic appeal. Joining precinct shapefiles to census data would allow candidate × demographic terms. This needs precinct boundaries for each election.
5. **Baseline definition.** Skews are relative to the average D-vs-R race of each election, which depends on the ballot (2022 has eight statewide races). Anchoring to the top of the ticket or to the statewide races would make skews more comparable across elections.
6. **Roll-off.** The two-party share ignores ballots that skip the race. A model of D, R, and undervotes together would separate persuasion from drawing voters into the race.
7. **Housekeeping.** Drop one of each pair of duplicate Senate races (2022 and 2024), and check totals against the PDF statements of votes.

## Next steps

1. Add the November 3, 2026 election once certified (deadline December 3, 2026), following "Adding the 2026 election".
2. Revisit the methodology improvements with the 2026 results, starting with 1 (candidate effects, race-specific terms, and incumbency, designed together) and 2 (a random slope on precinct lean per race).
3. Fill in `party` and `person_id` in `candidates.csv` for 2012 and 2014, set `ready = TRUE` for both in `R/elections.R`, and link people across years.
4. Housekeeping: duplicate Senate races, and totals against the PDF statements of votes.
5. Secondary elections (primaries, odd-year, and special elections): the extracts are downloaded and checked. Agree on the proposal, choose an anchor general for each election (see the match table), and implement the second stage; then consider the multinomial extension for the 66 multi-candidate contests and hand labels for the 2014 primary.
