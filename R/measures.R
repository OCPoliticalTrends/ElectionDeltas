# Statewide ballot propositions, tested automatically as two-choice races.
#
# Every statewide proposition with exactly the choices Yes and No is estimated
# like a designated nonpartisan race (nonpartisan_skews() in nonpartisan.R),
# with one side as "D" and the other as "R". The D side is chosen from the
# data: Yes is the D side when the Yes share rises with precinct Democratic
# lean (a positive slope of logit(Yes) on the predicted lean), No otherwise.
# Outputs, per proposition:
#   d_side      the choice aligned with Democratic precincts (Yes or No)
#   slope, R2   how partisan the measure was: near 1, as partisan as a
#               candidate race; near 0, partisanship did not predict it (the
#               D side is then arbitrary, and the fit flag fires)
#   race_skew   how far the D side ran ahead of (positive) or behind the
#               precincts' partisan lean
# Local measures, judicial retention votes, and recalls are left out.

# Statewide proposition titles: "Proposition 13" (2020 on) or
# "51-SCHOOL BONDS. ..." (2016-2018). Local measures are lettered
# ("A-City of Stanton, ...").
STATE_MEASURE <- "^(PROPOSITION\\s*[0-9]+|[0-9]+\\s*-)"

# Designations for every statewide proposition in `long`: Yes as `yes_party`,
# No as the other party, in the format of nonpartisan.csv.
measure_designations <- function(long, yes_party = "DEM") {
  ch <- unique(long[writein == FALSE & grepl(STATE_MEASURE, toupper(contest)),
                    .(election, contest_id, contest, choice_id, choice,
                      yn = toupper(trimws(choice)))])
  ok <- ch[, .(keep = .N == 2 && setequal(yn, c("YES", "NO"))), by = .(election, contest_id)][keep == TRUE]
  des <- ch[ok, on = .(election, contest_id)]
  des[, party := ifelse(yn == "YES", yes_party, setdiff(c("DEM", "REP"), yes_party))]
  des[, .(election, contest_id, contest, choice_id, choice, party)]
}

# Swap DEM and REP for the measures listed in `flip` (election, contest_id).
flip_designations <- function(des, flip) {
  des <- copy(des)
  des[flip, on = .(election, contest_id), party := ifelse(party == "DEM", "REP", "DEM")]
  des[]
}

# Skews of every statewide proposition of the elections covered (as in
# nonpartisan_skews()), or NULL if none.
measure_skews <- function(long, fit, w, coefs = NULL) {
  des <- measure_designations(long)
  if (!nrow(des)) return(NULL)
  first <- nonpartisan_skews(long, des, fit, w, coefs)
  if (is.null(first)) return(NULL)
  des <- flip_designations(des, first[slope < 0, .(election, contest_id)])
  m <- nonpartisan_skews(long, des, fit, w, coefs)
  m[, d_side := ifelse(toupper(trimws(dem_candidate)) == "YES", "Yes", "No")]
  m[, yes_share := round(ifelse(d_side == "Yes", dem_share, 1 - dem_share), 4)]
  m[, .(election, contest_id, contest, d_side, yes_share, precincts, precincts_without_lean,
        votes = two_party_votes, race_skew, race_skew_se, race_skew_pts, slope, slope_se,
        R2_shift, R2_slope, lean_source, fit_flag)]
}
