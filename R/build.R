# The precinct x contest table of two-candidate votes that the models fit.

# Spread D and R votes of the rows of `x` (one row per precinct, contest, and
# party, party DEM or REP) into one row per precinct x contest, keyed by the
# columns `by`. Drops DROP_PRECINCTS and rows with no two-party votes, and adds
# n = D + R and the precinct key election:precinct (precincts are nested in
# elections). `value` names the columns to spread: votes, and optionally
# person_id, which becomes candD and candR.
two_party_table <- function(x, by, value = "votes") {
  x <- x[!precinct %in% DROP_PRECINCTS]
  w <- dcast(x, as.formula(paste(paste(by, collapse = " + "), "~ party")),
             value.var = value)
  if (length(value) == 1) setnames(w, c("DEM", "REP"), c("D", "R"))
  else setnames(w, c("votes_DEM", "votes_REP", "person_id_DEM", "person_id_REP"),
                c("D", "R", "candD", "candR"))
  w[, n := D + R]
  w <- w[n > 0]
  w[, prec_key := paste(election, precinct, sep = ":")]
  w[]
}

# One row per precinct x contest for contests with exactly one Democrat and one
# Republican and no other candidate without a party (write-ins excepted).
# Third-party and write-in votes drop out of n = D + R. Adds the contest key
# election:contest_id and a row number obs for the observation-level effect.
build_dvr <- function(long, cand) {
  cand <- cand[, c(KEY, "party", "person_id"), with = FALSE]
  x <- merge(long[writein == FALSE, !"party"], cand, by = KEY, all.x = TRUE)

  shape <- unique(x[, .(election, contest_id, choice_id, party)])[
    , .(nD = sum(party == "DEM", na.rm = TRUE),
        nR = sum(party == "REP", na.rm = TRUE),
        nNone = sum(is.na(party) | party == "")),
    by = .(election, contest_id)]
  keep <- shape[nD == 1 & nR == 1 & nNone == 0, .(election, contest_id)]
  x <- x[keep, on = .(election, contest_id)][party %in% c("DEM", "REP")]

  w <- two_party_table(x, c("election", "precinct", "contest_id", "contest",
                            "registered", "ballots"),
                       value = c("votes", "person_id"))
  w[, contest_key := paste(election, contest_id, sep = ":")]
  w[, obs := .I]
  w[]
}
