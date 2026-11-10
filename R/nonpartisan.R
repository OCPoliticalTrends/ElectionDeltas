# Skew of a nonpartisan race whose candidates are designated D and R by hand.
#
# The designations live in nonpartisan.csv, one row per candidate of each
# designated race (columns election, contest_id, contest, choice_id, choice,
# party). Exactly one candidate per race is DEM and one is REP; other
# candidates are left blank and drop out of n = D + R, as do write-ins. For a
# race with several seats, the share is still the designated D candidate's
# share of the two designated candidates' votes. In a race with more than two
# candidates, other candidates of either side take votes from that side, so the
# two-candidate share also reflects vote-splitting. The columns candidates and
# designated_vote_share (the two designated candidates' share of all votes in
# the race) show how far this can reach, and vote_split flags races below
# VOTE_SPLIT_MIN; their skews should not be compared with head-to-head races.
#
# The designated races do not enter the main fit. Each precinct's lean
# (beta_e + u_i) is taken from the main fit to the partisan races, and for
# each designated race
#
#   logit(pi_i) = offset_i + skew
#
# is fitted by quasi-binomial regression, so skew is on the same scale as the
# race skews of the partisan contests (positive = the designated D candidate
# ran ahead of the precincts' partisan lean). The offset depends on the
# election (column lean_source):
#   main fit        a main (general) election: offset = lean_i
#   second stage    a secondary election fitted by the second stage:
#                   offset = a_e + b_e * lean_i, with lean_i from the anchor
#                   general and a_e, b_e that election's shift and slope
#   anchor lean     a secondary election the second stage could not fit (no
#                   D-vs-R contests): offset = lean_i of the anchor general,
#                   so the skew also absorbs that electorate's shift
# A second fit with a free slope, logit(pi_i) = a + slope * offset_i,
# measures how closely the race follows partisanship: a slope near 1 and a
# high R^2 mean the D/R reading is apt; a slope near 0 means precinct
# partisanship does not predict the race, and the skew says little.

# Append every candidate of one race to the designations file with a blank
# party, for the D and R designations to be filled in by hand. Designations
# already in the file are kept.
add_nonpartisan <- function(long, election, contest_id, path = NONPARTISAN_FILE) {
  e <- election; k <- contest_id
  new <- unique(long[election == e & contest_id == k & writein == FALSE,
                     .(election, contest_id, contest, choice_id, choice, party = "")])
  if (!nrow(new)) stop("No contest ", k, " in ", e)
  merge_lookup(new, path, keep = "party")
}

# Number of candidates (write-ins excepted) in each designated race, and the
# two designated candidates' share of all its votes.
designated_vote_share <- function(long, des) {
  des <- des[party %in% c("DEM", "REP")]
  v <- long[writein == FALSE & !precinct %in% DROP_PRECINCTS][
    unique(des[, .(election, contest_id)]), on = .(election, contest_id), nomatch = NULL]
  v[, designated := paste(election, contest_id, choice_id) %in% des[, paste(election, contest_id, choice_id)]]
  v[, .(candidates = uniqueN(choice_id),
        designated_vote_share = round(sum(votes[designated]) / sum(votes), 3)),
    by = .(election, contest_id)]
}

vote_split_flag <- function(share) ifelse(share < VOTE_SPLIT_MIN, "vote-split", "")

# Skews of the designated races, or NULL if none. Covers the main elections
# in `w` and the secondary elections in `long` whose anchor general is in `w`;
# `coefs` holds the second stage's shift and slope per secondary election
# (secondary_coefs()), or NULL.
nonpartisan_skews <- function(long, des, fit, w, coefs = NULL) {
  des <- des[party %in% c("DEM", "REP")]
  bad <- des[, .(nD = sum(party == "DEM"), nR = sum(party == "REP")),
             by = .(election, contest_id)][nD != 1 | nR != 1]
  if (nrow(bad)) stop("Each designated race needs one DEM and one REP: ",
                      paste(bad$election, bad$contest_id, collapse = ", "))
  main <- unique(w$election)
  anchor_of <- function(e) {
    if (e %in% main) return(e)
    a <- ELECTIONS[[e]]$anchor
    if (!is.null(a) && a %in% main) a else NA_character_
  }
  des <- des[election %in% unique(long$election)]
  if (!nrow(des)) return(NULL)
  els <- data.table(election = unique(des$election))
  els[, anchor := vapply(election, anchor_of, character(1))]
  els <- els[!is.na(anchor)]
  if (!nrow(els)) return(NULL)
  els[, `:=`(a = 0, b = 1, lean_source = ifelse(election %in% main, "main fit", "anchor lean"))]
  if (!is.null(coefs))
    els[coefs, on = "election", `:=`(a = i.shift, b = i.slope, lean_source = "second stage")]
  des <- des[election %in% els$election]

  x <- merge(long[writein == FALSE, !"party"], des[, c(KEY, "party"), with = FALSE], by = KEY)
  r <- two_party_table(x, c("election", "precinct", "contest_id", "contest"))
  r <- merge(r, els, by = "election")
  r[, anchor_key := paste(anchor, precinct, sep = ":")]
  r[precinct_lean(fit, w), on = .(anchor_key = prec_key), lean := i.lean]
  names_of <- des[, .(dem_candidate = choice[party == "DEM"],
                      rep_candidate = choice[party == "REP"]), by = .(election, contest_id)]
  names_of <- merge(names_of, designated_vote_share(long, des), by = c("election", "contest_id"))

  rbindlist(lapply(split(r, by = c("election", "contest_id")), function(t) {
    n_all <- nrow(t)
    t <- t[!is.na(lean)][, offset := a + b * lean]
    st <- race_fit_stats(t)
    share <- sum(t$D) / sum(t$n)
    data.table(
      election = t$election[1], contest_id = t$contest_id[1], contest = t$contest[1],
      precincts = nrow(t), precincts_without_lean = n_all - nrow(t),
      two_party_votes = sum(t$n), dem_share = round(share, 4),
      race_skew = round(st$shift, 3),
      race_skew_se = round(st$shift_se, 3),
      race_skew_pts = round(100 * st$shift * share * (1 - share), 1),
      slope = round(st$slope, 3),
      slope_se = round(st$slope_se, 3),
      R2_shift = round(st$R2_shift, 3),
      R2_slope = round(st$R2_slope, 3),
      lean_source = t$lean_source[1])
  }))[names_of, on = .(election, contest_id), nomatch = NULL][
    , c("election", "contest_id", "contest", "dem_candidate", "rep_candidate",
        "candidates", "designated_vote_share",
        "precincts", "precincts_without_lean", "two_party_votes", "dem_share",
        "race_skew", "race_skew_se", "race_skew_pts", "slope", "slope_se",
        "R2_shift", "R2_slope", "lean_source"), with = FALSE][
    , `:=`(fit_flag = fit_flag(slope, R2_shift),
           vote_split = vote_split_flag(designated_vote_share))][order(election, contest_id)]
}
