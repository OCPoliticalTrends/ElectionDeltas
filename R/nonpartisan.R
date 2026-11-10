# Skew of a nonpartisan race whose candidates are designated D and R by hand.
#
# The designations live in nonpartisan.csv, one row per candidate of each
# designated race (columns election, contest_id, contest, choice_id, choice,
# party). Exactly one candidate per race is DEM and one is REP; other
# candidates are left blank and drop out of n = D + R, as do write-ins. For a
# race with several seats, the share is still the designated D candidate's
# share of the two designated candidates' votes.
#
# The designated races do not enter the main fit. Each precinct's lean
# (beta_e + u_i) is taken from the main fit to the partisan races, and for
# each designated race
#
#   logit(pi_i) = lean_i + skew
#
# is fitted by quasi-binomial regression, so skew is on the same scale as the
# race skews of the partisan contests (positive = the designated D candidate
# ran ahead of the precincts' partisan lean). A second fit with a free slope,
# logit(pi_i) = a + slope * lean_i, measures how closely the race follows
# partisanship: a slope near 1 and a high R^2 mean the D/R reading is apt; a
# slope near 0 means precinct partisanship does not predict the race, and the
# skew says little.

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

# Skews of the designated races of the elections in `w`, or NULL if none.
nonpartisan_skews <- function(long, des, fit, w) {
  des <- des[party %in% c("DEM", "REP")]
  bad <- des[, .(nD = sum(party == "DEM"), nR = sum(party == "REP")),
             by = .(election, contest_id)][nD != 1 | nR != 1]
  if (nrow(bad)) stop("Each designated race needs one DEM and one REP: ",
                      paste(bad$election, bad$contest_id, collapse = ", "))
  des <- des[election %in% unique(w$election)]
  if (!nrow(des)) return(NULL)

  x <- merge(long[writein == FALSE, !"party"], des[, c(KEY, "party"), with = FALSE], by = KEY)
  r <- two_party_table(x, c("election", "precinct", "contest_id", "contest"))
  lean <- precinct_lean(fit, w)
  names_of <- des[, .(dem_candidate = choice[party == "DEM"],
                      rep_candidate = choice[party == "REP"]), by = .(election, contest_id)]

  rbindlist(lapply(split(r, by = c("election", "contest_id")), function(t) {
    n_all <- nrow(t)
    t <- merge(t, lean, by = "prec_key")
    f0 <- glm(cbind(D, R) ~ 1 + offset(lean), data = t, family = quasibinomial())
    f1 <- glm(cbind(D, R) ~ lean, data = t, family = quasibinomial())
    el <- elogit(t$D, t$R)
    wt <- t$n
    tot <- el - weighted.mean(el, wt)
    share <- sum(t$D) / sum(t$n)
    skew <- unname(coef(f0)[1])
    data.table(
      election = t$election[1], contest_id = t$contest_id[1], contest = t$contest[1],
      precincts = nrow(t), precincts_without_lean = n_all - nrow(t),
      two_party_votes = sum(t$n), dem_share = round(share, 4),
      race_skew = round(skew, 3),
      race_skew_se = round(sqrt(vcov(f0)[1, 1]), 3),
      race_skew_pts = round(100 * skew * share * (1 - share), 1),
      slope = round(unname(coef(f1)[2]), 3),
      slope_se = round(sqrt(vcov(f1)[2, 2]), 3),
      R2_shift = round(weighted_R2(el - t$lean - skew, tot, wt), 3),
      R2_slope = round(weighted_R2(el - predict(f1), tot, wt), 3))
  }))[names_of, on = .(election, contest_id), nomatch = NULL][
    , c("election", "contest_id", "contest", "dem_candidate", "rep_candidate",
        "precincts", "precincts_without_lean", "two_party_votes", "dem_share",
        "race_skew", "race_skew_se", "race_skew_pts", "slope", "slope_se",
        "R2_shift", "R2_slope"), with = FALSE][
    , fit_flag := fit_flag(slope, R2_shift)][]
}
