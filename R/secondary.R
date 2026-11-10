# Second stage: race skews of secondary elections (primaries, odd-year, and
# special elections) against the precinct leans of the main fit.
#
# Each secondary election e is linked to an anchor general election g
# (ELECTIONS[[e]]$anchor). Its precincts are joined to the anchor's precincts
# by precinct number, and for precinct i and contest j
#
#   logit(pi_ij) = a_e + b_e * lean_i + s_j + eps_ij
#
# where lean_i = beta_g + u_i is the precinct lean of the main fit (held
# fixed), a_e a shift per election (electorate composition and baseline), b_e
# a slope per election (how strongly partisanship sorts that electorate), s_j
# the race skew with one variance pooled over the secondary elections fitted,
# and eps_ij the observation-level effect. Secondary elections never enter the
# main fit. Precincts without a match in the anchor drop out; the share of
# two-party votes kept is reported per election.

# Secondary elections whose anchor is among `main_elections`, and the others.
secondary_plan <- function(main_elections) {
  sec <- secondary_elections()
  anchor <- vapply(sec, function(e) ELECTIONS[[e]]$anchor, character(1))
  list(use = sec[anchor %in% main_elections], skipped = sec[!anchor %in% main_elections])
}

secondary_formula <- function(n_elections) {
  if (n_elections > 1)
    cbind(D, R) ~ 0 + election + election:lean + (1 | contest_key) + (1 | obs)
  else
    cbind(D, R) ~ 1 + lean + (1 | contest_key) + (1 | obs)
}

# Fit the second stage. `long` is the long table of the secondary elections,
# `cand` the candidate lookup, `main_fit` and `w` the main fit and its data.
# Returns NULL when no secondary contest has exactly one Democrat and one
# Republican; otherwise the fit, its data, and the coverage per election.
fit_secondary <- function(long, cand, main_fit, w) {
  w2 <- build_dvr(long, cand)
  if (!nrow(w2)) return(NULL)
  w2[, anchor := vapply(election, function(e) ELECTIONS[[e]]$anchor, character(1))]
  w2[, anchor_key := paste(anchor, precinct, sep = ":")]
  lean <- precinct_lean(main_fit, w)
  w2[lean, on = .(anchor_key = prec_key), lean := i.lean]

  coverage <- w2[, .(contests = uniqueN(contest_key), precincts = uniqueN(precinct),
                     precincts_with_lean = uniqueN(precinct[!is.na(lean)]),
                     vote_share_with_lean = sum(n[!is.na(lean)]) / sum(n)),
                 by = .(election, anchor)]
  w2 <- w2[!is.na(lean)]
  w2[, obs := .I]
  fit <- glmmTMB(secondary_formula(uniqueN(w2$election)), data = w2,
                 family = binomial(link = "logit"))
  list(fit = fit, w = w2, coverage = coverage)
}

# Shift a_e and slope b_e of each secondary election, with SEs.
secondary_coefs <- function(sec) {
  fe <- fixef(sec$fit)$cond
  se <- sqrt(diag(vcov(sec$fit)$cond))
  els <- sort(unique(sec$w$election))
  if (length(els) == 1)
    return(data.table(election = els, shift = fe[["(Intercept)"]], shift_se = se[["(Intercept)"]],
                      slope = fe[["lean"]], slope_se = se[["lean"]]))
  a <- paste0("election", els); b <- paste0(a, ":lean")
  data.table(election = els, shift = unname(fe[a]), shift_se = unname(se[a]),
             slope = unname(fe[b]), slope_se = unname(se[b]))
}

# secondary_skews.csv and secondary_summary.csv. `cs` holds the main fit's
# contest skews; a secondary race whose year, office, and two candidates match
# a race of the main fit gets that race's skew as general_race_skew.
secondary_tables <- function(sec, cand, cs) {
  co <- secondary_coefs(sec)
  w2 <- merge(sec$w, co[, .(election, a = shift, b = slope)], by = "election")
  w2[, offset := a + b * lean]
  k <- ranef_table(sec$fit, "contest_key")
  names <- candidate_names(cand)
  nm <- function(id) names$name[match(id, names$person_id)]

  races <- w2[, {
    st <- race_fit_stats(.SD)
    .(precincts = .N, two_party_votes = sum(n), dem_share = sum(D) / sum(n),
      slope = st$slope, R2_shift = st$R2_shift)
  }, by = .(contest_key, election, anchor, contest, candD, candR)]
  races[k, on = .(contest_key = level), `:=`(race_skew = i.est, race_skew_se = i.se)]
  gen <- cs[, .(year = substr(contest_key, 1, 4), office = toupper(contest), candD, candR,
                general_race_skew = skew_contest)]
  races[, `:=`(year = substr(election, 1, 4), office = toupper(contest))]
  races[gen, on = .(year, office, candD, candR), general_race_skew := i.general_race_skew]

  skews <- races[, .(
    election, type = vapply(election, function(e) ELECTIONS[[e]]$type, character(1)),
    anchor, contest, dem_candidate = nm(candD), rep_candidate = nm(candR),
    precincts, two_party_votes, dem_share = round(dem_share, 4),
    race_skew = round(race_skew, 3), race_skew_se = round(race_skew_se, 3),
    race_skew_pts = round(100 * race_skew * dem_share * (1 - dem_share), 1),
    slope = round(slope, 3), R2_shift = round(R2_shift, 3),
    fit_flag = fit_flag(slope, R2_shift),
    general_race_skew = round(general_race_skew, 3))]
  setorder(skews, election, -race_skew)

  vc <- variance_components(sec$fit)
  summary <- merge(sec$coverage, co, by = "election", all.x = TRUE)
  summary[, `:=`(type = vapply(election, function(e) ELECTIONS[[e]]$type, character(1)),
                 skew_sd = vc[term == "contest_key", sd])]
  summary <- summary[, .(election, type, anchor, contests, precincts, precincts_with_lean,
                         vote_share_with_lean = round(vote_share_with_lean, 3),
                         shift = round(shift, 3), shift_se = round(shift_se, 3),
                         slope = round(slope, 3), slope_se = round(slope_se, 3),
                         skew_sd = round(skew_sd, 3))]
  list(skews = skews[], summary = summary[])
}
