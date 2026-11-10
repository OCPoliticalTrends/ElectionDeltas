# Fit the skew models and extract their effects.
#
#   logit(pi_ij) = beta_e + u_i + d_D(j) - r_R(j) + eps_ij       candidate model
#   logit(pi_ij) = beta_e + u_i + s_j + eps_ij                   contest model
#
# beta_e is one intercept per election, u_i the lean of precinct i (nested in
# its election), s_j = d_D(j) - r_R(j) the skew of contest j, and eps_ij the
# observation-level effect. glmmTMB estimates the Republican term as an
# ordinary random intercept that raises the Democratic share, so r_k is minus
# that estimate.

# family "olre": binomial with an observation-level random effect (eps_ij).
# family "betabinomial": overdispersion through the beta-binomial instead.
# skew "candidate": separate Democratic and Republican candidate effects.
# skew "contest": one effect per contest, the identified difference d - r.
# skew_var "pooled": one skew variance for all elections.
# skew_var "by_election" (contest model only): a separate skew variance for
#   each election, written diag(0 + election | contest_key). Each contest
#   belongs to one election, so only that election's column carries its skew.
# With a single election, beta_e reduces to one intercept and the skew
# variance is pooled.
model_formula <- function(family = "olre", skew = "candidate", n_elections = 2,
                          skew_var = "pooled") {
  if (skew_var == "by_election" && skew != "contest")
    stop("skew_var = \"by_election\" is implemented for the contest model only")
  by_el <- skew_var == "by_election" && n_elections > 1
  rhs <- c(if (n_elections > 1) "0 + election" else "1", "(1 | prec_key)",
           switch(skew,
                  candidate = c("(1 | candD)", "(1 | candR)"),
                  contest   = if (by_el) "diag(0 + election | contest_key)"
                              else "(1 | contest_key)"),
           if (family == "olre") "(1 | obs)")
  as.formula(paste("cbind(D, R) ~", paste(rhs, collapse = " + ")))
}

fit_skew <- function(w, family = "olre", skew = "candidate", skew_var = "pooled") {
  fam <- if (family == "olre") binomial(link = "logit") else betabinomial(link = "logit")
  glmmTMB(model_formula(family, skew, uniqueN(w$election), skew_var),
          data = w, family = fam)
}

# The fits of a run. The main fit (the contest model with a separate skew
# variance per election) gives the race skews and the precinct leans; the
# others are kept for comparison in model_summary.csv.
fit_all <- function(w) {
  list(
    cand_olre                = fit_skew(w, "olre", "candidate"),
    cand_bb                  = fit_skew(w, "betabinomial", "candidate"),
    contest_olre             = fit_skew(w, "olre", "contest"),
    contest_bb               = fit_skew(w, "betabinomial", "contest"),
    contest_olre_by_election = fit_skew(w, "olre", "contest", "by_election")
  )
}
MAIN_FIT <- "contest_olre_by_election"

variance_components <- function(fit) {
  vc <- VarCorr(fit)$cond
  out <- rbindlist(lapply(names(vc), function(g) {
    m <- vc[[g]]
    data.table(term = if (nrow(m) == 1) g else paste0(g, " ", rownames(m)),
               sd = sqrt(diag(m)))
  }))
  if (family(fit)$family == "betabinomial")
    out <- rbind(out, data.table(term = "betabinomial phi", sd = sigma(fit)))
  out
}

# Random effects with conditional SDs for one grouping factor. For the
# per-election contest term, each contest keeps the column of its own election
# (contest keys start with the election date).
ranef_table <- function(fit, group) {
  r <- as.data.table(as.data.frame(ranef(fit, condVar = TRUE)))[grpvar == group]
  r[, grp := as.character(grp)]
  if (uniqueN(r$term) > 1)
    r <- r[as.character(term) == paste0("election", substr(grp, 1, 10))]
  r[, .(level = grp, est = condval, se = condsd)]
}

# Precinct lean beta_e + u_i from a fitted contest model, keyed by prec_key.
precinct_lean <- function(fit, w) {
  u <- ranef_table(fit, "prec_key")[, .(prec_key = level, u = est)]
  fe <- fixef(fit)$cond
  u <- merge(u, unique(w[, .(prec_key, election)]), by = "prec_key")
  u[, beta := if (length(fe) == 1) fe[[1]] else fe[paste0("election", election)]]
  u[, .(prec_key, lean = beta + u)]
}

# Candidate effects d_k and r_k, both on the scale "positive = the candidate
# did better than the party baseline", with each candidate's number of
# contests and opponents and component of the candidate-contest graph.
candidate_effects <- function(fit, w) {
  d <- ranef_table(fit, "candD")[, party := "DEM"]
  r <- ranef_table(fit, "candR")[, `:=`(party = "REP", est = -est)]
  out <- rbind(d, r)
  setnames(out, "level", "person_id")
  n_contests <- rbind(w[, .(person_id = candD, contest_key)],
                      w[, .(person_id = candR, contest_key)])[
    , .(n_contests = uniqueN(contest_key)), by = person_id]
  out <- merge(out, n_contests, by = "person_id")
  out <- merge(out, candidate_components(w), by = c("person_id", "party"))
  out <- merge(out, candidate_opponents(w), by = "person_id")
  out[order(component, party, -est)]
}

# Contest skew d - r from the candidate model, and s_j from the contest model.
# The SE from the candidate model ignores the posterior correlation of d and
# r, so it overstates the uncertainty of their difference.
contest_skews <- function(fit_cand, fit_contest, w) {
  cs <- w[, .(precincts = .N, votes = sum(n), dem_share = sum(D) / sum(n)),
          by = .(contest_key, contest, candD, candR)]
  d <- ranef_table(fit_cand, "candD")
  r <- ranef_table(fit_cand, "candR")
  cs[d, on = .(candD = level), `:=`(d = i.est, d_se = i.se)]
  cs[r, on = .(candR = level), `:=`(r = -i.est, r_se = i.se)]
  cs[, skew := d - r]
  k <- ranef_table(fit_contest, "contest_key")
  cs[k, on = .(contest_key = level), `:=`(skew_contest = i.est, skew_contest_se = i.se)]
  cs[order(-skew)]
}
