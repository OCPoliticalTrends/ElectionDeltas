# Tables written to output/<years>/ after each run, and the console summary.
# Effects are on the logit scale, positive when the candidate ran ahead of the
# party baseline; *_pts columns convert to percentage points of the two-party
# share at the race's actual share p (multiplying by 100 p (1 - p)).

# results.csv: one row per race.
race_table <- function(cs, ce, cand, loco) {
  names <- candidate_names(cand)
  nm <- function(id) names$name[match(id, names$person_id)]
  race <- cs[, .(
    contest_key,
    election = substr(contest_key, 1, 10), contest,
    dem_candidate = nm(candD), rep_candidate = nm(candR),
    precincts, two_party_votes = votes,
    dem_share = round(dem_share, 4),
    race_skew = round(skew_contest, 3), race_skew_se = round(skew_contest_se, 3),
    race_skew_pts = round(100 * skew_contest * dem_share * (1 - dem_share), 1),
    dem_effect = round(d, 3), dem_effect_se = round(d_se, 3),
    rep_effect = round(r, 3), rep_effect_se = round(r_se, 3),
    candD, candR)]
  race[ce[party == "DEM"], on = .(candD = person_id),
       `:=`(dem_contests = i.n_contests, component = i.component)]
  race[ce[party == "REP"], on = .(candR = person_id), rep_contests := i.n_contests]
  race[loco, on = "contest_key", `:=`(loco_slope = round(i.slope, 3),
                                      loco_R2 = round(i.R2_shift, 3))]
  race[, fit_flag := fit_flag(loco_slope, loco_R2)]
  race[, c("contest_key", "candD", "candR") := NULL]
  setorder(race, election, -race_skew)
  race[]
}

# candidate_effects.csv: one row per candidate.
candidate_table <- function(ce, cand) {
  names <- candidate_names(cand)
  out <- ce[, .(name = names$name[match(person_id, names$person_id)], party,
                effect = round(est, 3), effect_se = round(se, 3),
                n_contests, n_opponents, component, component_size, person_id)]
  setorder(out, component, party, -effect)
  out[]
}

# model_summary.csv: convergence, AIC, intercepts, and variance components of
# every fit.
model_summary <- function(fits) {
  rbindlist(lapply(names(fits), function(m) {
    f <- fits[[m]]
    vc <- variance_components(f)
    rbind(data.table(model = m, term = c("converged", "AIC"),
                     value = c(f$sdr$pdHess, AIC(f))),
          data.table(model = m, term = paste0("intercept ", names(fixef(f)$cond)),
                     value = unname(fixef(f)$cond)),
          data.table(model = m, term = paste0("sd ", vc$term), value = vc$sd))
  }))[, value := ifelse(term == "AIC", round(value, 1), signif(value, 4))]
}

# ---- Console summary --------------------------------------------------------

round3 <- function(dt) dt[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)]

print_section <- function(title, x) {
  cat("\n==", title, "==\n")
  print(x)
}

print_summary <- function(w, fits, loco, cs, ce, np) {
  old <- options(width = 200, datatable.print.nrows = 200)
  on.exit(options(old))
  print_section("Data", w[, .(rows = .N, contests = uniqueN(contest_key),
                              precincts = uniqueN(prec_key),
                              votes_per_row = round(mean(n))), by = election])
  print_section("Fit summary", data.table(
    model = names(fits),
    converged = vapply(fits, function(f) f$sdr$pdHess, logical(1)),
    AIC = vapply(fits, AIC, numeric(1))))
  cat("\n== Variance components (SD, logit scale) ==\n")
  for (m in names(fits)) {
    cat(m, ":\n"); print(variance_components(fits[[m]]))
  }
  print_section("Leave-one-contest-out check", round3(
    loco[, .(election, contest = substr(contest, 1, 45), precincts, shift, slope,
             sd_resid_shift, R2_shift, R2_slope)]))
  print_section("Contest skew (positive = Democrat ran ahead of party baseline)", round3(
    cs[, .(election = substr(contest_key, 1, 4), contest = substr(contest, 1, 45),
           candD, candR, precincts, dem_share, skew = skew_contest, se = skew_contest_se)]))
  print_section("Components of the candidate-contest graph",
                unique(ce[, .(component, component_size)])[, .N, by = component_size][
                  order(-component_size)])
  print_section("Candidate effects in components of three or more (compare within party and component)",
                round3(ce[component_size > 2, .(component, party, person_id, n_contests,
                                                n_opponents, est, se)]))
  if (!is.null(np))
    print_section("Designated nonpartisan races",
                  np[, .(election, contest, dem_candidate, rep_candidate, dem_share,
                         race_skew, race_skew_se, race_skew_pts, slope, R2_shift, fit_flag)])
}
