# How well one precinct lean predicts a race.

# Empirical logit of the Democratic share.
elogit <- function(D, R) log((D + 0.5) / (R + 0.5))

# Share of the n-weighted variance of `total` (deviations from the weighted
# mean) left unexplained by `resid`, subtracted from 1.
weighted_R2 <- function(resid, total, wt) {
  wv <- function(z) sum(wt * z^2) / sum(wt)
  1 - wv(resid) / wv(total)
}

# Leave-one-contest-out check. For each contest, refit the contest model on
# the other contests of the same election, take the precinct lean learned from
# them, and ask how well it predicts the held-out contest's precinct-level
# shares once the contest is allowed a single shift (the skew), and then a
# shift plus a slope. The refits run in `cores` worker processes.
leave_one_contest_out <- function(w, cores = N_CORES) {
  w <- copy(w)[, el := elogit(D, R)]
  one <- function(k) {
    test <- w[contest_key == k]
    train <- w[contest_key != k & election == test$election[1]]
    f <- fit_skew(train, "olre", "contest")
    u <- ranef_table(f, "prec_key")
    test <- merge(test, u[, .(prec_key = level, u = est)], by = "prec_key")
    test[, lean := fixef(f)$cond[1] + u]
    wt <- test$n
    shift <- weighted.mean(test$el - test$lean, wt)
    slope_fit <- lm(el ~ lean, data = test, weights = wt)
    r_shift <- test$el - test$lean - shift
    r_slope <- residuals(slope_fit)
    tot <- test$el - weighted.mean(test$el, wt)
    wv <- function(z) sum(wt * z^2) / sum(wt)
    data.table(contest_key = k, election = substr(k, 1, 4), contest = test$contest[1],
               precincts = nrow(test), shift = shift, slope = coef(slope_fit)[2],
               sd_between = sqrt(wv(tot)),
               sd_resid_shift = sqrt(wv(r_shift)),
               sd_resid_slope = sqrt(wv(r_slope)),
               R2_shift = weighted_R2(r_shift, tot, wt),
               R2_slope = weighted_R2(r_slope, tot, wt))
  }
  keys <- unique(w$contest_key)
  res <- if (cores > 1) parallel::mclapply(keys, one, mc.cores = cores) else lapply(keys, one)
  failed <- vapply(res, inherits, logical(1), "try-error")
  if (any(failed)) stop("Leave-one-contest-out refit failed for ",
                        paste(keys[failed], collapse = ", "), ": ", res[failed][[1]])
  rbindlist(res)
}

# How a race's precinct-level shares follow a given prediction. `t` has one
# row per precinct with D, R, n, and `offset` (the predicted logit). Fits
#   logit(pi_i) = offset_i + shift          (quasi-binomial)
#   logit(pi_i) = a + slope * offset_i      (quasi-binomial)
# and returns the shift with its SE, the slope with its SE, and the R^2 of the
# empirical logits after each fit.
race_fit_stats <- function(t) {
  f0 <- glm(cbind(D, R) ~ 1 + offset(offset), data = t, family = quasibinomial())
  f1 <- glm(cbind(D, R) ~ offset, data = t, family = quasibinomial())
  el <- elogit(t$D, t$R)
  wt <- t$n
  tot <- el - weighted.mean(el, wt)
  shift <- unname(coef(f0)[1])
  list(shift = shift, shift_se = sqrt(vcov(f0)[1, 1]),
       slope = unname(coef(f1)[2]), slope_se = sqrt(vcov(f1)[2, 2]),
       R2_shift = weighted_R2(el - t$offset - shift, tot, wt),
       R2_slope = weighted_R2(el - predict(f1), tot, wt))
}

# A race is flagged when one precinct lean predicts it poorly: its R^2 after a
# single shift is below POOR_FIT$min_R2, or its slope on the lean lies outside
# POOR_FIT$slope. For a partisan race the slope and R^2 come from the
# leave-one-contest-out check; for a designated nonpartisan race, from its own
# regressions. A flagged race's skew averages over a pattern that the D/R
# reading fits poorly.
fit_flag <- function(slope, R2) {
  ifelse(R2 < POOR_FIT$min_R2 | slope < POOR_FIT$slope[1] | slope > POOR_FIT$slope[2],
         "poor partisan fit", "")
}
