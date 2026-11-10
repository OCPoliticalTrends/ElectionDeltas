# Fit the skew model to one or more elections.
#
#   Rscript run.R                         every election marked ready in R/elections.R
#   Rscript run.R 2024-11-05              one election
#   Rscript run.R 2022-11-08 2024-11-05   several
#   Rscript run.R --cores 4 ...           worker processes for the leave-one-out refits
#
# Updates candidates.csv (the hand-edited candidate lookup) and writes, to
# output/<years>/ (for example output/2022_2024/), replacing the last run's
# files:
#   results.csv                one row per race: skew, both candidates' effects,
#                              leave-one-contest-out slope and R^2, fit flag
#   candidate_effects.csv      one row per candidate
#   model_summary.csv          convergence, AIC, intercepts, variance components
#   leave_one_contest_out.csv  how well one precinct lean predicts each race
#   nonpartisan_skews.csv      skews of the races designated in nonpartisan.csv
#                              (see R/nonpartisan.R)
# Run long jobs with the console log outside OneDrive, which has truncated logs
# written inside the project folder.

source("R/setup.R")

args <- commandArgs(trailingOnly = TRUE)
cores <- N_CORES
if (length(i <- which(args == "--cores"))) {
  cores <- as.integer(args[i + 1])
  args <- args[-c(i, i + 1)]
}
elections <- sort(if (length(args)) args else ready_elections())
out_dir <- file.path(OUTPUT_DIR, paste(substr(elections, 1, 4), collapse = "_"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

long <- read_elections(elections)
cand <- update_candidates(long)
w <- build_dvr(long, cand)

fits <- fit_all(w)
main <- fits[[MAIN_FIT]]
loco <- leave_one_contest_out(w, cores)
cs <- contest_skews(fits$cand_olre, main, w)
ce <- candidate_effects(fits$cand_olre, w)
np <- if (file.exists(NONPARTISAN_FILE))
  nonpartisan_skews(long, read_lookup(NONPARTISAN_FILE), main, w)

fwrite(race_table(cs, ce, cand, loco), file.path(out_dir, "results.csv"))
fwrite(candidate_table(ce, cand), file.path(out_dir, "candidate_effects.csv"))
fwrite(model_summary(fits), file.path(out_dir, "model_summary.csv"))
fwrite(loco, file.path(out_dir, "leave_one_contest_out.csv"))
if (!is.null(np)) fwrite(np, file.path(out_dir, "nonpartisan_skews.csv"))

print_summary(w, fits, loco, cs, ce, np)
