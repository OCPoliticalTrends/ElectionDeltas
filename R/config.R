# Paths and constants shared by the modules. Paths are relative to the project
# folder.

DATA_DIR         <- "data"             # raw statement-of-votes extracts
OUTPUT_DIR       <- "output"           # one subfolder per run, named by years
CANDIDATES_FILE  <- "candidates.csv"   # hand-edited candidate lookup
NONPARTISAN_FILE <- "nonpartisan.csv"  # hand-designated nonpartisan races

# Key of one candidate line in one contest of one election, in the long table
# and in both lookup files.
KEY <- c("election", "contest_id", "choice_id")

# Pseudo-precinct 99999 (2022 and 2024) pools small precincts differently for
# each contest, so it has no stable lean and is dropped (see PLAN.md).
DROP_PRECINCTS <- "99999"

# Races that one precinct lean predicts poorly are flagged in the results (see
# fit_flag() in diagnostics.R).
POOR_FIT <- list(min_R2 = 0.75, slope = c(0.75, 1.33))

# A designated nonpartisan race is flagged as vote-split when its two
# designated candidates hold less than this share of all its votes: the other
# candidates take votes from one side, so the two-candidate share no longer
# measures a head-to-head (see vote_split_flag() in nonpartisan.R).
VOTE_SPLIT_MIN <- 0.9

# Worker processes for the leave-one-contest-out refits.
N_CORES <- max(1L, parallel::detectCores() - 1L)
