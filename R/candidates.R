# The candidate lookup (candidates.csv) and the candidate-contest graph.
#
# candidates.csv has one row per candidate in a partisan contest. Columns:
#   election, contest_id, contest, choice_id, choice   copied from the results file
#   party_file  party label as printed in the results file
#   party       party used by the model (edit by hand where party_file is
#               blank or wrong)
#   person_id   stable ID linking one person across contests and elections
#   note        free text, for example the reason for a hand-made link

# Offices elected on a partisan ballot, matched at the start of the contest
# title so that city and district offices with the same name (for example
# "CITY OF BREA City Treasurer") are left out. Contests with these titles enter
# the candidate lookup even when the file carries no party labels (2012 and
# 2014). The titles are those of the 2016-2024 files; other files may word them
# differently.
PARTISAN_OFFICE <- paste0("^(", paste(
  "PRESIDENT", "UNITED STATES SENATOR", "UNITED STATES REPRESENTATIVE",
  "STATE SENATOR", "MEMBER OF THE STATE ASSEMBLY", "GOVERNOR",
  "LIEUTENANT GOVERNOR", "SECRETARY OF STATE", "CONTROLLER", "TREASURER",
  "ATTORNEY GENERAL", "INSURANCE COMMISSIONER",
  "MEMBER,? STATE BOARD OF EQUALIZATION", sep = "|"), ")")

# Columns of candidates.csv that are edited by hand and survive every run.
CANDIDATE_HAND_COLS <- c("party", "person_id", "note")

# Default person ID: the ballot name, uppercased, accents transliterated, with
# punctuation removed. Hand edits in the lookup replace it where one person's
# name is written differently across contests or years.
default_person_id <- function(name) {
  # Transliteration may render an accent as a separate mark (A with acute as
  # 'A); drop such marks before replacing other punctuation with spaces.
  x <- toupper(iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT"))
  x <- gsub("['`^~]", "", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", "_", trimws(gsub(" +", " ", x)))
}

# Add every candidate in a partisan contest of `long` to the lookup at `path`
# (contests with any party label, or with a partisan office title), keeping
# the hand-edited columns of rows already there.
update_candidates <- function(long, path = CANDIDATES_FILE) {
  labeled <- long[party != "", unique(paste(election, contest_id))]
  new <- unique(long[!writein & (paste(election, contest_id) %in% labeled |
                                   grepl(PARTISAN_OFFICE, toupper(contest))),
                     .(election, contest_id, contest, choice_id, choice,
                       party_file = party)])
  new[, party := party_file]
  new[, person_id := default_person_id(choice)]
  new[, note := ""]
  merge_lookup(new, path, CANDIDATE_HAND_COLS)
}

# Display name of each person: the ballot name in their latest election.
candidate_names <- function(cand) {
  cand[order(election), .(name = choice[.N]), by = person_id]
}

# Connected components of the graph whose nodes are candidates and whose edges
# are contests. Within a component the data identify differences between
# candidates of the same party; adding one constant to every d and every r in
# a component leaves every contest skew d - r unchanged, so the split between
# the parties within a component rests on the candidate variances.
candidate_components <- function(w) {
  e <- unique(w[, .(a = paste0("D:", candD), b = paste0("R:", candR))])
  nodes <- unique(c(e$a, e$b))
  comp <- setNames(seq_along(nodes), nodes)
  repeat {
    m <- pmin(comp[e$a], comp[e$b])
    new <- comp
    for (i in seq_along(m)) new[c(e$a[i], e$b[i])] <- pmin(new[c(e$a[i], e$b[i])], m[i])
    if (identical(new, comp)) break
    comp <- new
  }
  out <- data.table(node = names(comp), comp = as.integer(factor(comp)))
  out[, `:=`(party = ifelse(startsWith(node, "D:"), "DEM", "REP"),
             person_id = sub("^[DR]:", "", node))]
  out[, size := .N, by = comp]
  out[, .(person_id, party, component = comp, component_size = size)]
}

# Number of distinct opponents of each person.
candidate_opponents <- function(w) {
  rbind(w[, .(person_id = candD, opp = candR)], w[, .(person_id = candR, opp = candD)])[
    , .(n_opponents = uniqueN(opp)), by = person_id]
}
