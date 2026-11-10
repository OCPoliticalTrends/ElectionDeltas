# Tests of the rules documented in PLAN.md and in the module comments.
#
#   Rscript tests/run_tests.R      (from the project folder)
#
# The tests build small made-up inputs in a temporary folder; they read no
# election data and fit no models. Base R only.

source("R/setup.R")

failures <- 0L
check <- function(name, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    message("  error: ", conditionMessage(e)); FALSE
  })
  cat(if (ok) "PASS" else "FAIL", name, "\n")
  if (!ok) failures <<- failures + 1L
}
errors <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
tmp <- tempfile("skew-tests-"); dir.create(tmp)

# Accented test names are built from UTF-8 bytes: under the C locale, a
# \u escape in a script file is parsed into the literal text "<c3><81>".
utf8 <- function(...) { s <- rawToChar(as.raw(c(...))); Encoding(s) <- "UTF-8"; s }
SANCHEZ <- paste0("LINDA T. S", utf8(0xc3, 0x81), "NCHEZ")

# ---- Person IDs --------------------------------------------------------------
check("person ID drops punctuation and nickname quotes",
      all(default_person_id(c('STEVEN "STEVE" CHOI', "RAUL ORTIZ, JR.", "SHARON QUIRK-SILVA")) ==
            c("STEVEN_STEVE_CHOI", "RAUL_ORTIZ_JR", "SHARON_QUIRK_SILVA")))
check("person ID transliterates accents",
      default_person_id(SANCHEZ) == "LINDA_T_SANCHEZ")
check("hyphen-for-accent spelling needs a hand link (IDs differ)",
      default_person_id("LINDA T. S-NCHEZ") != default_person_id(SANCHEZ))

# ---- Lookup files --------------------------------------------------------------
lk <- data.table(election = "2024-11-05", contest_id = "1", choice_id = c("1", "2"),
                 choice = c('STEVEN "STEVE" CHOI', "RAUL ORTIZ, JR."),
                 party = c("REP", ""), note = "")
p <- file.path(tmp, "lookup.csv")
write_lookup(copy(lk), p)
for (i in 1:3) write_lookup(read_lookup(p), p)
check("lookup round trip keeps quotes and commas unchanged", identical(read_lookup(p)$choice, lk$choice))

fresh <- data.table(election = "2024-11-05", contest_id = "1", choice_id = c("1", "3"),
                    choice = c("STEVEN CHOI (renamed in file)", "NEW PERSON"),
                    party = c("DEM", "DEM"), note = "")
old_other <- data.table(election = "2022-11-08", contest_id = "9", choice_id = "9",
                        choice = "OTHER", party = "REP", note = "hand")
write_lookup(rbind(copy(lk), old_other), p)
m <- merge_lookup(fresh, p, keep = c("party", "note"))
check("merge keeps hand-edited columns of existing rows", m[choice_id == "1", party] == "REP")
check("merge refreshes file-derived columns", m[choice_id == "1", choice] == "STEVEN CHOI (renamed in file)")
check("merge adds new rows", m[choice_id == "3", party] == "DEM")
check("merge keeps rows of elections not loaded", m[election == "2022-11-08", note] == "hand")

# ---- Vote totals check -----------------------------------------------------------
tc <- check_total("T", c("a", "b"))
check("total check passes when totals add up",
      is.null(tc(data.table(T = c("3", "4"), a = c("1", "2"), b = c("2", "2")))))
check("total check stops when a total differs",
      errors(tc(data.table(T = c("3", "5"), a = c("1", "2"), b = c("2", "2")))))

# ---- Reading each layout -----------------------------------------------------------
fx <- file.path(tmp, "data"); dir.create(fx)
writeLines(c(
  '"Precinct_Name","Split_Name","precinct_splitId","Reg_voters","Ballots","Reporting","Contest_id","Contest_title","Contest_party","Choice_id","Candidate_name","Choice_party","Candidate_Type","Absentee_votes","Early_votes","Election_Votes"',
  '"02001","","1","100","80","1","10","PRESIDENT","","5","BERNARD ""BERNIE"" SANDERS","","W","1","0","2"',
  paste0('"02001","","1","100","80","1","10","PRESIDENT","","6","', SANCHEZ, '","DEM","C","10","1","20"')),
  file.path(fx, "csv2012.csv"), useBytes = TRUE)
con <- file(file.path(fx, "csv2020.csv"), "wb")
writeBin(iconv(paste0(c(
  "#FormatVersion 1",
  '#Precinct,Precinct ID,Precinct Reporting,"Registered Voters","Ballots Cast",Contest Title,Contest ID,Choice Name,Choice ID,Choice Party,Total Votes,"Early Vote Center Votes","Election Day Vote Center Votes","Vote by Mail Votes"',
  paste0('"02001",1,Reporting   ,100,80,"X",10,"', SANCHEZ, '",7,"DEM",6,1,2,3'),
  '"02001",1,Reporting   ,100,80,"X",10,"Unassigned write-ins",0,"",1,0,0,1'), collapse = "\n"),
  from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]], con)
close(con)
writeLines(c(
  paste(".Precinct", "Registered Voters", "Ballots Cast", "Contest Title", "Contest ID",
        "Choice Name", "Choice ID", "Choice Party", "Total Votes", "Vote Center Votes",
        "Vote by Mail Votes", sep = "\t"),
  paste("02001", "100", "80", "X", "10", "LINDA T. S-NCHEZ", "7", "DEM", "5", "2", "3", sep = "\t")),
  file.path(fx, "tab2022.txt"))
pp <- c(paste(PARTY_GROUPS, "Voters"), paste(PARTY_GROUPS, "Ballots Cast"))
writeLines(c(
  paste(c(".Precinct", pp, "Contest Title", "Contest ID", "Choice Name", "Choice ID", "Choice Party",
          "Total Votes", "Vote Center Votes", "Vote by Mail Votes", "Ballots_cast"), collapse = "\t"),
  paste(c("02001", 1:7, c(1, 1, 0, 0, 0, 0, 2), "X", "10", "A", "7", "DEM", "5", "2", "3", "4"),
        collapse = "\t")),
  file.path(fx, "tab2024pp.txt"))
saved <- ELECTIONS
ELECTIONS <- list(
  "2016-11-08" = list(type = "general", file = "csv2012.csv", layout = "csv_2012", ready = TRUE),
  "2020-11-03" = list(type = "general", file = "csv2020.csv", layout = "csv_2020", ready = TRUE),
  "2024-11-05" = list(type = "general", file = "tab2022.txt", layout = "tab_2022", ready = TRUE),
  "2024-03-05" = list(type = "primary", file = "tab2024pp.txt", layout = "tab_2024_party_split"))
pr <- read_election("2024-03-05", fx)
check("party-split layout: registered voters and ballots are sums over parties",
      pr$registered == 28L && pr$ballots == 4L)
mix <- read_elections(c("2024-11-05", "2024-03-05"), fx)
check("elections of different layouts stack by column name",
      identical(mix$registered, c(100L, 28L)) && identical(mix$votes, c(5L, 5L)))
check("party-split layout: Ballots_cast must equal the sum of party ballots", {
  x <- readLines(file.path(fx, "tab2024pp.txt"))
  x[2] <- sub("\t4$", "\t5", x[2]); writeLines(x, file.path(fx, "tab2024pp.txt"))
  errors(read_election("2024-03-05", fx))
})
a <- read_election("2016-11-08", fx)
check("2012-2018 layout: votes are the sum of three columns", identical(a$votes, c(3L, 31L)))
check("2012-2018 layout: Candidate_Type W marks write-ins", identical(a$writein, c(TRUE, FALSE)))
check("2012-2018 layout: doubled quotes are restored", a$choice[1] == 'BERNARD "BERNIE" SANDERS')
check("2012-2018 layout: UTF-8 accents are kept", a$choice[2] == SANCHEZ)
b <- read_election("2020-11-03", fx)
check("2020 layout: Latin-1 text is converted to UTF-8", b$choice[1] == SANCHEZ)
check("2020 layout: choice ID 0 marks write-ins", identical(b$writein, c(FALSE, TRUE)))
c3 <- read_election("2024-11-05", fx)
check("2022-2024 layout: tab-separated file reads", c3$votes == 5L && c3$precinct == "02001")
ELECTIONS <- saved

# ---- Contest selection -------------------------------------------------------------
mk <- function(contest_id, choice_id, party, votes, precinct = "1", writein = FALSE)
  data.table(election = "2024-11-05", precinct, contest_id, contest = paste("C", contest_id),
             choice_id, choice = paste("P", choice_id), party, writein, votes,
             registered = 100L, ballots = 90L)
long <- rbind(
  mk("1", c("a", "b", "c"), c("DEM", "REP", "GRN"), c(40L, 30L, 5L)),   # D vs R plus a third party
  mk("1", "w", "", 2L, writein = TRUE),                                  # write-in
  mk("1", c("a", "b"), c("DEM", "REP"), c(10L, 10L), precinct = "99999"),
  mk("1", c("a", "b"), c("DEM", "REP"), c(0L, 0L), precinct = "2"),      # no two-party votes
  mk("2", c("d", "e"), c("DEM", "DEM"), c(20L, 20L)),                    # same party
  mk("3", c("f", "g"), c("REP", ""), c(20L, 20L)),                       # no Democrat
  mk("4", c("h", "i", "j"), c("DEM", "REP", "REP"), c(20L, 20L, 5L)),     # two Republicans
  mk("5", c("k", "l", "m"), c("DEM", "REP", ""), c(20L, 20L, 5L)))        # unlabeled candidate
cand <- unique(long[writein == FALSE, .(election, contest_id, choice_id, party,
                                        person_id = paste0("ID_", choice_id))])
w <- build_dvr(long, cand)
check("only contests with exactly one D and one R are kept", identical(unique(w$contest_id), "1"))
check("third-party and write-in votes drop out of n", identical(w$n, 70L))
check("pseudo-precinct 99999 and empty rows are dropped", identical(w$precinct, "1"))
check("candidate IDs are attached to D and R", w$candD == "ID_a" && w$candR == "ID_b")

partisan <- data.table(election = "2024-11-05", precinct = "1", contest_id = c("1", "2"),
                       contest = c("CITY OF BREA City Treasurer", "MEMBER OF THE STATE ASSEMBLY 72nd District"),
                       choice_id = c("1", "2"), choice = c("A", "B"), party = "", writein = FALSE,
                       votes = 1L, registered = 1L, ballots = 1L)
pc <- update_candidates(partisan, file.path(tmp, "cands.csv"))
check("partisan-office titles match at the start only (city treasurer excluded)",
      identical(pc$contest_id, "2"))

# ---- Designated nonpartisan races -----------------------------------------------------
dv <- designated_vote_share(long, data.table(election = "2024-11-05", contest_id = "1",
                                              choice_id = c("a", "b", "c"), party = c("DEM", "REP", "")))
check("designated vote share counts all candidates and excludes write-ins and precinct 99999",
      dv$candidates == 3L && dv$designated_vote_share == round(70 / 75, 3))
check("vote-split flag fires below the threshold and not at or above it",
      identical(vote_split_flag(c(0.5, 0.899, 0.9, 1)) != "", c(TRUE, TRUE, FALSE, FALSE)))

# ---- Statewide propositions -----------------------------------------------------------
ml <- rbind(
  mk("p1", c("y", "n"), "", c(10L, 5L))[, `:=`(contest = "Proposition 13", choice = c("Yes", "No"))],
  mk("p2", c("y", "n"), "", c(10L, 5L))[, `:=`(contest = "51-SCHOOL BONDS. FUNDING.", choice = c("YES", "NO"))],
  mk("p3", c("y", "n"), "", c(10L, 5L))[, `:=`(contest = "A-City of Stanton, Cannabis Tax", choice = c("Yes", "No"))],
  mk("p4", c("y", "n"), "", c(10L, 5L))[, `:=`(contest = "For Associate Justice of the Supreme Court X", choice = c("Yes", "No"))],
  mk("p5", c("y", "n", "m"), "", c(10L, 5L, 1L))[, `:=`(contest = "Proposition 99", choice = c("Yes", "No", "Maybe"))])
md <- measure_designations(ml)
check("statewide propositions of both title styles are selected; local, judicial, and non-Yes/No are not",
      identical(sort(unique(md$contest_id)), c("p1", "p2")))
check("propositions start with Yes as the D side", all(md[toupper(choice) == "YES", party] == "DEM") &&
        all(md[toupper(choice) == "NO", party] == "REP"))
fl <- flip_designations(md, data.table(election = "2024-11-05", contest_id = "p2"))
check("flipping makes No the D side for the listed measure only",
      fl[contest_id == "p2" & toupper(choice) == "NO", party] == "DEM" &&
        fl[contest_id == "p1" & toupper(choice) == "YES", party] == "DEM")

# ---- Carrying confirmed links and display names ------------------------------------
cp <- file.path(tmp, "links.csv")
write_lookup(data.table(
  election = "2024-11-05", contest_id = "1", contest = "MEMBER OF THE STATE ASSEMBLY 1st District",
  choice_id = c("1", "2", "3", "4"),
  choice = c("LINDA T. S-NCHEZ", "STEVEN S. CHOI", "JANE DOE", "JANE DOE"),
  party_file = "DEM", party = "DEM",
  person_id = c("LINDA_T_SANCHEZ", "STEVEN_CHOI", "JANE_DOE", "JANE_DOE_2"), note = "confirmed"), cp)
prim <- data.table(election = "2026-06-02", precinct = "1", contest_id = "9",
                   contest = "MEMBER OF THE STATE ASSEMBLY 1st District", choice_id = c("1", "2", "3"),
                   choice = c("LINDA T. S-NCHEZ", "JANE DOE", "STEVEN S CHOI"), party = "DEM",
                   writein = FALSE, votes = 1L, registered = 1L, ballots = 1L)
ELECTIONS[["2026-06-02"]] <- list(type = "primary", role = "secondary", anchor = "2024-11-05")
ELECTIONS[["2024-11-05"]]$role <- "main"
lk2 <- update_candidates(prim, cp)
check("a new row with the same ballot name as a confirmed link takes that person",
      lk2[election == "2026-06-02" & choice == "LINDA T. S-NCHEZ", person_id] == "LINDA_T_SANCHEZ" &&
        lk2[election == "2026-06-02" & choice == "STEVEN S CHOI", note] == CARRIED_NOTE)
check("a ballot name linked to two people is not carried",
      lk2[election == "2026-06-02" & choice == "JANE DOE", person_id] == "JANE_DOE")
check("existing rows keep their hand-edited links",
      identical(lk2[election == "2024-11-05", person_id],
                c("LINDA_T_SANCHEZ", "STEVEN_CHOI", "JANE_DOE", "JANE_DOE_2")))
nm <- candidate_names(rbind(lk2, data.table(election = "2026-06-02", contest_id = "9", contest = "",
  choice_id = "8", choice = "ONLY IN PRIMARY", party_file = "", party = "", person_id = "ONLY", note = "")))
check("display names come from general elections first",
      nm[person_id == "STEVEN_CHOI", name] == "STEVEN S. CHOI" && nm[person_id == "ONLY", name] == "ONLY IN PRIMARY")

# ---- Second stage --------------------------------------------------------------------
check("second stage uses secondary elections whose anchor is in the run",
      { p <- secondary_plan("2024-11-05"); "2026-06-02" %in% p$use && !"2026-06-02" %in% p$skipped })
check("second stage skips secondary elections whose anchor is not in the run",
      "2026-06-02" %in% secondary_plan("2022-11-08")$skipped)
check("second stage: one shift and slope per election, or one of each for a single election",
      grepl("0 + election + election:lean", deparse1(secondary_formula(2)), fixed = TRUE) &&
        grepl("1 + lean", deparse1(secondary_formula(1)), fixed = TRUE))
ELECTIONS <- saved
set.seed(1)
syn <- data.table(offset = seq(-1.5, 1.5, length.out = 60), n = 4000L)
syn[, D := as.integer(round(n * plogis(offset + 0.2)))][, R := n - D]
st <- race_fit_stats(syn)
check("race fit statistics recover a known shift and slope",
      abs(st$shift - 0.2) < 0.01 && abs(st$slope - 1) < 0.02 && st$R2_shift > 0.99)

# ---- Candidate graph ---------------------------------------------------------------
g <- data.table(candD = c("A", "A", "C"), candR = c("X", "Y", "Z"))
cc <- candidate_components(g)
check("candidates linked through shared contests share a component",
      cc[person_id == "X", component] == cc[person_id == "Y", component] &&
        cc[person_id == "A", component_size] == 3L)
check("unlinked contests form separate components",
      cc[person_id == "C", component] != cc[person_id == "A", component])

# ---- Model formula and fit flag ------------------------------------------------------
check("one election: a single intercept",
      grepl("~ 1 +", deparse1(model_formula("olre", "contest", 1)), fixed = TRUE))
check("several elections: one intercept per election and per-election skew variance",
      grepl("0 + election", deparse1(model_formula("olre", "contest", 3, "by_election")), fixed = TRUE) &&
        grepl("diag(0 + election | contest_key)",
              deparse1(model_formula("olre", "contest", 3, "by_election")), fixed = TRUE))
check("per-election skew variance is refused for the candidate model",
      errors(model_formula("olre", "candidate", 3, "by_election")))
check("fit flag fires on each threshold and not inside them",
      identical(fit_flag(c(1, 0.3, 1.4, 1, 0.76), c(0.9, 0.9, 0.9, 0.7, 0.76)) != "",
                c(FALSE, TRUE, TRUE, TRUE, FALSE)))

unlink(tmp, recursive = TRUE)
cat(if (failures) paste(failures, "test(s) failed\n") else "All tests passed\n")
quit(status = as.integer(failures > 0))
