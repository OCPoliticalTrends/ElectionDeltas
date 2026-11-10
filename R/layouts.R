# File layouts of the OC Vote statement-of-votes extracts.
#
# Each layout says how to read one family of files and how to map its columns
# onto the standard long table built by read_election() in load.R:
#
#   election, precinct, contest_id, contest, choice_id, choice, party,
#   writein, votes, registered, ballots
#
# A layout is a list with:
#   sep, skip            passed to data.table::fread
#   encoding             the file's text encoding, converted to UTF-8 on reading
#   cols                 named character vector: standard name -> file column
#   votes                function(dt) returning total votes per row
#   writein              function(dt) returning TRUE for write-in rows
#   registered, ballots  optional functions(dt), for files without a single
#                        column for them (cols then omits them)
#   check                optional function(dt) that stops on inconsistent data
#
# Choice IDs are read as character labels throughout. Which file uses which
# layout is set in elections.R.

# Sum of integer columns.
sum_cols <- function(dt, cols) Reduce(`+`, lapply(cols, function(p) as.integer(dt[[p]])))

# Stop if a total-votes column differs from the sum of its vote-method columns.
check_total <- function(total, parts) function(dt) {
  bad <- sum(sum_cols(dt, parts) != as.integer(dt[[total]]))
  if (bad > 0) stop(bad, " rows where ", total, " differs from the sum of vote methods")
}

LAYOUTS <- list(

  # 2012-2018: comma-separated, UTF-8 (accented names such as SÁNCHEZ are
  # encoded that way). Votes are split into three columns with no total.
  # Write-ins have Candidate_Type W. Precinct IDs have 7 digits in 2012 and
  # 2014 and 5 digits from 2016 on.
  csv_2012 = list(
    sep = ",", skip = 0, encoding = "UTF-8",
    cols = c(
      precinct   = "Precinct_Name",
      contest_id = "Contest_id",
      contest    = "Contest_title",
      choice_id  = "Choice_id",
      choice     = "Candidate_name",
      party      = "Choice_party",
      registered = "Reg_voters",
      ballots    = "Ballots"
    ),
    votes = function(dt) as.integer(dt[["Absentee_votes"]]) +
      as.integer(dt[["Early_votes"]]) + as.integer(dt[["Election_Votes"]]),
    writein = function(dt) dt[["Candidate_Type"]] == "W"
  ),

  # 2020: comma-separated after a first line "#FormatVersion 1", Latin-1.
  # Total Votes is the sum of the Early Vote Center, Election Day Vote Center,
  # and Vote by Mail columns. Write-in lines carry choice ID 0.
  csv_2020 = list(
    sep = ",", skip = 1, encoding = "latin1",
    cols = c(
      precinct   = "#Precinct",
      contest_id = "Contest ID",
      contest    = "Contest Title",
      choice_id  = "Choice ID",
      choice     = "Choice Name",
      party      = "Choice Party",
      registered = "Registered Voters",
      ballots    = "Ballots Cast"
    ),
    votes = function(dt) as.integer(dt[["Total Votes"]]),
    writein = function(dt) dt[["Choice ID"]] == "0",
    check = check_total("Total Votes", c("Early Vote Center Votes",
                                         "Election Day Vote Center Votes",
                                         "Vote by Mail Votes"))
  ),

  # 2022-2024: tab-separated (the county publishes them with a .txt
  # extension). Total Votes is the sum of the Vote Center and Vote by Mail
  # columns. Write-in lines (named, unassigned, and rejected) carry choice ID 0.
  # Choice IDs for "No" on ballot measures are negative. The files are ASCII;
  # accented letters are replaced by a hyphen (LINDA T. S-NCHEZ).
  tab_2022 = list(
    sep = "\t", skip = 0, encoding = "UTF-8",
    cols = c(
      precinct   = ".Precinct",
      contest_id = "Contest ID",
      contest    = "Contest Title",
      choice_id  = "Choice ID",
      choice     = "Choice Name",
      party      = "Choice Party",
      registered = "Registered Voters",
      ballots    = "Ballots Cast"
    ),
    votes = function(dt) as.integer(dt[["Total Votes"]]),
    writein = function(dt) dt[["Choice ID"]] == "0",
    check = check_total("Total Votes", c("Vote Center Votes", "Vote by Mail Votes"))
  )
)

# Presidential primaries (2020, 2024): registration and turnout are split by
# party ("DEM Voters", "DEM Ballots Cast", and so on for seven groups), with no
# single column for either; registered voters and ballots are their sums.
PARTY_GROUPS <- c("DEM", "REP", "AI", "GRN", "LIB", "P/F", "N/P")
party_split <- function(base, ...) {
  lay <- modifyList(LAYOUTS[[base]], list(...))
  lay$cols <- lay$cols[setdiff(names(lay$cols), c("registered", "ballots"))]
  lay$registered <- function(dt) sum_cols(dt, paste(PARTY_GROUPS, "Voters"))
  lay$ballots <- function(dt) sum_cols(dt, paste(PARTY_GROUPS, "Ballots Cast"))
  lay
}

# March 3, 2020: as csv_2020, but with the vote-method columns named
# "Early VC Votes" and "Election Days VC Votes".
LAYOUTS$csv_2020_party_split <- party_split(
  "csv_2020",
  check = check_total("Total Votes", c("Early VC Votes", "Election Days VC Votes",
                                       "Vote by Mail Votes")))

# March 5, 2024: as tab_2022. Its total Ballots_cast column must equal the sum
# of the party ballots. Write-ins are identified as in tab_2022 (choice ID 0);
# the presidential candidates of the party contests also carry choice ID 0, but
# those contests are not D-vs-R contests and never enter the model.
LAYOUTS$tab_2024_party_split <- party_split("tab_2022")
LAYOUTS$tab_2024_party_split$check <- function(dt) {
  LAYOUTS$tab_2022$check(dt)
  bad <- sum(sum_cols(dt, paste(PARTY_GROUPS, "Ballots Cast")) != as.integer(dt[["Ballots_cast"]]))
  if (bad > 0) stop(bad, " rows where Ballots_cast differs from the sum of party ballots")
}
