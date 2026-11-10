# Registry of elections. The name of each entry (the election date) is the
# election label used everywhere else. Fields:
#   type    as the county names the election: general, primary, special,
#           special-primary, or general-district (a local-district election)
#   file    file name in DATA_DIR, named <date>_<type>.<csv|txt> after its
#           actual format
#   layout  entry of LAYOUTS in layouts.R
#   rename  optional: standard name -> file column, where the file differs
#           from its layout
#   role    main: a November general election, fitted by run.R;
#           secondary: a primary, odd-year, or special election, estimated
#           by the second stage (R/secondary.R, run.R --secondary) against
#           the precinct leans of the main fit; never part of the main fit
#   anchor  secondary only: the general election whose precinct leans the
#           second stage uses, chosen by the share of ballots cast in precinct
#           numbers that also exist in that general (see PLAN.md); the same
#           year's general except June 2026, anchored to November 2024 until
#           November 2026 is available
#   ready   FALSE keeps a main election out of a run with no arguments; 2012
#           and 2014 wait for hand-entered party labels in candidates.csv
ELECTIONS <- list(
  "2012-11-06" = list(type = "general", file = "2012-11-06_general.csv",
                      layout = "csv_2012", role = "main", ready = FALSE),
  "2014-06-03" = list(type = "primary", file = "2014-06-03_primary.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2014-11-04"),
  "2014-11-04" = list(type = "general", file = "2014-11-04_general.csv",
                      layout = "csv_2012", role = "main", ready = FALSE),
  "2015-03-17" = list(type = "special-primary", file = "2015-03-17_special-primary.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2014-11-04"),
  "2016-06-07" = list(type = "primary", file = "2016-06-07_primary.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2016-11-08"),
  "2016-11-08" = list(type = "general", file = "2016-11-08_general.csv",
                      layout = "csv_2012", role = "main", ready = TRUE),
  "2018-06-05" = list(type = "primary", file = "2018-06-05_primary.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2018-11-06"),
  "2018-11-06" = list(type = "general", file = "2018-11-06_general.csv",
                      layout = "csv_2012", role = "main", ready = TRUE),
  "2019-03-12" = list(type = "special", file = "2019-03-12_special.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2018-11-06"),
  "2019-11-05" = list(type = "general-district", file = "2019-11-05_general-district.csv",
                      layout = "csv_2012", role = "secondary",
                      anchor = "2018-11-06"),
  "2020-03-03" = list(type = "primary", file = "2020-03-03_primary.csv",
                      layout = "csv_2020_party_split", role = "secondary",
                      anchor = "2020-11-03"),
  "2020-11-03" = list(type = "general", file = "2020-11-03_general.csv",
                      layout = "csv_2020", role = "main", ready = TRUE),
  "2021-03-09" = list(type = "special", file = "2021-03-09_special.csv",
                      layout = "csv_2020", role = "secondary",
                      anchor = "2020-11-03"),
  "2022-06-07" = list(type = "primary", file = "2022-06-07_primary.txt",
                      layout = "tab_2022", role = "secondary",
                      anchor = "2022-11-08"),
  "2022-11-08" = list(type = "general", file = "2022-11-08_general.txt",
                      layout = "tab_2022", role = "main", ready = TRUE),
  "2024-03-05" = list(type = "primary", file = "2024-03-05_primary.txt",
                      layout = "tab_2024_party_split", role = "secondary",
                      anchor = "2024-11-05"),
  "2024-11-05" = list(type = "general", file = "2024-11-05_general.txt",
                      layout = "tab_2022", role = "main", ready = TRUE,
                      rename = c(choice = "Choice Name1")),
  "2025-02-25" = list(type = "special-primary", file = "2025-02-25_special-primary.txt",
                      layout = "tab_2022", role = "secondary",
                      anchor = "2024-11-05"),
  "2025-11-04" = list(type = "special", file = "2025-11-04_special.txt",
                      layout = "tab_2022", role = "secondary",
                      anchor = "2024-11-05",
                      rename = c(choice = "Choice Name1")),
  "2026-06-02" = list(type = "primary", file = "2026-06-02_primary.txt",
                      layout = "tab_2022", role = "secondary",
                      anchor = "2024-11-05",
                      rename = c(choice = "Choice Name1"))
)

# Main elections that a run with no arguments fits.
ready_elections <- function() {
  names(Filter(function(e) e$role == "main" && isTRUE(e$ready), ELECTIONS))
}

secondary_elections <- function() {
  names(Filter(function(e) e$role == "secondary", ELECTIONS))
}
