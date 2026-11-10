# Hand-edited lookup files (candidates.csv, nonpartisan.csv): one row per
# candidate line, keyed by KEY, all columns character.

# Base R's reader is used because data.table::fread leaves doubled quotes ("")
# inside quoted fields unescaped, so names such as STEVEN "STEVE" CHOI would
# gain quotes on every read and write.
read_lookup <- function(path) {
  as.data.table(utils::read.csv(path, colClasses = "character",
                                na.strings = character(), check.names = FALSE,
                                encoding = "UTF-8"))
}

write_lookup <- function(dt, path) {
  setorderv(dt, KEY)
  fwrite(dt, path)
  invisible(dt)
}

# Merge freshly derived rows `new` into the lookup at `path`. For rows already
# in the file, the columns in `keep` keep their hand-edited values; all other
# columns are refreshed from `new`. Rows in the file that are not in `new`
# (for example, elections not loaded in this run) are kept unchanged.
merge_lookup <- function(new, path, keep) {
  if (file.exists(path)) {
    old <- read_lookup(path)
    if (length(keep)) new[old, on = KEY, (keep) := mget(paste0("i.", keep))]
    new <- rbind(old[!new, on = KEY], new)
  }
  write_lookup(new, path)
}
