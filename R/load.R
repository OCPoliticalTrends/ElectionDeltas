# Read election files into the standard long table described in layouts.R.

read_election <- function(election, data_dir = DATA_DIR) {
  spec <- ELECTIONS[[election]]
  if (is.null(spec)) stop("No entry in ELECTIONS for ", election)
  lay <- LAYOUTS[[spec$layout]]
  if (is.null(lay)) stop("Layout ", spec$layout, " is not implemented")

  cols <- lay$cols
  if (!is.null(spec$rename)) cols[names(spec$rename)] <- spec$rename

  raw <- fread(file.path(data_dir, spec$file), sep = lay$sep, skip = lay$skip,
               colClasses = "character", strip.white = TRUE)
  missing <- setdiff(cols, names(raw))
  if (length(missing)) stop("Columns not found in ", spec$file, ": ",
                            paste(missing, collapse = ", "))
  if (!is.null(lay$check)) lay$check(raw)

  out <- raw[, cols, with = FALSE]
  setnames(out, names(cols))
  # Convert text to UTF-8 from the file's encoding (fread marks nothing when R
  # runs in the C locale). fread can also leave the doubled quotes of a quoted
  # CSV field undecoded, as in BERNARD ""BERNIE"" SANDERS; restore the single
  # quote.
  for (v in c("contest", "choice")) {
    x <- enc2utf8(iconv(out[[v]], from = lay$encoding, to = "UTF-8"))
    set(out, j = v, value = gsub('""', '"', x, fixed = TRUE))
  }
  out[, writein := lay$writein(raw)]
  out[, votes := lay$votes(raw)]
  for (v in c("registered", "ballots"))
    set(out, j = v, value = if (is.function(lay[[v]])) lay[[v]](raw) else as.integer(out[[v]]))
  out[, election := election]
  setcolorder(out, "election")
  out[]
}

read_elections <- function(elections) rbindlist(lapply(elections, read_election))
