# Load the packages and source every module, in dependency order. Scripts in
# the project folder start with source("R/setup.R").

suppressMessages({
  library(data.table)
  library(glmmTMB)
})

for (f in c("config", "layouts", "elections", "load", "lookup", "candidates",
            "build", "fit", "diagnostics", "nonpartisan", "measures", "secondary", "report")) {
  source(file.path("R", paste0(f, ".R")))
}
