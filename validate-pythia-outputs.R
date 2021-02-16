#library(here)
library(argparser)
library(data.table)

setwd(".")

p <- argparser::arg_parser("VAlidate Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "--output", short="-o", "Path to the file of validation report")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help="Variable names for aggregation")
argv <- argparser::parse_args(p)

suppressWarnings(in_dir <- normalizePath(argv$input))

variables <- argv$variables
if (is.na(variables)) {
  variables <- c("HDAT","HWAH", "EDAT", "MDAT", "ADAT", "HDAT");
}

if (!dir.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

flist <- list()
dts <- list()
print("Loading files for validation")
flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df <- data.table::rbindlist(dts)
print("Starting validation.")
report <- data.table(ID=rnorm(0),
                     "pct_invalid" = rnorm(0),
                     "cnt_invalid" = rnorm(0),
                     "pct_zero" = rnorm(0),
                     "pct_zero" = rnorm(0))

for (variable in variables) {
  total <- df[, .N]
  invalid <- df[get(variable) < 0, .N]
  zero <- df[get(variable) == 0, .N]
  report <- rbind(report, list(variable,
                               paste0(round(invalid/total*100, 2), "%"),
                               paste0(invalid, "/", total),
                               paste0(round(zero/total*100, 2), "%"),
                               paste0(zero, "/", total)))
}
print(report)
if (!is.na(argv$output)) {
  suppressWarnings(out_file <- normalizePath(argv$output))
  out_dir <- dirname(out_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  data.table::fwrite(report, file = out_file)
}
print("Complete.")
