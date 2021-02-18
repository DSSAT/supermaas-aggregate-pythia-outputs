#library(here)
library(argparser)
library(data.table)

setwd(".")

p <- argparser::arg_parser("VAlidate Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "--output", short = "-o", "Path to the file of validation report")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = "Variable names for validation")
p <- argparser::add_argument(p, "--min", short = "-i", flag = TRUE, help = "Report minimum value for range check")
p <- argparser::add_argument(p, "--max", short = "-a", flag = TRUE, help = "Report maximum value for range check")
p <- argparser::add_argument(p, "--mean", short = "-e", flag = TRUE, help = "Report mean value for range check")
p <- argparser::add_argument(p, "--med", short = "-d", flag = TRUE, help = "Report meadian value for range check")
p <- argparser::add_argument(p, "--no-zero", short="-n", flag = TRUE, help = "Exclude 0 value from range check")
argv <- argparser::parse_args(p)

suppressWarnings(in_dir <- normalizePath(argv$input))

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- c("HWAH", "EDAT", "MDAT", "ADAT", "HDAT");
})

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
report <- data.table(Variable=rnorm(0),
                     "Invalid_pct" = rnorm(0),
                     "Invalid_cnt" = rnorm(0),
                     "Zero_pct" = rnorm(0),
                     "Zero_cnt" = rnorm(0))
if (argv$min) {
  report[,`:=`(min=rnorm(0))]
}
if (argv$max) {
  report[,`:=`(max=rnorm(0))]
}
if (argv$mean) {
  report[,`:=`(mean=rnorm(0))]
}
if (argv$med) {
  report[,`:=`(median=rnorm(0))]
}
for (variable in variables) {
  total <- df[, .N]
  if (endsWith(variable, "DAT")) {
    invalid <- df[get(variable) <= -99 | get(variable) == 9999999, .N]
  } else {
    invalid <- df[get(variable) <= -99, .N]
  }
  if (endsWith(variable, "DAT") || variable == "TMAXA" || variable == "TMINA") {
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),"","")
  } else {
    zero <- df[get(variable) == 0, .N]
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),
                paste0(round(zero/total*100, 2), "%"),
                paste0(zero, "/", total))
  }
  
  if (argv$no-zero && variable != "TMAXA" && variable != "TMINA") {
    valid_entries <- df[get(variable)> -99 & get(variable) != 0]
  } else {
    valid_entries <- df[get(variable)> -99]
  }
  if (endsWith(variable, "DAT")) {
    valid_entries <- valid_entries[get(variable) != 9999999]
  }
  if (argv$min) {
    row <- c(row, valid_entries[,min(get(variable))])
  }
  if (argv$max) {
    row <- c(row, valid_entries[,max(get(variable))])
  }
  if (argv$mean) {
    row <- c(row, valid_entries[,mean(get(variable))])
  }
  if (argv$med) {
    row <- c(row, valid_entries[,median(get(variable))])
  }
  report <- rbind(report, row)
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
