#library(here)
library(argparser)
library(data.table)

setwd(".")

data_cde_file <- "DATA_CDE.csv"
if (file.exists(data_cde_file)) {
  var_dic <- data.table::fread(data_cde_file)
} else {
  # const_ha_vars <- c("DWAP", "CWAM", "HWAM", "HWAH", "BWAH", "PWAM", "")
  # const_temp_vars <- c("TMAXA", "TMINA")
  # const_date_vars <- c("SDAT", "PDAT", "EDAT", "ADAT", "MDAT", "HDAT")
}

p <- argparser::arg_parser("VAlidate Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--output", short = "-o", "Path to the file of validation report")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for validation: [", paste(var_dic[unit != "text", name], collapse = ","), "]"))
p <- argparser::add_argument(p, "--min", short = "-i", flag = TRUE, help = "Report minimum value for range check")
p <- argparser::add_argument(p, "--max", short = "-a", flag = TRUE, help = "Report maximum value for range check")
p <- argparser::add_argument(p, "--mean", short = "-e", flag = TRUE, help = "Report mean value for range check")
p <- argparser::add_argument(p, "--med", short = "-d", flag = TRUE, help = "Report meadian value for range check")
p <- argparser::add_argument(p, "--std", short = "-s", flag = TRUE, help = "Report standard deviation for range check")
p <- argparser::add_argument(p, "--no-zero", short="-n", flag = TRUE, help = "Exclude 0 value from range check")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv", "-v", "PDAT", "MDAT", "HDAT","HWAM", "TMAXA", "TMINA", "PRCP", "--min","--max","--med", "--std", "-n"))

suppressWarnings(in_dir <- normalizePath(argv$input))

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- c("HWAH", "EDAT", "MDAT", "ADAT", "HDAT");
})

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

flist <- list()
dts <- list()
print("Loading files for validation")
if (!dir.exists(in_dir)) {
  flist[1] = in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
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
if (argv$std) {
  report[,`:=`(std=rnorm(0))]
}
for (variable in variables) {
  total <- df[, .N]
  if (variable %in% var_dic[unit == "date", name]) {
    header <- paste0(variable, "_ISO")
    df[, (header) := as.Date(paste0(get(variable)), "%Y%j")]
    invalid <- df[is.na(get(header)), .N]
  } else {
    header <- variable
    invalid <- df[get(header) <= -99, .N]
  }
  
  if (variable %in% var_dic[unit %in% c("date", "degree_c"), name]) {
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),"","")
  } else {
    zero <- df[get(header) == 0, .N]
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),
                paste0(round(zero/total*100, 2), "%"),
                paste0(zero, "/", total))
  }
  if (variable %in% var_dic[unit == "date", name]) {
    valid_entries <- df[!is.na(get(header))]
  } else if (argv$no_zero && !variable %in% var_dic[unit == "degree_c", name]) {
    valid_entries <- df[get(header)> -99 & get(header) != 0]
  } else {
    valid_entries <- df[get(header)> -99]
  }
  
  if (argv$min) {
    row <- c(row, paste0(valid_entries[,min(get(header))]))
  }
  if (argv$max) {
    row <- c(row, paste0(valid_entries[,max(get(header))]))
  }
  if (argv$mean) {
    row <- c(row, paste0(valid_entries[,mean(get(header))]))
  }
  if (argv$med) {
    row <- c(row, paste0(valid_entries[,median(get(header))]))
  }
  if (argv$std) {
    row <- c(row, paste0(valid_entries[,sd(get(variable))]))
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
