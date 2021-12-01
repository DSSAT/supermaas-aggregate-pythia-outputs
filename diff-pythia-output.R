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

# predefined_vars <- c("PRODUCTION")
predefined_vars <- c("HWAH")

p <- argparser::arg_parser("Generate relative difference comparison report based on two aggregation results from Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input_base", "Aggregation result file for baseline data")
p <- argparser::add_argument(p, "input_scenario", "Aggregation result file for scenario data")
p <- argparser::add_argument(p, "output", "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for comparison, if not given, then comparing all non-factor columns"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor!="", factor]), collapse=","), "]"))

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case17\\agg_result\\agg_crop_per_person_base.csv", "test\\data\\case17\\agg_result\\agg_crop_per_person_scenario.csv", "test\\data\\case17\\diff_result\\diff.csv"))

suppressWarnings(in_dir_base <- normalizePath(argv$input_base))
suppressWarnings(in_dir_scenario <- normalizePath(argv$input_scenario))
suppressWarnings(out_file <- normalizePath(argv$output))

yield_threshold_method <- argv$yield_threshold_method
yield_threshold_constant <- as.numeric(argv$yield_threshold_constant)
variables <- argv$variables
factors <- argv$factors

if (!dir.exists(in_dir_base) && !file.exists(in_dir_base)) {
  stop(sprintf("%s does not exist.", in_dir_base))
}

if (!dir.exists(in_dir_scenario) && !file.exists(in_dir_scenario)) {
  stop(sprintf("%s does not exist.", in_dir_scenario))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Process baseline data and calculate threshold
print("Loading files for relative difference comparison")
df_base <- data.table::fread(in_dir_base)
df_scenario <- data.table::fread(in_dir_scenario)

if (is.na(factors)) {
  headers <- colnames(df_base)
  factors <- headers[headers %in% var_dic[factor != "", factor]]
}

if (is.na(variables)) {
  headers <- colnames(df_base)
  variables <- headers[!headers %in% factors]
}

print("Comparing files for relative difference comparison")
diff <- merge(df_base, df_scenario, by = factors)
for (variable in variables) {
  diff[,(paste0("DIFF_", variable)) := (get(paste0(variable, ".y")) - get(paste0(variable, ".x")))/get(paste0(variable, ".x")) * 100]
  diff[,(paste0(variable, ".x")):= NULL]
  diff[,(paste0(variable, ".y")):= NULL]
}

data.table::fwrite(diff, file = out_file)

print("Complete.")
