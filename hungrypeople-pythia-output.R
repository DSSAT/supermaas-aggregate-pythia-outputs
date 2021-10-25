#library(here)
library(argparser)
library(data.table)
library(raster)

setwd(".")

data_cde_file <- "DATA_CDE.csv"
if (file.exists(data_cde_file)) {
  var_dic <- data.table::fread(data_cde_file)
} else {
  # const_ha_vars <- c("DWAP", "CWAM", "HWAM", "HWAH", "BWAH", "PWAM", "")
  # const_temp_vars <- c("TMAXA", "TMINA")
  # const_date_vars <- c("SDAT", "PDAT", "EDAT", "ADAT", "MDAT", "HDAT")
}

predefined_vars <- c("PRODUCTION", "TIMESTAMP")
# predefined_percentiles <- c(0, 5, 25,50, 75, 95, 100)
default_factors_threshold <- c("LATITUDE", "LONGITUDE", "RUN_NAME")
default_factors_population <- c("LATITUDE", "LONGITUDE", "HYEAR")
default_vars <- c("HWAH")

p <- argparser::arg_parser("Estimate the population affected by crop failures based on Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input_base", "Pythia output result directory or file for Baseline conditions")
p <- argparser::add_argument(p, "input_fore", "Pythia output result directory or file for Forecast simulations")
p <- argparser::add_argument(p, "output", "statistics report CSV file")
p <- argparser::add_argument(p, "--yield_threshold_method", short="-m", default="std", help=paste0("The method used for calculating yield threshold, options could be [std, pctl]"))
p <- argparser::add_argument(p, "--yield_threshold_constant", short="-c", default="1.5", help=paste0("The constant used for calculating yield threshold"))
# p <- argparser::add_argument(p, "--is_aggregated", short="-i", flag = TRUE, help=paste0("Indicate the input folder or file is already the result of aggregation script"))
# # p <- argparser::add_argument(p, "--boxplot", short="-b", help = "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=1, help=paste0("Variable names for determine population affected by crop failure:, by default will be ", paste(default_vars_threshold, collapse = ",")))
p <- argparser::add_argument(p, "--factors_threshold", short="-f", nargs=Inf, help=paste0("Factor names for threshhold calculation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will be ", paste(default_factors_population, collapse = ",")))
p <- argparser::add_argument(p, "--factors_population", short="-p", nargs=Inf, help=paste0("Factor names for threshhold calculation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will be ", paste(default_factors, collapse = ",")))
# p <- argparser::add_argument(p, "--factors_agg", short="-c", nargs=Inf, help=paste0("Factor names for aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will use factors for statistics plus HYEAR"))
p <- argparser::add_argument(p, "--gadm_path", short="-g", default = "gadm_shapes", nargs=Inf, help="Path to the GADM shape file forlder")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case16", "test\\data\\case2", "test\\output\\report_P1.csv"))

suppressWarnings(in_dir_base <- normalizePath(argv$input_base))
suppressWarnings(in_dir_fore <- normalizePath(argv$input_fore))
suppressWarnings(out_file <- normalizePath(argv$output))

yield_threshold_method <- argv$yield_threshold_method
yield_threshold_constant <- as.numeric(argv$yield_threshold_constant)
variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- default_vars
})
factors_threshold <- argv$factors_threshold
suppressWarnings(if (is.na(factors_threshold)) {
  factors_threshold <- default_factors_threshold
})
factors_population <- argv$factors_population
suppressWarnings(if (is.na(factors_population)) {
  factors_population <- default_factors_population
})

if (!dir.exists(in_dir_base) && !file.exists(in_dir_base)) {
  stop(sprintf("%s does not exist.", in_dir_base))
}

if (!dir.exists(in_dir_fore) && !file.exists(in_dir_fore)) {
  stop(sprintf("%s does not exist.", in_dir_fore))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Process baseline data and calculate threshold
flist <- list()
dts <- list()
print("Loading files for calculation threshold from baseline conditions")
if (!dir.exists(in_dir_base)) {
  flist <- in_dir_base
} else {
  flist <- list.files(path = in_dir_base, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df <- data.table::rbindlist(dts)

valid_entries <- df
colNames <- colnames(valid_entries)
if ("EDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(EDAT), "%Y%j"))]
}
if ("MDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(MDAT), "%Y%j"))]
}
if ("ADAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(ADAT), "%Y%j"))]
}
if ("HDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
}
if ("HWAH" %in% colNames) {
  valid_entries <- valid_entries[HWAH > 0]
}

if (!"HYEAR" %in% colNames) {
  valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
}
# if (!"PYEAR" %in% colNames) {
#   valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
# }

print("Starting threshold calculation")
variable <- variables[1]
if (yield_threshold_method == "std") {
  baseline <- valid_entries[,.(THRESHOLD = mean(get(variable)) - yield_threshold_constant * sd(get(variable))),by = factors_threshold]
  # baseline[,threshold := (mean - (yield_threshold_constant)*std)]
} else if (yield_threshold_method == "pctl") {
  # yield_threshold_constant <- 25
  baseline <- valid_entries[,.(THRESHOLD = quantile(get(variable), probs = yield_threshold_constant/100)), by = factors_threshold]
} else {
  stop(sprintf("%s is not supported.", yield_threshold_method))
}

# Process forecast data and calculate population affected by crop failure
flist <- list()
dts <- list()
print("Loading files for forecast simulations")
if (!dir.exists(in_dir_fore)) {
  flist <- in_dir_fore
} else {
  flist <- list.files(path = in_dir_fore, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df2 <- data.table::rbindlist(dts)

valid_entries <- df2
colNames <- colnames(valid_entries)
if ("EDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(EDAT), "%Y%j"))]
}
if ("MDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(MDAT), "%Y%j"))]
}
if ("ADAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(ADAT), "%Y%j"))]
}
if ("HDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
}
if ("HWAH" %in% colNames) {
  valid_entries <- valid_entries[HWAH >= 0]
}

if (!"HYEAR" %in% colNames) {
  valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
}
# if (!"PYEAR" %in% colNames) {
#   valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
# }

print("Starting crop failure impact calculation")
forecast <- merge(valid_entries, baseline, by=factors_threshold, all=T)
forecast[, PIXEL_TOTAL_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]
forecastLowHarvest <- forecast[get(variable) < THRESHOLD]
forecastLowHarvest[, PIXEL_LOW_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]
forecastLowHarvest[, HUNGRY_PEOPLE := POPULATION * PIXEL_LOW_HARVEST_AREA/ PIXEL_TOTAL_HARVEST_AREA]
final_ADMLV0 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0)]
final_ADMLV1 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0, ADMLV1)]

data.table::fwrite(final_ADMLV0, file = out_file)
data.table::fwrite(final_ADMLV1, file = out_file, append = T)

print("Complete.")
