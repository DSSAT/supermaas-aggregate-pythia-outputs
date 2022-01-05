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

# predefined_percentiles <- c(0, 5, 25,50, 75, 95, 100)
# default_factors_threshold <- c("LATITUDE", "LONGITUDE", "RUN_NAME")
default_factors_population <- c("LATITUDE", "LONGITUDE", "HYEAR")
default_var <- "HWAH"

p <- argparser::arg_parser("Estimate the population affected by crop failures based on Pythia outputs for World Modelers(fixed)")
# p <- argparser::add_argument(p, "input_base", "Pythia output result directory or file for Baseline conditions")
p <- argparser::add_argument(p, "input_fore", "Pythia output result directory or file for Forecast simulations")
p <- argparser::add_argument(p, "output", "statistics report CSV file")
# p <- argparser::add_argument(p, "--yield_threshold_method", short="-m", default="std", help=paste0("The method used for calculating yield threshold, options could be [std, pctl]"))
# p <- argparser::add_argument(p, "--yield_threshold_constant", short="-c", default="1.5", help=paste0("The constant used for calculating yield threshold"))
p <- argparser::add_argument(p, "--yield_threshold", short="-y", default="100", help=paste0("Threshold for low yield, kg/ha"))
p <- argparser::add_argument(p, "--population_threshold", short="-p", default="95", help=paste0("Threshold for population, percent"))
# p <- argparser::add_argument(p, "--boxplot", short="-b", help = "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--variable", short="-v", nargs=1, help=paste0("Variable name for determine population affected by crop failure:, by default will be ", paste(default_vars, collapse = ",")))
# p <- argparser::add_argument(p, "--factors_agg", short="-c", nargs=Inf, help=paste0("Factor names for aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will use factors for statistics plus HYEAR"))
p <- argparser::add_argument(p, "--gadm_path", short="-g", default = "gadm_shapes", nargs=Inf, help="Path to the GADM shape file forlder")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case17\\base", "test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv", "-m", "std", "-c", "1.5"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\base", "test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P_25.csv", "-m", "pctl", "-c", "25"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv" , "-y", "1000"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv"))

# suppressWarnings(in_dir_base <- normalizePath(argv$input_base))
suppressWarnings(in_dir_fore <- normalizePath(argv$input_fore))
suppressWarnings(out_file <- normalizePath(argv$output))

# yield_threshold_method <- argv$yield_threshold_method
# yield_threshold_constant <- as.numeric(argv$yield_threshold_constant)
yield_threshold <- as.numeric(argv$yield_threshold)
population_threshold <- as.numeric(argv$population_threshold) / 100
variable <- argv$variable
suppressWarnings(if (is.na(variable) || is.null(variable)) {
  variable <- default_var
})
# factors_threshold <- argv$factors_threshold
# suppressWarnings(if (is.na(factors_threshold)) {
#   factors_threshold <- default_factors_threshold
# })
# factors_population <- argv$factors_population
# suppressWarnings(if (is.na(factors_population)) {
#   factors_population <- default_factors_population
# })
factors_population <- default_factors_population

# if (!dir.exists(in_dir_base) && !file.exists(in_dir_base)) {
#   stop(sprintf("%s does not exist.", in_dir_base))
# }

if (!dir.exists(in_dir_fore) && !file.exists(in_dir_fore)) {
  stop(sprintf("%s does not exist.", in_dir_fore))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# # Process baseline data and calculate threshold
# flist <- list()
# dts <- list()
# print("Loading files for calculation threshold from baseline conditions")
# if (!dir.exists(in_dir_base)) {
#   flist <- in_dir_base
# } else {
#   flist <- list.files(path = in_dir_base, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
# }
# for(f in flist) {
#   dts <- c(dts, list(data.table::fread(f)))
# }
# df <- data.table::rbindlist(dts)
# 
# valid_entries <- df
# colNames <- colnames(valid_entries)
# if ("EDAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(EDAT), "%Y%j"))]
# }
# if ("MDAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(MDAT), "%Y%j"))]
# }
# if ("ADAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(ADAT), "%Y%j"))]
# }
# if ("HDAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
# }
# if ("HWAH" %in% colNames) {
#   valid_entries <- valid_entries[HWAH > 0]
# }
# 
# if (!"HYEAR" %in% colNames) {
#   valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
# }
# if (!"HMONTH" %in% colNames) {
#   valid_entries[,HMONTH:=format(as.Date(paste0(HDAT), "%Y%j"), "%m")]
# }
# 
# # if (!"PYEAR" %in% colNames) {
# #   valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
# # }
# 
# print("Starting threshold calculation")
# variable <- variables[1]
# if (yield_threshold_method == "std") {
#   baseline <- valid_entries[,.(THRESHOLD = mean(get(variable)) - yield_threshold_constant * sd(get(variable))),by = factors_threshold]
#   # baseline[,threshold := (mean - (yield_threshold_constant)*std)]
# } else if (yield_threshold_method == "pctl") {
#   # yield_threshold_constant <- 25
#   baseline <- valid_entries[,.(THRESHOLD = quantile(get(variable), probs = yield_threshold_constant/100)), by = factors_threshold]
# } else {
#   stop(sprintf("%s is not supported.", yield_threshold_method))
# }

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

if (!"ADMLV0" %in% colnames(df2) || !"ADMLV1" %in% colnames(df2)) {
  print("Calcuating country and region information for each pixel")
  runCommand <- paste("Rscript","fix-pythia-outputs.R", in_dir_fore, "-g", "6")
  suppressWarnings(ret <- system(runCommand, show.output.on.console=FALSE))
  if (ret != 0) {
    print("Error happened during aggregation, process quit. Please run aggregation script separatedly to pre-check your run.")
    q()
  }
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
}

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
# fix : calcualte average of target varaible first, then compare to threshold
# forecast <- merge(valid_entries, baseline, by=factors_threshold, all.x=T)
columns <- c(factors_population,"ADMLV0", "ADMLV1", "HARVEST_AREA", "RUN_NAME", "HWAH", "POPULATION")
forecast <- valid_entries[,..columns][POPULATION <= quantile(POPULATION, probs = population_threshold)]
forecast[, PIXEL_TOTAL_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]

forecastLowHarvest <- forecast[get(variable) < yield_threshold]
forecastLowHarvest[, PIXEL_LOW_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]
forecastLowHarvest[, HUNGRY_PEOPLE := POPULATION * PIXEL_LOW_HARVEST_AREA/ PIXEL_TOTAL_HARVEST_AREA]

final_ADMLV0 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0)]
final_ADMLV0 <- merge(final_ADMLV0, forecast[,.(ADMLV0 = unique(ADMLV0))], by = "ADMLV0", all = T)
final_ADMLV0[is.na(HUNGRY_PEOPLE), HUNGRY_PEOPLE := 0]
setnames(final_ADMLV0, "ADMLV0", var_dic[name=="ADMLV0", factor])
setnames(final_ADMLV0, "HUNGRY_PEOPLE", "hungry_people")
# final_ADMLV0 <- forecastLowHarvest[,.(ADMLV1 = "", HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0)]
final_ADMLV1 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0, ADMLV1)]
final_ADMLV1 <- merge(final_ADMLV1, unique(forecast[,.(ADMLV0, ADMLV1)]), by = c("ADMLV0", "ADMLV1"), all = T)
final_ADMLV1[is.na(HUNGRY_PEOPLE), HUNGRY_PEOPLE := 0]
setnames(final_ADMLV1, "ADMLV0", var_dic[name=="ADMLV0", factor])
setnames(final_ADMLV1, "ADMLV1", var_dic[name=="ADMLV1", factor])
setnames(final_ADMLV1, "HUNGRY_PEOPLE", "hungry_people")

data.table::fwrite(rbind(final_ADMLV0, list("", "")), file = out_file)
data.table::fwrite(final_ADMLV1, file = out_file, append = T, col.names = T)

print("Complete.")
