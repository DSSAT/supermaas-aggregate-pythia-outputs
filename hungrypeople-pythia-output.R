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

default_factors_population <- c("LATITUDE", "LONGITUDE", "HYEAR")
default_var <- "HWAH"

p <- argparser::arg_parser("Estimate the population affected by crop failures based on Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input_fore", "Pythia output result directory or file for Forecast simulations")
p <- argparser::add_argument(p, "output", "statistics report CSV file")
p <- argparser::add_argument(p, "--yield_threshold", short="-y", default="100", help=paste0("Threshold for low yield, kg/ha"))
p <- argparser::add_argument(p, "--population_threshold", short="-p", default="95", help=paste0("Threshold for population, percent"))
# p <- argparser::add_argument(p, "--boxplot", short="-b", help = "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--variable", short="-v", nargs=1, help=paste0("Variable name for determine population affected by crop failure:, by default will be ", paste(default_var, collapse = ",")))
p <- argparser::add_argument(p, "--ignore_zero", short="-z", flag = TRUE, help=paste0("Ignore the rows with 0 hungry people in the output"))
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case17\\base", "test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv", "-m", "std", "-c", "1.5"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\base", "test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P_25.csv", "-m", "pctl", "-c", "25"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv" , "-y", "1000"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\result\\report_P1.csv", "-z"))

suppressWarnings(in_dir_fore <- normalizePath(argv$input_fore))
suppressWarnings(out_file <- normalizePath(argv$output))

yield_threshold <- as.numeric(argv$yield_threshold)
population_threshold <- as.numeric(argv$population_threshold) / 100
variable <- argv$variable
ignore_zero_flg <- argv$ignore_zero
suppressWarnings(if (is.na(variable) || is.null(variable)) {
  variable <- default_var
})
factors_population <- default_factors_population

if (!dir.exists(in_dir_fore) && !file.exists(in_dir_fore)) {
  stop(sprintf("%s does not exist.", in_dir_fore))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
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

header <- var_dic[name==variable, average]
aggFactors <- c(factors_population, "ADMLV0", "ADMLV1", "RUN_NAME")
columns <- c(aggFactors, "HARVEST_AREA", "POPULATION")

# calcualte average of target varaible first, then compare to threshold
forecast <- valid_entries[,mean(get(variable)), by = aggFactors]
setnames(forecast, "V1", header)
forecast <- merge(forecast, valid_entries[,..columns], by=aggFactors, all.x =T, sort = F)

# Exclude pixels with population > the threshold for further analysis. 
forecast <- forecast[POPULATION <= quantile(POPULATION, probs = population_threshold)]
forecast[, PIXEL_TOTAL_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]

# Use the population map to compute the number of people affected by low harvests. For each pixel and each forecast year
forecastLowHarvest <- forecast[get(header) < yield_threshold]
forecastLowHarvest[, PIXEL_LOW_HARVEST_AREA := sum(HARVEST_AREA), by = factors_population]
forecastLowHarvest[, HUNGRY_PEOPLE := POPULATION * PIXEL_LOW_HARVEST_AREA/ PIXEL_TOTAL_HARVEST_AREA]

# Aggregate the HungryPeoplei,n to Admin levels 0 for each year
final_ADMLV0 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0, HYEAR)][order(ADMLV0, HYEAR)]
if (!ignore_zero_flg) {
  final_ADMLV0 <- merge(final_ADMLV0, unique(forecast[,.(ADMLV0, HYEAR)]), by = c("ADMLV0", "HYEAR"), all = T, sort = F)
  final_ADMLV0[is.na(HUNGRY_PEOPLE), HUNGRY_PEOPLE := 0]
}
setnames(final_ADMLV0, "HYEAR", var_dic[name=="HYEAR", factor])
setnames(final_ADMLV0, "ADMLV0", var_dic[name=="ADMLV0", factor])
setnames(final_ADMLV0, "HUNGRY_PEOPLE", "hungry_people")

# Aggregate the HungryPeoplei,n to Admin levels 0 for each year
final_ADMLV1 <- forecastLowHarvest[,.(HUNGRY_PEOPLE = sum(HUNGRY_PEOPLE)), by = .(ADMLV0, ADMLV1, HYEAR)][order(ADMLV0, ADMLV1, HYEAR)]
if (!ignore_zero_flg) {
  final_ADMLV1 <- merge(final_ADMLV1, unique(forecast[,.(ADMLV0, ADMLV1, HYEAR)]), by = c("ADMLV0", "ADMLV1", "HYEAR"), all = T, sort = F)
  final_ADMLV1[is.na(HUNGRY_PEOPLE), HUNGRY_PEOPLE := 0]
}
setnames(final_ADMLV1, "HYEAR", var_dic[name=="HYEAR", factor])
setnames(final_ADMLV1, "ADMLV0", var_dic[name=="ADMLV0", factor])
setnames(final_ADMLV1, "ADMLV1", var_dic[name=="ADMLV1", factor])
setnames(final_ADMLV1, "HUNGRY_PEOPLE", "hungry_people")

data.table::fwrite(rbind(final_ADMLV0, list("", "", "")), file = out_file)
data.table::fwrite(final_ADMLV1, file = out_file, append = T, col.names = T)

print("Complete.")
