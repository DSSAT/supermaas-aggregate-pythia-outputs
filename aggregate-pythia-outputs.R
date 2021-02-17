#library(here)
library(argparser)
library(data.table)

setwd(".")

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help="Variable names for aggregation")
# p <- argparser::add_argument(p, "--period_annual", short="-a", flag=TRUE, help="Do the aggregation by year")
# p <- argparser::add_argument(p, "--period_month", short="-m", flag=TRUE, help="Do the aggregation by month")
# p <- argparser::add_argument(p, "--period_season", short="-s", flag=TRUE, help="Do the aggregation by growing season")
argv <- argparser::parse_args(p)

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

#in_dir <- here("work", "fen_tot0")
#out_file <- here("work", "wmout.csv")

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- "PRODUCTION";
})

if (!dir.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

flist <- list()
dts <- list()
print("Loading files for aggregation.")
flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df <- data.table::rbindlist(dts)
valid_entries <- df[EDAT > 0 & MDAT > 0 & ADAT > 0 & HDAT > 0 & HWAH >= 0]
print("Starting aggregation.")
# if (argv$period_annual) {
if (TRUE) {
  print("Processing annual calculation.")
  calc_production <- valid_entries[,`:=`(YEAR = trunc(HDAT/1000)), by = .(LATITUDE, LONGITUDE)]
  aggregated<- calc_production[, .(HDATE=as.Date(paste0(mean(HDAT)), "%Y%j")), by = .(LATITUDE,LONGITUDE,YEAR)]
  final <- aggregated[,.(lat=LATITUDE,lng=LONGITUDE,timestamp=HDATE)]
  for (variable in variables) {
    print(paste("Processing",  variable))
    if (variable == "PRODUCTION") {
      variable<-"PRODUCTION"
      calc_production[,(variable) := HARVEST_AREA * HWAH, by = .(LATITUDE, LONGITUDE)]
      aggregated[, (variable):= calc_production[,sum(get(variable)), by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
      final[, production := aggregated[,round(get(variable)/1000)]]
    } else {
      header <- paste0(variable, "_SUM")
      aggregated[, (header):= calc_production[,sum(get(variable)), by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
      final[, (header):= aggregated[,get(header)]]
    }
    
  }
  
  data.table::fwrite(final, file = out_file)
}
# if (argv$period_month) {
#   #print("Processing monthly calculation.")
#   print("Monthly aggregation has not been supported yet")
# }
# if (argv$period_season) {
#   #print("Processing seasonal calculation.")
#   print("Seasonal aggregation has not been supported yet")
# }

print("Complete.")
