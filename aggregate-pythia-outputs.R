#library(here)
library(argparser)
library(data.table)

setwd(".")

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
argv <- argparser::parse_args(p)

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

#in_dir <- here("work", "fen_tot0")
#out_file <- here("work", "wmout.csv")

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
fdirs <- list.dirs(path = in_dir)
for(d in fdirs) {
  f <- list.files(path = d, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
  flist <- c(flist, f)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
print("Starting aggregation.")
df <- data.table::rbindlist(dts)
valid_entries <- df[EDAT > 0 & MDAT > 0 & ADAT > 0 & HDAT > 0 & HWAH >= 0]
calc_production <- valid_entries[, `:=`(PROD = HARVEST_AREA * HWAH, YEAR = trunc(HDAT/1000)), by = .(LATITUDE, LONGITUDE)]
aggregated<- calc_production[,.(PRODUCTION=sum(PROD), HDATE=as.Date(paste0(mean(HDAT)), "%Y%j")), by = .(LATITUDE,LONGITUDE,YEAR)]
final <- aggregated[,.(lat=LATITUDE,lng=LONGITUDE,timestamp=HDATE,production=round(PRODUCTION/1000))]
data.table::fwrite(final, file = out_file)
print("Complete.")