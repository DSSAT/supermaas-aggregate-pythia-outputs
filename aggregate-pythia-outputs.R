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

predefined_vars <- c("PRODUCTION", "TIMESTAMP")

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help=paste0("Variable names for predefined aggregation: [", paste(predefined_vars, collapse=","), "]"))
p <- argparser::add_argument(p, "--total", short="-t", nargs=Inf, help=paste0("Variable names for summary aggregation: [", paste(var_dic[total==TRUE,name], collapse=","), "]"))
p <- argparser::add_argument(p, "--average", short="-a", nargs=Inf, help=paste0("Variable names for average aggregation: [", paste(var_dic[average==TRUE,name], collapse=","), "]"))
# p <- argparser::add_argument(p, "--period_annual", short="-a", flag=TRUE, help="Do the aggregation by year")
# p <- argparser::add_argument(p, "--period_month", short="-m", flag=TRUE, help="Do the aggregation by month")
# p <- argparser::add_argument(p, "--period_season", short="-s", flag=TRUE, help="Do the aggregation by growing season")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "test\\output\\report2.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2_dev.csv"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

#in_dir <- here("work", "fen_tot0")
#out_file <- here("work", "wmout.csv")

variables <- argv$variables
totVariables <- argv$total
avgVariables <- argv$average
suppressWarnings(if (is.na(variables) && is.na(totVariables) && is.na(avgVariables)) {
  variables <- predefined_vars
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
  # aggregated<- calc_production[,.(HARVEST_DATE=mean(as.Date(paste0((HDAT)), "%Y%j"))),by = .(LATITUDE,LONGITUDE,YEAR)]
  aggregated<- calc_production[,.(HARVEST_AREA_TOT=sum(HARVEST_AREA)),by = .(LATITUDE,LONGITUDE,YEAR)]
  final <- aggregated[,.(lat=LATITUDE,lng=LONGITUDE, year=YEAR)]
  
  # execute predefined variable aggregation
  suppressWarnings(if (!is.na(variables)) {
    for (variable in variables) {
      print(paste("Processing",  variable))
      if (variable == "TIMESTAMP") {
        calc_production[,`:=`(HDAT_ISO = as.Date(paste0(HDAT), "%Y%j"))]
        aggregated[,(variable):= calc_production[,mean.Date(HDAT_ISO),by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
        final[, timestamp := aggregated[,get(variable)]]
      } else if (variable == "PRODUCTION") {
        calc_production[,(variable) := HARVEST_AREA * HWAH, by = .(LATITUDE, LONGITUDE)]
        aggregated[, (variable):= calc_production[,sum(get(variable)), by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
        final[, production := aggregated[,round(get(variable)/1000)]]
      }
      
    }
  })
  
  # execute summary aggregation
  suppressWarnings(if (!is.na(totVariables)) {
    for (variable in totVariables) {
      if (var_dic[name == variable, total]) {
        print(paste("Processing summary for",  variable))
        header <- paste0(variable, "_TOT")
        
        if (var_dic[name == variable, unit] == "kg/ha") {
          
          calc_production[,(header):= get(variable) * HARVEST_AREA, by = .(LATITUDE, LONGITUDE)]
          aggregated[, (header):= calc_production[,sum(get(header)), by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
          final[, (header):= aggregated[,get(header)]]
          
        } else {
          print(paste("Processing summary for",  variable, "is unsupported and skipped"))
        }
        
      } else {
        print(paste("Processing summary for",  variable, "is unsupported and skipped"))
      }
    }
  })
  
  # execute average aggregation
  suppressWarnings(if (!is.na(avgVariables)) {
    for (variable in avgVariables) {
      if (var_dic[name == variable, total]) {
        print(paste("Processing summary for",  variable))
        header <- paste0(variable, "_AVG")
        
        if (var_dic[name == variable, unit] == "kg/ha") {
          
          header_tot <- paste0(variable, "_TOT")
          calc_production[,(header_tot):= get(variable) * HARVEST_AREA, by = .(LATITUDE, LONGITUDE)]
          aggregated[, (header):= calc_production[,sum(get(header_tot)), by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
          aggregated[,(header):=get(header)/HARVEST_AREA_TOT]
          final[, (header):= aggregated[,get(header)]]
          
        } else if (var_dic[name == variable, unit] == "date") {
          
          calc_production[,(header):= as.Date(paste0(get(variable)), "%Y%j")]
          aggregated[,(header):= calc_production[,mean.Date(get(header)),by = .(LATITUDE,LONGITUDE,YEAR)][,V1]]
          final[, (header) := aggregated[,get(header)]]
          
        } else {
          print(paste("Processing summary for",  variable, "is unsupported and skipped"))
        }
        
      } else {
        print(paste("Processing summary for",  variable, "is unsupported and skipped"))
      }
    }
  })
  
  if ("timestamp" %in% colnames(final)) {
    final[, year:=NULL]
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
