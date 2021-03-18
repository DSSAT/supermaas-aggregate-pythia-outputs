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

predefined_vars <- c("PRODUCTION", "TIMESTAMP","")
default_factors <- c("LATITUDE", "LONGITUDE", "HYEAR")

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help=paste0("Variable names for predefined aggregation: [", paste(predefined_vars, collapse=","), "]"))
p <- argparser::add_argument(p, "--total", short="-t", nargs=Inf, help=paste0("Variable names for summary aggregation: [", paste(var_dic[total!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--average", short="-a", nargs=Inf, help=paste0("Variable names for average aggregation: [", paste(var_dic[average!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--total_ton", short="-o", nargs=Inf, help=paste0("Variable names for summary aggregation with unit of ton and round to integer: [", paste(var_dic[total_ton!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for summary aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "]"))
p <- argparser::add_argument(p, "--period_annual", short="-a", flag=TRUE, help="Do the aggregation by year")
# p <- argparser::add_argument(p, "--period_month", short="-m", flag=TRUE, help="Do the aggregation by month")
# p <- argparser::add_argument(p, "--period_season", short="-s", flag=TRUE, help="Do the aggregation by growing season")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2.csv", "-v", "PRODUCTION", "-t", "CWAM", "HWAH", "-a", "MDAT", "CWAM", "HWAH", "-o", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2_dev.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case5\\ETH_Maize_irrig", "test\\data\\case5\\report5.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

#in_dir <- here("work", "fen_tot0")
#out_file <- here("work", "wmout.csv")

variables <- argv$variables
totVariables <- argv$total
avgVariables <- argv$average
totTonVariables <- argv$total_ton
suppressWarnings(if (is.na(variables) && is.na(totVariables) && is.na(avgVariables) && is.na(totTonVariables)) {
  variables <- predefined_vars
})
factors <- argv$factor
suppressWarnings(if (is.na(factors)) {
  factors <- default_factors
})
argv$period_annual <- TRUE

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
valid_entries <- df[
  !is.na(as.Date(paste0(EDAT), "%Y%j")) &
  !is.na(as.Date(paste0(MDAT), "%Y%j")) &
  !is.na(as.Date(paste0(ADAT), "%Y%j")) &
  !is.na(as.Date(paste0(HDAT), "%Y%j")) &
  HWAH >= 0
]
if (!"HYEAR" %in% colnames(valid_entries)) {
  valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
}
if (!"PYEAR" %in% colnames(valid_entries)) {
  valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
}
print("Starting aggregation.")
if (argv$period_annual) {
  print("Processing annual calculation.")
  calc_production <- valid_entries
  calc_production <- calc_production[,`:=`(HARVEST_AREA_PCT = HARVEST_AREA/sum(HARVEST_AREA)), by = factors]
  aggregated <- calc_production[,.(HARVEST_AREA_TOT=sum(HARVEST_AREA)),by = factors]
  final <- aggregated[, ..factors]
  setnames(final, var_dic[name %in% factors, factor])
  
  # execute predefined variable aggregation
  suppressWarnings(if (!is.na(variables)) {
    for (variable in variables) {
      if (variable %in% predefined_vars) {
        print(paste("Processing",  variable))
        if (variable == "TIMESTAMP") {
          calc_production[,`:=`(HDAT_ISO = as.Date(paste0(HDAT), "%Y%j"))]
          aggregated[,(variable):= calc_production[,mean.Date(HDAT_ISO),by = factors][,V1]]
          final[, timestamp := aggregated[,get(variable)]]
        } else if (variable == "PRODUCTION") {
          calc_production[,(variable) := HARVEST_AREA * HWAH]
          aggregated[, (variable):= calc_production[,sum(get(variable)), by = factors][,V1]]
          final[, production := aggregated[,round(get(variable)/1000)]]
        }
      } else {
        print(paste("Processing",  variable, "is unsupported and skipped"))
      }
    }
  })
  
  # execute summary aggregation
  suppressWarnings(if (!is.na(totVariables)) {
    for (variable in totVariables) {
      header <- var_dic[name == variable, total]
      if (header != "") {
        print(paste("Processing summary for",  variable))
        
        if (var_dic[name == variable, unit] == "kg/ha") {
          
          calc_production[,(header):= as.numeric(get(variable)) * HARVEST_AREA]
          aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
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
      header <- var_dic[name == variable, average]
      if (header != "") {
        print(paste("Processing average for",  variable))
        
        if (var_dic[name == variable, unit] == "kg/ha") {
          
          ## header_tot <- var_dic[name == variable, total]
          # header_tot <- paste0(variable, "_TOT")
          # if (!header_tot %in% colnames(calc_production)) {
          #   calc_production[,(header_tot):= get(variable) * HARVEST_AREA]
          # }
          aggregated[, (header):= calc_production[,sum(as.numeric(get(variable)) * HARVEST_AREA_PCT), by = factors][,V1]]
          aggregated[, (header):=get(header)/HARVEST_AREA_TOT]
          final[, (header):= aggregated[,get(header)]]
          
        } else if (var_dic[name == variable, unit] == "date") {
          
          calc_production[,(header):= as.Date(paste0(get(variable)), "%Y%j")]
          aggregated[,(header):= calc_production[,as.Date(sum(as.integer(get(header)) * HARVEST_AREA_PCT), origin="1970-01-01"),by = factors][,V1]]
          final[, (header) := aggregated[,get(header)]]
          
        } else {
          print(paste("Processing average for",  variable, "is unsupported and skipped"))
        }
        
      } else {
        print(paste("Processing average for",  variable, "is unsupported and skipped"))
      }
    }
  })
  
  # execute total_ton aggregation
  suppressWarnings(if (!is.na(totTonVariables)) {
    for (variable in totTonVariables) {
      header <- var_dic[name == variable, total_ton]
      if (header != "") {
        print(paste("Processing summary (unit=ton) for",  variable))
        
        if (var_dic[name == variable, unit] == "kg/ha") {
          
          calc_production[,(header):= as.numeric(get(variable)) * HARVEST_AREA]
          aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
          final[, (header):= aggregated[,round(get(header)/1000)]]
          
        } else {
          print(paste("Processing summary (unit=ton) for",  variable, "is unsupported and skipped"))
        }
        
      } else {
        print(paste("Processing summary (unit=ton) for",  variable, "is unsupported and skipped"))
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
