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

predefined_vars <- c("PRODUCTION", "TIMESTAMP", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE")
default_factors <- c("LATITUDE", "LONGITUDE", "HYEAR")
population_threshold <- 0.95

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help=paste0("Variable names for predefined aggregation: [", paste(predefined_vars, collapse=","), "]"))
p <- argparser::add_argument(p, "--total", short="-t", nargs=Inf, help=paste0("Variable names for summary aggregation: [", paste(var_dic[total!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--average", short="-a", nargs=Inf, help=paste0("Variable names for average aggregation: [", paste(var_dic[average!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--total_ton", short="-o", nargs=Inf, help=paste0("Variable names for summary aggregation with unit of ton and round to integer: [", paste(var_dic[total_ton!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "]"))
# p <- argparser::add_argument(p, "--gadm_path", short="-g", default = "gadm_shapes", nargs=Inf, help="Path to the GADM shape file forlder")
p <- argparser::add_argument(p, "--crop_failure_threshold", short="-c", default = 100, nargs=Inf, help="Threshold to determine if the crop is failure, (kg/ha)")
p <- argparser::add_argument(p, "--low_production_per_person_threshold", short="-l", default = 100, nargs=Inf, help="Threshold to determine if the production per person is low, (kg/person)")
p <- argparser::add_argument(p, "--ignore_multiyear_sum_to_average", short="-i", flag = TRUE, help=paste0("Flag to disable the handling for getting average value against values from multi-years"))
# p <- argparser::add_argument(p, "--period_annual", short="-a", flag=TRUE, help="Do the aggregation by year")
# p <- argparser::add_argument(p, "--period_month", short="-m", flag=TRUE, help="Do the aggregation by month")
# p <- argparser::add_argument(p, "--period_season", short="-s", flag=TRUE, help="Do the aggregation by growing season")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "test\\output\\report1.csv", "-v", "PRODUCTION", "TIMESTAMP", "-t", "CWAM", "HWAH", "-a", "HDAT", "MDAT", "CWAM", "HWAH", "-o", "CWAM", "HWAH", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2.csv", "-v", "PRODUCTION", "-t","HARVEST_AREA", "CWAM", "HWAH", "-a", "MDAT", "CWAM", "HWAH", "-o", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2_dev.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case5\\ETH_Maize_irrig", "test\\data\\case5\\report5.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case6", "test\\output\\report6.csv", "-a", "HWAH", "GSD", "ETFD", "FTHD", "HIAM", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case6\\pp_GGCMI_Maize_ir.csv", "test\\output\\report6.csv", "-a", "PRCP", "HWAH", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2.csv", "-a", "PRCP", "HWAH", "-f","LONGITUDE", "LATITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2_country.csv", "-a", "PRCP", "HWAH", "-f","ADMLV0"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2_region.csv", "-a", "PRCP", "HWAH", "-f","ADMLV1"))
# argv <- argparser::parse_args(p, c("test\\data\\case11\\pp_GGCMI_SWH_SWheat_rf.csv", "test\\data\\case11\\agg_pp_GGCMI_SWH_SWheat_rf_country.csv", "-a", "PRCP", "HWAH", "-f","ADMLV0"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg\\pp_ETH_Maize_irrig_belg_S_season_base__fen_tot0.csv", "test\\data\\case12\\Maize_Belg\\pp_ETH_Maize_irrig_belg_S_season_base__fen_tot0_region.csv", "-a", "PRCP", "HWAH", "-f","ADMLV1", "HYEAR"))
# argv <- argparser::parse_args(p, c("test\\data\\case13", "test\\data\\case13\\result2_mid\\report13_0.csv", "-a", "HWAH", "-f","ADMLV0", "FILE", "HYEAR"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\base", "test\\data\\case17\\agg_result\\agg_crop_per_person_base_adm1.csv", "-v", "CROP_PER_PERSON", "-f","ADMLV1", "HYEAR", "CR"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\agg_result\\agg_crop_per_person_drop_scenario_adm1.csv", "-v", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "-a", "HWAH", "-f","ADMLV1", "HYEAR", "CR"))
# argv <- argparser::parse_args(p, c("test\\data\\case17\\scenario", "test\\data\\case17\\agg_result\\agg_crop_per_person_drop_scenario_pixel.csv", "-v", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "-a", "HWAH", "-f","LONGITUDE", "LATITUDE", "HYEAR", "CR"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\baseline\\pythia_out\\pp_GHA_ALL.csv", "test\\data\\case18\\baseline\\debug\\agg_crop_per_person_drop_scenario_pixel.csv", "-v", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "-a", "HWAH", "-f","LONGITUDE", "LATITUDE", "HYEAR", "CR"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\scenario\\pythia_out", "test\\data\\case18\\test\\scenario\\analysis_out\\stage_14_admlv2.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","ADMLV0", "RUN_NAME", "HYEAR"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test3\\baseline\\pythia_out", "test\\data\\case18\\test2\\scenario\\analysis_out\\stage_2.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR", "HYEAR", "-l", "200"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

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
cfThreshold <- argv$crop_failure_threshold
hpThreshold <- argv$low_production_per_person_threshold
multiYearIgnFlg <- argv$ignore_multiyear_sum_to_average

# argv$period_annual <- TRUE

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

flist <- list()
dts <- list()
print("Loading files for aggregation.")
if (!dir.exists(in_dir)) {
  flist <- in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)[,FILE := tools::file_path_sans_ext(basename(f))]))
}
df <- data.table::rbindlist(dts)
valid_entries <- df
colNames <- colnames(valid_entries)
# if ("EDAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(EDAT), "%Y%j"))]
# }
# if ("MDAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(MDAT), "%Y%j"))]
# }
# if ("ADAT" %in% colNames) {
#   valid_entries <- valid_entries[!is.na(as.Date(paste0(ADAT), "%Y%j"))]
# }
if ("HDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
}
if ("HWAH" %in% colNames) {
  # valid_entries <- valid_entries[HWAH >= 0]
  valid_entries[HWAH < 0, HWAH := 0]
}

if (!"HYEAR" %in% colNames) {
  valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
}
if (!"HMONTH" %in% colNames) {
  valid_entries[,HMONTH:=format(as.Date(paste0(HDAT), "%Y%j"), "%m")]
}
if (!"PYEAR" %in% colNames) {
  valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
}
if (!"GSD" %in% colNames) {
  valid_entries[,`:=`(GSD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PDAT), "%Y%j")))]
}
if (!"ETFD" %in% colNames && "ADAT" %in% colNames && "EDAT" %in% colNames) {
  valid_entries[,`:=`(ETFD = as.integer(as.Date(paste0(ADAT), "%Y%j") - as.Date(paste0(EDAT), "%Y%j")))]
}
if (!"FTHD" %in% colNames && "ADAT" %in% colNames) {
  valid_entries[,`:=`(FTHD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(ADAT), "%Y%j")))]
}
if (!"HARVEST_AREA" %in% colNames) {
  valid_entries[,`:=`(HARVEST_AREA = 1)]
}

if (T %in% (paste0("ADMLV", 0:5) %in% factors)) {
  factors <- unique(c(paste0("ADMLV", 0:c(5:0)[match(T,paste0("ADMLV", 5:0) %in% factors)]), factors))
}

if ((!"ADMLV0" %in% colNames && "ADMLV0" %in% factors)) {
  
  stop(sprintf("admin info is not calculated yet, please run fix-pythia-output script first"))
}

if (!"ADMLVP" %in% colNames) {
  valid_entries[,`:=`(ADMLVP = 1)]
}

if (!"SEASON" %in% colNames && "SEASON" %in% factors) {
  factors <- factors[!factors %in% "SEASON"]
}


print("Starting aggregation.")
# if (argv$period_annual) {
# print("Processing annual calculation.")
calc_production <- valid_entries
# calc_production <- calc_production[,`:=`(HARVEST_AREA_PCT = HARVEST_AREA/sum(HARVEST_AREA*ADMLVP)), by = factors]
# aggregated <- calc_production[,.(HARVEST_AREA_TOT=sum(HARVEST_AREA*ADMLVP)),by = factors]
calc_production <- calc_production[,`:=`(HARVEST_AREA_PCT = HARVEST_AREA/sum(HARVEST_AREA)), by = factors]
# if ("POPULATION" %in% colNames) {
#   calc_production[,POPULATION_FCT := POPULATION / ADMLVP * HARVEST_AREA / sum(HARVEST_AREA), by = .(LATITUDE, LONGITUDE, HYEAR)]
# }

aggregated <- calc_production[,.(HARVEST_AREA_TOT=sum(HARVEST_AREA)),by = factors]
final <- aggregated[, ..factors]
headers <- c()
for (faName in factors) {
  headers <- c(headers, var_dic[name==faName, factor])
}
if ("ADMLV0" %in% factors) {
  if ("ADMLV2" %in% factors) {
    final[, admlv_info := paste(ADMLV0, ADMLV1, ADMLV2, sep = "_")]
  } else if ("ADMLV1" %in% factors) {
    final[, admlv_info := paste(ADMLV0, ADMLV1, sep = "_")]
  } else {
    final[, admlv_info := ADMLV0]
  }
  setcolorder(final, c("admlv_info", colnames(final)[-length(colnames(final))]))
  headers <- c("admlv_info", headers)
}
setnames(final, headers)

# execute predefined variable aggregation
suppressWarnings(if (!is.na(variables)) {
  for (variable in variables) {
    if (variable %in% predefined_vars) {
      print(paste("Processing",  variable))
      if (variable == "TIMESTAMP") {
        if ("year" %in% var_dic[name %in% factors, unit]) {
          aggregated[,(variable):= calc_production[,mean.Date(as.Date(paste0(HDAT), "%Y%j")),by = factors][,V1]]
        } else {
          aggregated[,(variable):= calc_production[,format(as.Date("1970-01-01") + mean(as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d"),by = factors][,V1]]
        }
        final[, timestamp := aggregated[,get(variable)]]
      } else if (variable == "PRODUCTION") {
        calc_production[,(variable) := HARVEST_AREA * HWAH]
        if (multiYearIgnFlg) {
          aggregated[, (variable):= calc_production[,sum(get(variable)), by = factors][,V1]]
        } else {
          aggregated[, (variable):= calc_production[,sum(get(variable)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        final[, production := aggregated[,round(get(variable)/1000, digit = 2)]]
      } else if (variable == "CROP_PER_PERSON") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[,PRODUCTION := HARVEST_AREA * HWAH]
        }
        if (multiYearIgnFlg) {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum(POPULATION), by = factors][,V1]]
        } else {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum(POPULATION), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        final[, crop_per_person := aggregated[,round(get(variable), 1)]]
      } else if (variable == "CROP_PER_DROP") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[,PRODUCTION := HARVEST_AREA * HWAH]
        }
        if (multiYearIgnFlg) {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum((PRCP + IRCM)), by = factors][,V1]]
        } else {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum((PRCP + IRCM)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        final[, crop_per_drop := aggregated[,round(get(variable), 2)]]
      } else if (variable == "CROP_FAILURE_AREA") {
        calc_production[HWAH < cfThreshold, (variable) := HARVEST_AREA]
        calc_production[HWAH >= cfThreshold, (variable) := 0]
        aggregated[, (variable):= calc_production[,sum(get(variable)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        # aggregated[, (variable):= calc_production[,mean(get(variable)), by = factors][,V1]]
        final[, crop_failure_area := aggregated[,round(get(variable), 2)]]
      } else if (variable == "HUNGRY_PEOPLE") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[, PRODUCTION := HARVEST_AREA * HWAH]
        }
        calc_production[, POPULATION_NOCITY := POPULATION]
        calc_production[POPULATION >= quantile(POPULATION, probs = population_threshold), POPULATION_NOCITY := 0]
        
        calc_production_pixel <- unique(calc_production[, .(CROP_PER_PERSON = sum(PRODUCTION)/sum(POPULATION), POPULATION_NOCITY), by = c(unique(c("LATITUDE","LONGITUDE", "HYEAR", factors)))])
        calc_production_pixel[, (variable) := POPULATION_NOCITY]
        calc_production_pixel[CROP_PER_PERSON > hpThreshold, (variable) := 0]
        
        # merge(calc_production_pixel, unique(calc_production[,mget(factors[!factors %in% c("LATITUDE", "LONGITUDE")]), by = .(LATITUDE, LONGITUDE)]), by = c("LATITUDE", "LONGITUDE"), sort = F, all.x=T)
        for (factor in factors) {
          if (!factor %in% colnames(calc_production_pixel)) {
            calc_production_pixel[, (factor) := calc_production[, get(factor), by = .(LATITUDE, LONGITUDE, HYEAR)][, V1]]
          }
        }
        aggregated[, (variable) := calc_production_pixel[, sum(get(variable)), by = factors][,V1]]
        final[, hungry_people := aggregated[,round(get(variable), 0)]]
      }
    } else {
      print(paste("Processing",  variable, ", which is unsupported and skipped"))
    }
  }
})

# execute summary aggregation
suppressWarnings(if (!is.na(totVariables)) {
  for (variable in totVariables) {
    if (!variable %in% colnames(calc_production)) {
      print(paste("Processing summary for",  variable, ", which is missing and skipped"))
      next
    }
    header <- var_dic[name == variable, total]
    if (header != "") {
      print(paste("Processing summary for",  variable))
      
      if (var_dic[name == variable, unit] == "kg/ha") {
        
        calc_production[,(header):= as.numeric(get(variable)) * HARVEST_AREA]
        if (multiYearIgnFlg) {
          aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
        } else {
          aggregated[, (header):= calc_production[,sum(get(header)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        final[, (header):= aggregated[,get(header)]]
        
      } else if (variable == "HARVEST_AREA") {
        typeof(unique(c(factors, "HYEAR")))
        aggregated[, (header):= calc_production[,sum(get(variable)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        final[, (header):= aggregated[,get(header)]]
      } else {
        print(paste("Processing summary for",  variable, ", which is unsupported and skipped"))
      }
      
    } else {
      print(paste("Processing summary for",  variable, ", which is unsupported and skipped"))
    }
  }
})

# execute average aggregation
suppressWarnings(if (!is.na(avgVariables)) {
  for (variable in avgVariables) {
    if (!variable %in% colnames(calc_production)) {
      print(paste("Processing average for",  variable, ", which is missing and skipped"))
      next
    }
    header <- var_dic[name == variable, average]
    if (header != "") {
      print(paste("Processing average for",  variable))
      
      if (var_dic[name == variable, unit] == "date") {
        
        calc_production[,(header):= as.Date(paste0(get(variable)), "%Y%j")]
        if ("year" %in% var_dic[name %in% factors, unit]) {
          aggregated[,(header):= calc_production[,as.Date(sum(as.integer(get(header)) * HARVEST_AREA_PCT), origin="1970-01-01"),by = factors][,V1]]
        } else {
          aggregated[,(header):= calc_production[,format(as.Date("1970-01-01") + sum(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01"))) * HARVEST_AREA_PCT), "%m-%d"),by = factors][,V1]]
        }
        final[, (header) := aggregated[,get(header)]]
        
      } else {
        
        ## header_tot <- var_dic[name == variable, total]
        # header_tot <- paste0(variable, "_TOT")
        # if (!header_tot %in% colnames(calc_production)) {
        #   calc_production[,(header_tot):= get(variable) * HARVEST_AREA]
        # }
        if (multiYearIgnFlg) {
          aggregated[, (header):= calc_production[,sum(as.numeric(get(variable)) * HARVEST_AREA_PCT), by = factors][,V1]]
        } else {
          aggregated[, (header):= calc_production[,sum(get(variable)* HARVEST_AREA)/sum(HARVEST_AREA), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        
        final[, (header):= aggregated[,get(header)]]
        
      }
      
    } else {
      print(paste("Processing average for",  variable, ", which is unsupported and skipped"))
    }
  }
})

# execute total_ton aggregation
suppressWarnings(if (!is.na(totTonVariables)) {
  for (variable in totTonVariables) {
    if (!variable %in% colnames(calc_production)) {
      print(paste("Processing summary (unit=ton) for",  variable, ", which is missing and skipped"))
      next
    }
    header <- var_dic[name == variable, total_ton]
    if (header != "") {
      print(paste("Processing summary (unit=ton) for",  variable))
      
      if (var_dic[name == variable, unit] == "kg/ha") {
        
        calc_production[,(header):= as.numeric(get(variable)) * HARVEST_AREA]
        if (multiYearIgnFlg) {
          aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
        } else {
          aggregated[, (header):= calc_production[,sum(get(header)), by = c(unique(c(factors, "HYEAR")))][,mean(V1), by = factors][,V1]]
        }
        final[, (header):= aggregated[,round(get(header)/1000, digits = 2)]]
        
      } else {
        print(paste("Processing summary (unit=ton) for",  variable, ", which is unsupported and skipped"))
      }
      
    } else {
      print(paste("Processing summary (unit=ton) for",  variable, ", which is unsupported and skipped"))
    }
  }
})

# if ("timestamp" %in% colnames(final) || !"year" %in% var_dic[name %in% factors, unit]) {
#   final[, year:=NULL]
# }

data.table::fwrite(final, file = out_file)
# }
# if (argv$period_month) {
#   #print("Processing monthly calculation.")
#   print("Monthly aggregation has not been supported yet")
# }
# if (argv$period_season) {
#   #print("Processing seasonal calculation.")
#   print("Seasonal aggregation has not been supported yet")
# }

print("Complete.")
