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

predefined_vars <- c("PRODUCTION", "TIMESTAMP", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "TOTAL_WATER")
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
p <- argparser::add_argument(p, "--lookup_data", short="-d", nargs=1, help="Path to the file which contains the lookup data, like population")
p <- argparser::add_argument(p, "--crop_failure_threshold", short="-c", default = 100, nargs=Inf, help="Threshold to determine if the crop is failure, (kg/ha)")
p <- argparser::add_argument(p, "--low_production_per_person_threshold", short="-l", default = 100, nargs=Inf, help="Threshold to determine if the production per person is low, (kg/person)")
p <- argparser::add_argument(p, "--ignore_multiyear_sum_to_average", short="-i", flag = TRUE, help=paste0("Flag to disable the handling for getting average value against values from multi-years"))
p <- argparser::add_argument(p, "--year_factor_var", short="-y", default = "HYEAR", nargs=Inf, help="The variable which is used as the factor of year")
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
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test8\\baseline\\pythia_out", "test\\data\\case18\\test8\\baseline\\analysis_out\\stage_2_1.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR","RUN_NAME", "HYEAR", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test4\\baseline\\pythia_out", "test\\data\\case18\\test4\\baseline\\analysis_out\\stage_3_1.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR", "HYEAR", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test4\\baseline\\pythia_out", "test\\data\\case18\\test4\\baseline\\analysis_out\\stage_5_1.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "POPULATION", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test4\\baseline\\pythia_out", "test\\data\\case18\\test8\\baseline\\analysis_out\\stage_14_admlv0_1.csv", "-v", "PRODUCTION", "-f","ADMLV0", "HMONTH", "HYEAR", "CR"))
# argv <- argparser::parse_args(p, c("test\\data\\case19\\baseline\\pythia_out", "test\\data\\case19\\baseline\\analysis_out\\stage_4.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR", "RUN_NAME", "SEASON", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case20\\baseline\\pythia_out", "test\\data\\case20\\baseline\\analysis_out\\stage_2.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","LATITUDE", "LONGITUDE", "CR","RUN_NAME", "HYEAR", "SEASON", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test8\\baseline\\pythia_out", "test\\data\\case18\\test4\\baseline\\analysis_out\\stage_5_1.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "POPULATION", "-o", "NICM", "-a", "HWAM", "-f", "ADMLV0", "CR","RUN_NAME", "HYEAR", "-l", "200"))
# argv <- argparser::parse_args(p, c("test\\data\\case21\\pythia_out\\ETH_MZ_2022_pdss", "test\\data\\case21\\analysis_out\\ETH_MZ_2022_pdss\\debug\\stage_7_admlv0.csv", "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","ADMLV0", "CR","MGMT", "HYEAR", "SEASON", "SCENARIO", "-l", "200","-d","test\\data\\case21\\lookup_data\\ETH_population_admlv2.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case22\\pythia_out", "test\\data\\case22\\analysis_out\\stage_7_admlv0.csv", "-v", "PRODUCTION", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM", "-f","ADMLV0", "CR","RUN_NAME", "WYEAR", "-l", "200"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))
lookup_file <- argv$lookup_data
suppressWarnings(if (!is.na(lookup_file)) {lookup_file <- normalizePath(lookup_file)})

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
yearFactor <- argv$year_factor_var
# argv$period_annual <- TRUE

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

lookupData <- NA
suppressWarnings(if(!is.na(lookup_file)) {
  if (file.exists(lookup_file)) {
    lookupData <- data.table::fread((lookup_file))
  }
})

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
if ("PDAT" %in% colNames) {
  valid_entries <- valid_entries[!is.na(as.Date(paste0(PDAT), "%Y%j"))]
}
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
if (!"SYEAR" %in% colNames) {
  valid_entries[,`:=`(SYEAR = trunc(SDAT/1000))]
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
populationVars <- c("LATITUDE", "LONGITUDE")
if ("ADMLVP" %in% colNames) {
  for (admVar in paste0("ADMLV", 0:5)) {
    if (admVar %in% colNames) {
      populationVars <- c(populationVars, admVar)
    }
  }
  populationVars <- c(populationVars, "ADMLVP")
}
# populationFactors <- unique(c(populationVars, factors))
populationFactors <- intersect(populationVars, factors)

if (!yearFactor %in% factors) {
  if ("PYEAR" %in% factors) {
    yearFactor <- "PYEAR"
  } else if ("HYEAR" %in% factors) {
    yearFactor <- "HYEAR"
  } else if ("WYEAR" %in% factors || length(unique(valid_entries[,HYEAR])) == 1) {
    yearFactor <- "WYEAR"
  }
}

if ("LATE_SEASON" %in% colNames) {
  print("Apply late season rules.")
  commonDiff = valid_entries[,.(diff=HYEAR-WYEAR)][,.N,by=diff][N==max(N),diff][1]
  valid_entries[HYEAR-WYEAR!=commonDiff, HYEAR:=WYEAR + commonDiff]
  # valid_entries[HYEAR-PYEAR!=commonDiff,.(LATITUDE,LONGITUDE,PYEAR,HYEAR,GSD,PDAT,HDAT,LATE_SEASON)]
}

# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",.(HARVEST_AREA,.N),by=.(SEASON,HYEAR,MGMT,ADMLV0)]
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",.(.N),by=.(HYEAR,MGMT)]
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",.(.N),by=.(HYEAR)]
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",sum(HARVEST_AREA),by=.(HYEAR)]
# 
# unique(valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",.(.N),by=.(HYEAR,LATITUDE,LONGITUDE)][,N])
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia",.(.N),by=.(HYEAR,LATITUDE,LONGITUDE)][N==18]
# 
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia"&HYEAR%in%c(1988)&LATITUDE==6.2917&LONGITUDE==39.2083,.(.N),by=.(HYEAR)]
# test <- valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia"&ADMLV2=="Guji"&MGMT=="Rainfed High N Inputs"&HYEAR%in%c(1986,1987,1988,1989)&LATITUDE==6.2917&LONGITUDE==39.2083]
# test[,.(WYEAR,PYEAR,HYEAR,SDAT,PDAT,HDAT)]
# valid_entries[SCENARIO=="Planting Window Shift -60 day"&SEASON=="Belg"&ADMLV1=="Oromia"&ADMLV2=="Guji"&HYEAR%in%c(1986,1987,1988,1989)&LATITUDE==6.2917&LONGITUDE==39.2083][,sum(HARVEST_AREA),by=.(HYEAR)]

# valid_entries[SEASON=="Belg"&LATE_SEASON==F]
# test <- data.table::fread("E:\\SSD_User\\Documents\\NetBeansProjects\\Develop\\WM_project\\supermaas-aggregate-pythia-outputs\\test\\data\\case21\\ETH_MZ_2022_N\\pythia_out\\original\\pp.csv")
# unique(test[,.(LATITUDE,LONGITUDE,POPULATION)])[,sum(POPULATION)]

# valid_entries[LATITUDE==10.2917&LONGITUDE==37.2917,sum(HARVEST_AREA),by=.(SCENARIO,HYEAR)]
# valid_entries[SCENARIO=="Fertilizer +0"&HYEAR%in%c(1985,1986)&SEASON=="Meher",sum(HARVEST_AREA),by=.(SEASON,HYEAR)]
# valid_entries[LATITUDE==10.2917&LONGITUDE==37.2917&HYEAR==1986,sum(HARVEST_AREA),by=SCENARIO]
# population[SCENARIO=="Fertilizer +0"&HYEAR%in%c(1985,1986)&SEASON=="Meher",sum(POPULATION),by=.(SEASON,HYEAR,MGMT)]
# population[LATITUDE==10.2917&LONGITUDE==37.2917&SCENARIO=="Planting Window Shift -60 day"&HYEAR%in%c(1985,1986)&SEASON=="Meher",sum(POPULATION),by=.(SEASON,HYEAR,MGMT)]
# population_base[SCENARIO=="Fertilizer +0"&HYEAR%in%c(1985,1986)&SEASON=="Meher",sum(POPULATION),by=.(SEASON,HYEAR,MGMT)]
# population_base<-unique(calc_production[, POPULATION, by = populationFactors])
# population_base[,.N,by=.(LATITUDE, LONGITUDE,HYEAR)][N!=20]
# population[,.N,by=.(LATITUDE, LONGITUDE,HYEAR,SEASON,MGMT,SCENARIO)][N!=1]
# population[,.N,by=.(LATITUDE, LONGITUDE,HYEAR,SEASON,SCENARIO)][N!=4]
# population[,.N,by=.(LATITUDE, LONGITUDE,HYEAR,MGMT,SCENARIO)][N!=2]
# population[,.N,by=.(LATITUDE, LONGITUDE,HYEAR)]
# unique(population[,MGMT])
# unique(population[,.(LATITUDE, LONGITUDE)])[,.N]
# unique(population[,.(HYEAR)])[,.N]
# unique(population[,.(LATITUDE, LONGITUDE)])[,.N] * unique(population[,.(HYEAR)])[,.N]
# unique(population[,.(LATITUDE, LONGITUDE,HYEAR)])[,.N]
# unique(valid_entries[,.(LATITUDE, LONGITUDE)])[,.N]
# unique(valid_entries[,.(HYEAR)])[,.N]
# unique(valid_entries[,.(LATITUDE, LONGITUDE)])[,.N] * unique(valid_entries[,.(HYEAR)])[,.N]
# unique(valid_entries[,.(LATITUDE, LONGITUDE,HYEAR)])[,.N]
# 
# unique(valid_entries[,.(LATITUDE, LONGITUDE,HYEAR)])[,.N,by=.(LATITUDE, LONGITUDE)][N<34]
# unique(valid_entries[,.(LATITUDE, LONGITUDE,PYEAR)])[,.N,by=.(LATITUDE, LONGITUDE)][N!=35]
# unique(df[,.(LATITUDE, LONGITUDE,HYEAR)])[,.N,by=.(LATITUDE, LONGITUDE)][N!=35]
# valid_entries[LATITUDE==9.2917&LONGITUDE==39.0417,.(LATITUDE, LONGITUDE,PYEAR,HYEAR,GSD,PDAT,HDAT,LATE_SEASON)][PYEAR!=HYEAR]
# valid_entries[LATITUDE==9.2917&LONGITUDE==39.0417,.(LATITUDE, LONGITUDE,PYEAR,HYEAR,GSD,PDAT,HDAT,LATE_SEASON,CROSS_YEAR=PYEAR!=HYEAR)]
# data.table::fwrite(valid_entries[LATITUDE==9.2917&LONGITUDE==39.0417,.(LATITUDE, LONGITUDE,PYEAR,HYEAR,GSD,PDAT,HDAT,LATE_SEASON,CROSS_YEAR=PYEAR!=HYEAR)], file = "dump.csv")
# unique(population_base[,POPULATION,by=.(LATITUDE, LONGITUDE,HYEAR,MGMT,SCENARIO)])[,sum(POPULATION),by=.(HYEAR,SCENARIO)]

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

# calculate the population
if (!is.na(lookupData) && !F %in% (c(populationFactors, "POPULATION") %in% colnames(lookupData))) {
  print("Apply population data from lookup table instead of original Pythia output")
  population <- lookupData[,.(POPULATION=sum(POPULATION)), by = populationFactors]
  aggregated <- merge(aggregated, population, by = populationFactors, sort = F)
} else if ("POPULATION" %in% colNames) {
  print(paste0("Try to apply the population data, but require it come with <", paste0(populationFactors, collapse = ","), ">"))
  print("Will use the population data from original Pythia output")
  population <- unique(calc_production[, POPULATION, by = populationVars])[, .(POPULATION=sum(POPULATION)), by = populationFactors]
  aggregated <- merge(aggregated, population, by = populationFactors, sort = F)
}

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
          aggregated[, (variable):= calc_production[,sum(get(variable)), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
        }
        final[, production := aggregated[,round(get(variable)/1000, digit = 2)]]
      } else if (variable == "CROP_PER_PERSON") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[,PRODUCTION := HARVEST_AREA * HWAH]
          if (multiYearIgnFlg) {
            aggregated[, PRODUCTION := calc_production[,sum(PRODUCTION), by = factors][,V1]]
          } else {
            aggregated[, PRODUCTION := calc_production[,sum(PRODUCTION), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
          }
        }
        aggregated[, (variable) := PRODUCTION / POPULATION]
        final[, crop_per_person := aggregated[,round(get(variable), 1)]]
      } else if (variable == "CROP_PER_DROP") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[,PRODUCTION := HARVEST_AREA * HWAH]
        }
        if (multiYearIgnFlg) {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum((PRCP + IRCM) * HARVEST_AREA), by = factors][is.na(V1),V1:=0][,V1]]
        } else {
          aggregated[, (variable):= calc_production[,sum(PRODUCTION)/sum((PRCP + IRCM) * HARVEST_AREA), by = c(unique(c(factors, yearFactor)))][is.na(V1),V1:=0][,mean(V1), by = factors][,V1]]
        }
        final[, crop_per_drop := aggregated[,round(get(variable), 2)]]
        final[is.na(crop_per_drop), crop_per_drop := 0]
      } else if (variable == "CROP_FAILURE_AREA") {
        calc_production[HWAH < cfThreshold, (variable) := HARVEST_AREA]
        calc_production[HWAH >= cfThreshold, (variable) := 0]
        aggregated[, (variable):= calc_production[,sum(get(variable)), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
        # aggregated[, (variable):= calc_production[,mean(get(variable)), by = factors][,V1]]
        final[, crop_failure_area := aggregated[,round(get(variable), 2)]]
      } else if (variable == "TOTAL_WATER") {
        if (multiYearIgnFlg) {
          aggregated[, (variable):= calc_production[,sum((PRCP + IRCM) * HARVEST_AREA), by = factors][,V1]]
        } else {
          aggregated[, (variable):= calc_production[,sum((PRCP + IRCM) * HARVEST_AREA), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
        }
        final[, total_water := aggregated[,get(variable)*10]]
      } else if (variable == "HUNGRY_PEOPLE") {
        if (!"PRODUCTION" %in% colnames(calc_production)) {
          calc_production[, PRODUCTION := HARVEST_AREA * HWAH]
        }
        pixelFactors <- c("LATITUDE","LONGITUDE", yearFactor)
        if ("SEASON" %in% colnames(calc_production)) {
          pixelPopulationFactors <- c("LATITUDE","LONGITUDE",yearFactor,"RUN_NAME","SEASON")
        } else {
          pixelPopulationFactors <- c("LATITUDE","LONGITUDE",yearFactor,"RUN_NAME")
        }
        
        calc_production_pixel <- calc_production[,.(PRODUCTION = sum(PRODUCTION)), by = pixelFactors]
        calc_production_pixel[,POPULATION := calc_production[,sum(POPULATION), by = pixelPopulationFactors][,mean(V1), by = pixelFactors][,V1]]
        calc_production_pixel[,CROP_PER_PERSON := PRODUCTION / POPULATION]
        
        population_threshold_num <- calc_production_pixel[,quantile(POPULATION, probs = population_threshold),by = HYEAR][,mean(V1)]
        calc_production_pixel_nocity <- calc_production_pixel[POPULATION < population_threshold_num]
        calc_production_pixel_nocity[,HUNGRY_PEOPLE := 0][CROP_PER_PERSON < hpThreshold, HUNGRY_PEOPLE := POPULATION]
        
        calc_production <- merge(calc_production, calc_production_pixel_nocity[,mget(c(pixelFactors, "HUNGRY_PEOPLE"))], by = pixelFactors, sort = F, all.x=T)
        calc_production[is.na(HUNGRY_PEOPLE), HUNGRY_PEOPLE := 0]
        calc_production[, HUNGRY_PEOPLE := HUNGRY_PEOPLE * ADMLVP]
        
        aggregated[, (variable) := calc_production[, sum(HUNGRY_PEOPLE), by = populationFactors][,mean(V1), by = factors][,V1]]
        final[, hungry_people := aggregated[,get(variable)]]
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
          aggregated[, (header):= calc_production[,sum(get(header)), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
        }
        final[, (header):= aggregated[,get(header)]]
        
      } else if (variable == "HARVEST_AREA") {
        aggregated[, (header):= calc_production[,sum(get(variable)), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
        final[, (header):= aggregated[,get(header)]]
      } else if (variable == "POPULATION") {
        # aggregated[, (header):= calc_production[,sum(get(variable)), by = populationFactors][,mean(V1), by = factors][,V1]]
        final[, (header):= aggregated[,POPULATION]]
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
          aggregated[, (header):= calc_production[,sum(get(variable)* HARVEST_AREA)/sum(HARVEST_AREA), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
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
          aggregated[, (header):= calc_production[,sum(get(header)), by = c(unique(c(factors, yearFactor)))][,mean(V1), by = factors][,V1]]
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

if ("month" %in% colnames(final)) {
  dataGroups <- unique(final[,mget(headers[!headers %in% "month"])])
  groupSize <- nrow(dataGroups)
  dataGroups <- dataGroups[rep(seq_len(groupSize), 12), ]
  monthsList <- c()
  for (i in 1:12) {
    monthsList <- c(monthsList, rep(i, groupSize))
  }
  dataGroups[, month := monthsList]
  setcolorder(dataGroups, headers)
  final <- merge(dataGroups, final, by = headers, sort = F, all.x = T)
  
  for (header in colnames(final)) {
    if (!header %in% headers) {
      final[is.na(get(header)), (header) := 0]
    }
  }
}

# if ("timestamp" %in% colnames(final) || !"year" %in% var_dic[name %in% factors, unit]) {
#   final[, year:=NULL]
# }
cat(paste0("Save result to ", out_file), " ...")
data.table::fwrite(final, file = out_file)
cat("done\r\n")
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
