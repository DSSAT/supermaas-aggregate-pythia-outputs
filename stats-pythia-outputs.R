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
predefined_percentiles <- c(0, 5, 25,50, 75, 95, 100)
default_factors <- c("ADMLV0", "ADMLV1")
default_vars <- c("HWAH")

p <- argparser::arg_parser("Generate statistics report based on Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output result directory or file for statistics")
p <- argparser::add_argument(p, "output", "statistics report CSV file")
p <- argparser::add_argument(p, "--is_aggregated", short="-i", flag = TRUE, help=paste0("Indicate the input folder or file is already the result of aggregation script"))
# p <- argparser::add_argument(p, "--boxplot", short="-b", help = "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help=paste0("Variable names for predefined aggregation: [", paste(predefined_vars, collapse=","), "]"))
p <- argparser::add_argument(p, "--total", short="-t", nargs=Inf, help=paste0("Variable names for summary aggregation: [", paste(var_dic[total!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--average", short="-a", nargs=Inf, help=paste0("Variable names for average aggregation: [", paste(var_dic[average!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--total_ton", short="-o", nargs=Inf, help=paste0("Variable names for summary aggregation with unit of ton and round to integer: [", paste(var_dic[total_ton!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for statistics: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will be ", paste(default_factors, collapse = ",")))
p <- argparser::add_argument(p, "--factors_agg", short="-c", nargs=Inf, help=paste0("Factor names for aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "], by default will use factors for statistics plus HYEAR"))
p <- argparser::add_argument(p, "--gadm_path", short="-g", default = "gadm_shapes", nargs=Inf, help="Path to the GADM shape file forlder")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "test\\output\\report1.csv", "-v", "PRODUCTION", "TIMESTAMP", "-t", "CWAM", "HWAH", "-a", "HDAT", "MDAT", "CWAM", "HWAH", "-o", "CWAM", "HWAH", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2.csv", "-v", "PRODUCTION", "-t", "CWAM", "HWAH", "-a", "MDAT", "CWAM", "HWAH", "-o", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case2", "test\\output\\report2_dev.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case5\\ETH_Maize_irrig", "test\\data\\case5\\report5.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case6", "test\\output\\report6.csv", "-a", "HWAH", "GSD", "ETFD", "FTHD", "HIAM", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case6\\pp_GGCMI_Maize_ir.csv", "test\\output\\report6.csv", "-a", "PRCP", "HWAH", "-f", "LATITUDE", "LONGITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2.csv", "-a", "PRCP", "HWAH", "-f","LONGITUDE", "LATITUDE"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2_country.csv", "-a", "PRCP", "HWAH", "-f","ADMLV0"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "test\\data\\case10\\agg_pp_GGCMI_Maize_ir2_region.csv", "-a", "PRCP", "HWAH", "-f","ADMLV1"))
# argv <- argparser::parse_args(p, c("test\\data\\case11\\pp_GGCMI_SWH_SWheat_rf.csv", "test\\data\\case11\\agg_pp_GGCMI_SWH_SWheat_rf_country.csv", "-a", "PRCP", "HWAH", "-f","ADMLV0"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg\\pp_ETH_Maize_irrig_belg_S_season_base__fen_tot0.csv", "test\\data\\case12\\Maize_Belg\\pp_ETH_Maize_irrig_belg_S_season_base__fen_tot0_region.csv", "-a", "PRCP", "HWAH", "-f","ADMLV1", "HYEAR"))

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
aggFactors <- argv$factor_agg
suppressWarnings(if (is.na(aggFactors)) {
  aggFactors <- c(factors, "HYEAR")
})

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

out_dir <- dirname(out_file)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# boxplot <- argv$boxplot
# if (!is.na(boxplot) && !dir.exists(dirname(boxplot))) {
#   dir.create(dirname(boxplot), recursive = TRUE)
# }

flist <- list()
dts <- list()
print("Loading files for statistics")
if (!argv$is_aggregated) {
  print("Load file and run aggregation")
  tmpFile <- file.path("tmp", paste0(as.integer(Sys.time()), ".csv"))
  runCommand <- paste("Rscript","aggregate-pythia-outputs.R", in_dir, tmpFile, "-g", argv$gadm_path)
  suppressWarnings(if (!is.na(variables)) {
    runCommand <- paste(runCommand, "-v", paste(variables, collapse = " "))
  })
  suppressWarnings(if (!is.na(totVariables)) {
    runCommand <- paste(runCommand, "-t", paste(totVariables, collapse = " "))
  })
  suppressWarnings(if (!is.na(avgVariables)) {
    runCommand <- paste(runCommand, "-a", paste(avgVariables, collapse = " "))
  })
  suppressWarnings(if (!is.na(totTonVariables)) {
    runCommand <- paste(runCommand, "-o", paste(totTonVariables, collapse = " "))
  })
  suppressWarnings(if (!is.na(factors)) {
    runCommand <- paste(runCommand, "-f", paste(aggFactors, collapse = " "))
  })
  suppressWarnings(ret <- system(runCommand, show.output.on.console=FALSE))
  if (ret != 0) {
    print("Error happened during aggregation, process quit. Please run aggregation script separatedly to pre-check your run.")
    q()
  }
  
  df <- data.table::fread(tmpFile)
  if (!file.remove(tmpFile)) {
    print(paste0("Temporary file ", tmpFile, " is left without cleaned out"))
  }
  
} else {
  
  if (!dir.exists(in_dir)) {
    flist <- in_dir
  } else {
    flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
  }
  for(f in flist) {
    dts <- c(dts, list(data.table::fread(f)))
  }
  df <- data.table::rbindlist(dts)
}
valid_entries <- df
# colNames <- colnames(valid_entries)

print("Starting statistics.")
final <- valid_entries[,.(Var = predefined_percentiles), by = c(var_dic[name %in% factors, factor])]
suppressWarnings(if (!is.na(variables)) {
  for (variable in variables) {
    print(paste("Processing percentile calculation for",  variable))
    header <- tolower(variable)
    if (variable == "TIMESTAMP") {
      pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=as.Date(quantile(as.integer(get(header)), probs=predefined_percentiles/100), origin="1970-01-01")), by = c(var_dic[name %in% factors, factor])]
    } else {
      pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=quantile(get(header), probs=predefined_percentiles/100)), by = c(var_dic[name %in% factors, factor])]
    }
    setnames(pcts, "PCTVAL", header)
    final <- merge(final, pcts, by = c(var_dic[name %in% factors, factor], "Var"))
  }
})
suppressWarnings(if (!is.na(totVariables)) {
  for (variable in totVariables) {
    header <- var_dic[name == variable, total]
    print(paste("Processing percentile calculation for",  header))
    pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=quantile(get(header), probs=predefined_percentiles/100)), by = c(var_dic[name %in% factors, factor])]
    setnames(pcts, "PCTVAL", header)
    final <- merge(final, pcts, by = c(var_dic[name %in% factors, factor], "Var"))
  }
})
suppressWarnings(if (!is.na(avgVariables)) {
  for (variable in avgVariables) {
    header <- var_dic[name == variable, average]
    print(paste("Processing percentile calculation for",  header))
    if (var_dic[name == variable, unit] == "date") {
      pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=as.Date(quantile(as.integer(get(header)), probs=predefined_percentiles/100), origin="1970-01-01")), by = c(var_dic[name %in% factors, factor])]
    } else {
      pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=quantile(get(header), probs=predefined_percentiles/100)), by = c(var_dic[name %in% factors, factor])]
    }
    setnames(pcts, "PCTVAL", header)
    final <- merge(final, pcts, by = c(var_dic[name %in% factors, factor], "Var"))
  }
})
suppressWarnings(if (!is.na(totTonVariables)) {
  for (variable in totTonVariables) {
    header <- var_dic[name == variable, total_ton]
    print(paste("Processing percentile calculation for",  header))
    pcts <- valid_entries[, .(Var = predefined_percentiles, PCTVAL=quantile(get(header), probs=predefined_percentiles/100)), by = c(var_dic[name %in% factors, factor])]
    setnames(pcts, "PCTVAL", header)
    final <- merge(final, pcts, by = c(var_dic[name %in% factors, factor], "Var"))
  }
})
final[,Var:=paste0(Var, "PCT")][Var == "0PCT", Var := "MIN"][Var == "100PCT", Var := "MAX"]

data.table::fwrite(final, file = out_file)

print("Complete.")
