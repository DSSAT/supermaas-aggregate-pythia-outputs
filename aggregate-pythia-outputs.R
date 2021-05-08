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

predefined_vars <- c("PRODUCTION", "TIMESTAMP","")
default_factors <- c("LATITUDE", "LONGITUDE", "HYEAR")

p <- argparser::arg_parser("Aggregate Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "output", "final output of the aggregated files")
p <- argparser::add_argument(p, "--variables", short="-v", nargs=Inf, help=paste0("Variable names for predefined aggregation: [", paste(predefined_vars, collapse=","), "]"))
p <- argparser::add_argument(p, "--total", short="-t", nargs=Inf, help=paste0("Variable names for summary aggregation: [", paste(var_dic[total!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--average", short="-a", nargs=Inf, help=paste0("Variable names for average aggregation: [", paste(var_dic[average!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--total_ton", short="-o", nargs=Inf, help=paste0("Variable names for summary aggregation with unit of ton and round to integer: [", paste(var_dic[total_ton!="",name], collapse=","), "]"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for summary aggregation: [", paste(unique(var_dic[factor!="", name]), collapse=","), "]"))
# p <- argparser::add_argument(p, "--period_annual", short="-a", flag=TRUE, help="Do the aggregation by year")
# p <- argparser::add_argument(p, "--period_month", short="-m", flag=TRUE, help="Do the aggregation by month")
# p <- argparser::add_argument(p, "--period_season", short="-s", flag=TRUE, help="Do the aggregation by growing season")
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

colNames <- colnames(valid_entries)
if (!"HYEAR" %in% colNames) {
  valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
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

if ((!"ADMLV0" %in% colNames && "ADMLV0" %in% factors) || 
    !"ADMLV1" %in% colNames && "ADMLV1" %in% factors) {
  
  # Use GADM whole world shape file to query the country and region names
  gadmShape <- shapefile("gadm_shapes\\gadm36_1.shp")

  # proj4str <- CRS(proj4string(gadmShape))
  proj4str <- CRS("+init=epsg:4326")
  pixels <- valid_entries[,.N,by=.(LONGITUDE,LATITUDE)]
  pixels[,N:=NULL]
  pixelsSP <- SpatialPoints(pixels[,.(LONGITUDE,LATITUDE)],  proj4string=proj4str)
  # gridded(pixelsSP) = TRUE
  indices <- over(pixelsSP, gadmShape)
  pixels[,`:=`(ADMLV0=indices$NAME_0,ADMLV1=indices$NAME_1)]
  
  # Fix the edge pixels which might be located on the sea caused by resolution
  pixelsFixed <- pixels[is.na(ADMLV0)]
  if (pixelsFixed[,.N] > 0) {
    cat(paste0("Detect ", pixelsFixed[,.N], " pixels located on the sea, try to locate neareast land to get the names of countries and regions...\n"))
    incr <- 0.05 # degree increment used for finding nearby location
    maxRetry <- 5 # maximum retry 5 times for searching the land
    for (i in 1 : pixelsFixed[,.N]) {
      pixelsX <- pixelsFixed[i,.(LONGITUDE,LATITUDE)]
      cat(paste0("[", i,"/",pixelsFixed[,.N],"] "))
      cat(paste0("Resolve Long/Lat: ", paste(pixelsFixed[i,.(LONGITUDE,LATITUDE)], collapse = "  ") , " ..."))
      for (cnt in 1 : maxRetry) {
        diff <- incr * cnt
        pixelsX <- pixelsX[,.(LONGITUDE=LONGITUDE+c(-diff,0,diff,0,-diff,diff,diff,-diff),LATITUDE=LATITUDE+c(0,diff,0,-diff,diff,diff,-diff,-diff))]
        pixelsSPX <- SpatialPoints(pixelsX,  proj4string=proj4str)
        indicesX <- over(pixelsSPX, gadmShape)
        pixelsX[,`:=`(ADMLV0=indicesX$NAME_0,ADMLV1=indicesX$NAME_1)]
        if (pixelsX[!is.na(ADMLV1), .N] > 0) {
          pixelsXResult <- pixelsX[!is.na(ADMLV1),.(.N),by=.(ADMLV0,ADMLV1)][N==max(N)]
          # pixelsFixed[i,`:=`(ADMLV0 = pixelsXResult[1,ADMLV0], ADMLV1 = pixelsXResult[1,ADMLV1])]
          pixels[LATITUDE == pixelsFixed[i,LATITUDE] & LONGITUDE == pixelsFixed[i,LONGITUDE], `:=`(ADMLV0 = pixelsXResult[1,ADMLV0], ADMLV1 = pixelsXResult[1,ADMLV1])]
          break
        }
      }
      if (cnt > maxRetry) {
        cat("failed")
        cat("\n")
      } else {
        cat("\r")
      }
    }
    cat("Done resolving                                                                                \n")
  }
  
  # Fix incorrect country name for PRC
  pixels[ADMLV0 %in% c("Hong Kong", "Taiwan", "Macao"), `:=`(ADMLV1 = ADMLV0, ADMLV0="China")]

  # Create factor column for aggregations
  if (!"ADMLV0" %in% colNames && "ADMLV0" %in% factors) {
    valid_entries <- merge(valid_entries, pixels[,.(LATITUDE,LONGITUDE,ADMLV0)], by=c("LATITUDE","LONGITUDE"), all=T)
  }
  if (!"ADMLV1" %in% colNames && "ADMLV1" %in% factors) {
    valid_entries <- merge(valid_entries, pixels[,.(LATITUDE,LONGITUDE,ADMLV1)], by=c("LATITUDE","LONGITUDE"), all=T)
  }
  
  # Clear cache
  gadmShape <- NULL
  pixels <- NULL
  pixelsSP <- NULL
  indices <- NULL
}

print("Starting aggregation.")
# if (argv$period_annual) {
# print("Processing annual calculation.")
calc_production <- valid_entries
calc_production <- calc_production[,`:=`(HARVEST_AREA_PCT = HARVEST_AREA/sum(HARVEST_AREA)), by = factors]
aggregated <- calc_production[,.(HARVEST_AREA_TOT=sum(HARVEST_AREA)),by = factors]
final <- aggregated[, ..factors]
headers <- c()
for (faName in factors) {
  headers <- c(headers, var_dic[name==faName, factor])
}
setnames(final, headers)

# execute predefined variable aggregation
suppressWarnings(if (!is.na(variables)) {
  for (variable in variables) {
    if (variable %in% predefined_vars) {
      print(paste("Processing",  variable))
      if (variable == "TIMESTAMP") {
        if ("year" %in% var_dic[name %in% factors, unit]) {
          aggregated[,(variable):= calc_production[,mean.Date(HYEAR),by = factors][,V1]]
        } else {
          aggregated[,(variable):= calc_production[,format(as.Date("1970-01-01") + mean(as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d"),by = factors][,V1]]
        }
        final[, timestamp := aggregated[,get(variable)]]
      } else if (variable == "PRODUCTION") {
        calc_production[,(variable) := HARVEST_AREA * HWAH]
        aggregated[, (variable):= calc_production[,sum(as.numeric(get(variable))), by = factors][,V1]]
        final[, production := aggregated[,round(get(variable)/1000)]]
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
        aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
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
        aggregated[, (header):= calc_production[,sum(as.numeric(get(variable)) * HARVEST_AREA_PCT), by = factors][,V1]]
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
        aggregated[, (header):= calc_production[,sum(get(header)), by = factors][,V1]]
        final[, (header):= aggregated[,round(get(header)/1000)]]
        
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
