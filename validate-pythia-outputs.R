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

drawMap <- function(plotData, shpData, cateoryText, variable){
  if (plotData[,.N] > 0) {
    setnames(plotData, colnames(plotData)[length(colnames(plotData))], variable)
    plotDataSf <- st_as_sf(plotData, coords = c("LONGITUDE", "LATITUDE"), crs =4326) #converting into spatial data
    st_crs(plotDataSf)
    plot <- ggplot()+
      geom_sf(data=plotDataSf, aes(color=get(variable)))+
      geom_sf(data=shpData, size=0.25, alpha=0.5, fill="grey")+
      labs(x="Longitude", y="Latitude", color=variable)+
      scale_color_gradientn(colours = rainbow(7),
                            n.breaks=6
                            # ,labels=c(0,5, 10, 15, 20, 25)
      )+
      annotation_scale(location = "bl", width_hint = 0.2) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             pad_x = unit(0.05, "in"), pad_y = unit(0.25, "in"),
                             width = unit(0.35, "in"), height = unit(0.35, "in"),
                             style = north_arrow_fancy_orienteering)
    
    
    ggsave(
      plot,
      filename = paste0(base_file_name, "_", variable, "_", cateoryText, ".png"),
      plot = last_plot(),
      # path = out_dir,
      width = 10,
      height = 10,
      limitsize = F
    )
  }
}

p <- argparser::arg_parser("VAlidate Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--output", short = "-o", "Path to the file of validation report")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for validation: [", paste(var_dic[, name], collapse = ","), "]"))
p <- argparser::add_argument(p, "--min", short = "-i", flag = TRUE, help = "Report minimum value for range check")
p <- argparser::add_argument(p, "--max", short = "-a", flag = TRUE, help = "Report maximum value for range check")
p <- argparser::add_argument(p, "--mean", short = "-e", flag = TRUE, help = "Report mean value for range check")
p <- argparser::add_argument(p, "--med", short = "-d", flag = TRUE, help = "Report meadian value for range check")
p <- argparser::add_argument(p, "--std", short = "-s", flag = TRUE, help = "Report standard deviation for range check")
p <- argparser::add_argument(p, "--no-zero", short = "-n", flag = TRUE, help = "Exclude 0 value from range check")
p <- argparser::add_argument(p, "--z-score", short = "-z", default = 2.5, help = "Set Z-Score to detect the outlier values")
p <- argparser::add_argument(p, "--shape_file", short = "-p", help = "Path to the shape file which will used for generating map with outlier values")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv"))
# argv <- argparser::parse_args(p, c("test\\data\\case3", "-o", "test\\output\\report3.csv", "-v", "PDAT", "MDAT", "HDAT","HWAM", "TMAXA", "TMINA", "PRCP", "GSD", "FTHD", "--min","--max","--med", "--std", "-n"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_rf.csv", "-o", "test\\output\\report10\\report10.csv", "-p", "test\\data\\case8\\05d_pt_land2_soil_grid_bnd.shp", "-v", "PDAT", "MDAT", "HDAT","HWAM", "TMAXA", "TMINA", "PRCP", "GSD", "ETFD", "FTHD", "HIAM", "--min","--max", "--mean", "--med", "--std", "-n"))
# argv <- argparser::parse_args(p, c("test\\data\\case11\\pp_GGCMI_SWH_SWheat_rf.csv", "-o", "test\\output\\report11\\report11.csv", "-p", "test\\data\\case8\\05d_pt_land2_soil_grid_bnd.shp", "-v", "PDAT", "MDAT", "HDAT","HWAM", "TMAXA", "TMINA", "PRCP", "GSD", "ETFD", "FTHD", "HIAM", "--min","--max", "--mean", "--med", "--std", "-n"))
# argv <- argparser::parse_args(p, c("test\\data\\case11\\pp_GGCMI_SWH_SWheat_rf.csv", "-o", "test\\output\\report11\\report11.csv", "-p", "test\\data\\case8\\05d_pt_land2_soil_grid_bnd.shp", "-v", "ADMLV0", "PDAT", "MDAT", "HDAT","HWAM", "TMAXA", "TMINA", "PRCP", "GSD", "ETFD", "FTHD", "HIAM", "--min","--max", "--mean", "--med", "--std", "-n"))

suppressWarnings(in_dir <- normalizePath(argv$input))

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- c("HWAH", "EDAT", "MDAT", "ADAT", "HDAT");
})

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}
if (!is.na(argv$output)) {
  suppressWarnings(out_file <- normalizePath(argv$output))
  out_dir <- dirname(out_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  base_file_name <- tools::file_path_sans_ext(basename(out_file))
} else {
  out_dir <- NA
}

# Load shape file if avaiable
shpData <- NA
suppressWarnings(if (!is.na(argv$shape_file)) {
  library(ggplot2)
  library(sf)
  library(raster)
  library(ggspatial)
  library(rnaturalearth)
  library(rnaturalearthdata) #for shapefiles, addition to gadm
  in_shape <- normalizePath(argv$shape_file)
  shpData <- st_read(in_shape) #read shapefile
  crs(shpData)
})

# Read files
flist <- list()
dts <- list()
print("Loading files for validation")
if (!dir.exists(in_dir)) {
  flist <- in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df <- data.table::rbindlist(dts)

# Create pre-processed columns
colNames <- colnames(df)
if (!"HYEAR" %in% colNames) {
  df[,`:=`(HYEAR = trunc(HDAT/1000))]
}
if (!"HMONTH" %in% colNames) {
  df[,HMONTH:=format(as.Date(paste0(HDAT), "%Y%j"), "%m")]
}
if (!"PYEAR" %in% colNames) {
  df[,`:=`(PYEAR = trunc(PDAT/1000))]
}
if (!"GSD" %in% colNames) {
  df[,`:=`(GSD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PDAT), "%Y%j")))]
}
if (!"ETFD" %in% colNames && "ADAT" %in% colNames && "EDAT" %in% colNames) {
  df[,`:=`(ETFD = as.integer(as.Date(paste0(ADAT), "%Y%j") - as.Date(paste0(EDAT), "%Y%j")))]
}
if (!"FTHD" %in% colNames && "ADAT" %in% colNames) {
  df[,`:=`(FTHD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(ADAT), "%Y%j")))]
}
if ((!"ADMLV0" %in% colNames && "ADMLV0" %in% variables) || 
    !"ADMLV1" %in% colNames && "ADMLV1" %in% variables) {
  
  # Use GADM whole world shape file to query the country and region names
  gadmShape <- shapefile("gadm_shapes\\gadm36_1.shp")
  # proj4str <- CRS(proj4string(gadmShape))
  proj4str <- CRS("+init=epsg:4326")
  pixels <- df[,.N,by=.(LONGITUDE,LATITUDE)]
  pixels[,N:=NULL]
  pixelsSP <- SpatialPoints(pixels[,.(LONGITUDE,LATITUDE)],  proj4string=proj4str)
  indices <- over(pixelsSP, gadmShape)
  pixels[,`:=`(ADMLV0=indices$NAME_0,ADMLV1=indices$NAME_1)]
  
  # Fix the edge pixels which might be located on the sea caused by resolution
  pixelsFixed <- pixels[is.na(ADMLV0)]
  incr <- 0.05 # degree increment used for finding nearby location
  maxRetry <- 5 # maximum retry 5 times for searching the land
  cnt <- 1
  while (pixelsFixed[,.N] > 0 && cnt <= maxRetry) {
    diff <- incr * cnt
    cnt <- cnt + 1
    pixelsFixed[,`:=`(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE)]
    pixelsXs <- NULL
    for (i in 1 : pixelsFixed[,.N]) {
      pixelsX <- pixelsFixed[i,.(LONGITUDE,LATITUDE,LONGITUDE_ORG,LATITUDE_ORG)]
      
      pixelsX <- pixelsX[,.(LONGITUDE=LONGITUDE+c(-diff,0,diff,0,-diff,diff,diff,-diff),LATITUDE=LATITUDE+c(0,diff,0,-diff,diff,diff,-diff,-diff), LONGITUDE_ORG = LONGITUDE_ORG, LATITUDE_ORG = LATITUDE_ORG  )]
      if (is.null(pixelsXs)) {
        pixelsXs <- pixelsX
      } else {
        pixelsXs <- rbind(pixelsXs, pixelsX)
      }
    }
    pixelsSPXs <- SpatialPoints(pixelsXs,  proj4string=proj4str)
    indicesXs <- over(pixelsSPXs, gadmShape)
    pixelsXs[,`:=`(ADMLV0=indicesXs$NAME_0,ADMLV1=indicesXs$NAME_1)]
    if (pixelsXs[!is.na(ADMLV1), .N] > 0) {
      pixelsXsResult <- pixelsXs[!is.na(ADMLV1),.(.N, maxN=max(.N)),by=.(ADMLV0,ADMLV1,LONGITUDE_ORG,LATITUDE_ORG)][N==maxN]
      pixelsXsResult <- unique(pixelsXsResult, by=c("LONGITUDE_ORG", "LATITUDE_ORG"))
      pixels[paste0(LONGITUDE, "_", LATITUDE) %in% pixelsXsResult[,paste0(LONGITUDE_ORG, "_", LATITUDE_ORG)], `:=`(ADMLV0 = pixelsXsResult[,ADMLV0], ADMLV1 = pixelsXsResult[,ADMLV1])]
      pixelsFixed <- pixels[is.na(ADMLV1)]
    }
  }
  
  # Fix incorrect country name for PRC
  pixels[ADMLV0 %in% c("Hong Kong", "Taiwan", "Macao"), `:=`(ADMLV1 = ADMLV0, ADMLV0="China")]
  
  # Create factor column for aggregations
  if (!"ADMLV0" %in% colNames && "ADMLV0" %in% variables) {
    df <- merge(df, pixels[,.(LATITUDE,LONGITUDE,ADMLV0)], by=c("LATITUDE","LONGITUDE"), all=T)
  }
  if (!"ADMLV1" %in% colNames && "ADMLV1" %in% variables) {
    df <- merge(df, pixels[,.(LATITUDE,LONGITUDE,ADMLV1)], by=c("LATITUDE","LONGITUDE"), all=T)
  }
  
  # Clear cache
  gadmShape <- NULL
  pixels <- NULL
  pixelsSP <- NULL
  indices <- NULL
}

# create report form
print("Starting validation.")
report <- data.table(Variable=rnorm(0),
                     "Invalid_pct" = rnorm(0),
                     "Invalid_cnt" = rnorm(0),
                     "Zero_pct" = rnorm(0),
                     "Zero_cnt" = rnorm(0))
if (argv$min) report[,`:=`(min=rnorm(0))]
if (argv$med) report[,`:=`(median=rnorm(0))]
if (argv$mean) report[,`:=`(mean=rnorm(0))]
if (argv$max) report[,`:=`(max=rnorm(0))]
if (argv$std) report[,`:=`(std=rnorm(0))]
report[,`:=`(outlier_min_max_pct=rnorm(0))]
report[,`:=`(outlier_min_max_cnt=rnorm(0))]
report[,`:=`(outlier_z_score_pct=rnorm(0))]
report[,`:=`(outlier_z_score_cnt=rnorm(0))]
outlierReportM <- data.table(LATITUDE=rnorm(0),
                             LONGITUDE = rnorm(0),
                             PYEAR = rnorm(0),
                             HYEAR = rnorm(0),
                             RUN_NAME = rnorm(0),
                             CR = rnorm(0))
outlierReportZ <- data.table(LATITUDE=rnorm(0),
                             LONGITUDE = rnorm(0),
                             PYEAR = rnorm(0),
                             HYEAR = rnorm(0),
                             RUN_NAME = rnorm(0),
                             CR = rnorm(0))

# Validate requested variables
for (variable in variables) {
  
  # check if variable is available in the target file
  if (!variable %in% colnames(df)) {
    print(paste("Processing",  variable, ", which is missing and skipped"))
    next
  }
  
  # Load headers
  print(paste("Processing",  variable))
  total <- df[, .N]
  if (variable %in% var_dic[unit == "date", name]) {
    header <- paste0(variable, "_ISO")
    df[, (header) := as.Date(paste0(get(variable)), "%Y%j")]
    invalid <- df[is.na(get(header)), .N]
  } else if (variable %in% var_dic[unit == "text", name]) {
    header <- variable
    invalid <- df[is.na(get(header)) | get(header) == "" | get(header) == "-99", .N]
  } else {
    header <- variable
    invalid <- df[get(header) <= -99, .N]
  }
  
  # count records with valid/invalid values
  if (variable %in% var_dic[unit %in% c("date", "degree_c", "text"), name]) {
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),"","")
  } else {
    zero <- df[get(header) == 0, .N]
    row <- list(variable,
                paste0(round(invalid/total*100, 2), "%"),
                paste0(invalid, "/", total),
                paste0(round(zero/total*100, 2), "%"),
                paste0(zero, "/", total))
  }
  if (variable %in% var_dic[unit == "text", name]) {
    invalid_entries <- df[is.na(get(header)) | get(header) == "" | get(header) == "-99"]
    if (argv$min) {
      row <- c(row, "")
    }
    if (argv$med) {
      row <- c(row, "")
    }
    if (argv$mean) {
      row <- c(row, "")
    }
    if (argv$max) {
      row <- c(row, "")
    }
    if (argv$std) {
      row <- c(row, "")
    }
    row <- c(row, "", "")
    if(!is.na(out_dir) && invalid_entries[,.N] > 0) {
      invalid_entries <- invalid_entries[,.(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tmpvar=get(variable))]
      setnames(invalid_entries, "tmpvar", variable)
      data.table::fwrite(invalid_entries, file = file.path(out_dir, paste0(base_file_name, "_", variable, "_outlier_text.csv")))
      drawMap(invalid_entries[,.N,by=.(LATITUDE, LONGITUDE)], shpData, "outlier_text", variable)
    }
    
  } else {
    if (variable %in% var_dic[unit == "date", name]) {
      valid_entries <- df[!is.na(get(header))][!is.na(PDAT_ISO)]
    } else if (argv$no_zero && !variable %in% var_dic[unit == "degree_c", name]) {
      valid_entries <- df[get(header)> -99 & get(header) != 0]
    } else {
      valid_entries <- df[get(header)> -99]
    }
    
    # calculate min, max, mean, median and std
    if (variable %in% var_dic[unit == "date", name]) {
      if (argv$min) {
        row <- c(row, paste0(valid_entries[,format(as.Date("1970-01-01") + min(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d")]))
      }
      if (argv$med) {
        row <- c(row, paste0(valid_entries[,format(as.Date("1970-01-01") + median(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d")]))
      }
      mean <- valid_entries[,format(as.Date("1970-01-01") + mean(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d")]
      if (argv$mean) {
        row <- c(row, paste0(mean))
      }
      if (argv$max) {
        row <- c(row, paste0(valid_entries[,format(as.Date("1970-01-01") + max(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01")))), "%m-%d")]))
      }
      std <- valid_entries[,sd(as.integer(get(header) - as.Date(paste0(PYEAR, "-01-01"))))]
      if (argv$std) {
        row <- c(row, paste0(std))
      }
    } else {
      if (argv$min) {
        row <- c(row, paste0(valid_entries[,min(get(header))]))
      }
      if (argv$med) {
        row <- c(row, paste0(valid_entries[,median(get(header))]))
      }
      mean <- valid_entries[,mean(get(header))]
      if (argv$mean) {
        row <- c(row, paste0(mean))
      }
      if (argv$max) {
        row <- c(row, paste0(valid_entries[,max(get(header))]))
      }
      std <- valid_entries[,sd(get(variable))]
      if (argv$std) {
        row <- c(row, paste0(std))
      }
    }
    
    
    # Use min/max to detect outlier values
    max <- var_dic[name==variable,max]
    min <- var_dic[name==variable,min]
    outlierReport <- valid_entries[get(header) > max | get(header) < min, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
    outlierReport[, (variable) := tempvar]
    outlierReport[, tempvar := NULL]
    if (is.na(max) && is.na(min)) {
      row <- c(row, NA, NA)
    } else {
      row <- c(row,
               paste0(round(outlierReport[,.N]/total*100, 2), "%"),
               paste0(outlierReport[,.N], "/", total))
    }
    if (outlierReport[,.N] > 0) {
      if (outlierReportM[,.N] == 0) {
        outlierReportM <- outlierReport
      } else {
        outlierReportM <- merge(outlierReportM, outlierReport, by=c("LATITUDE","LONGITUDE","PYEAR","HYEAR","RUN_NAME","CR"), all=T)
      }
    }
    if(!is.na(out_dir) && outlierReport[,.N] > 0) {
      data.table::fwrite(outlierReport, file = file.path(out_dir, paste0(base_file_name, "_", variable, "_outlier_min_max.csv")))
      if (!is.na(shpData)) {
        mapPlotData <- valid_entries[get(header) < min, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
        if(mapPlotData[,.N] > 0) {
          drawMap(mapPlotData[,min(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_min_M", variable)
          drawMap(mapPlotData[,.N,by=.(LATITUDE, LONGITUDE)], shpData, "outlier_min_cnt_M", variable)
        }
        mapPlotData <- valid_entries[get(header) > max, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
        if(mapPlotData[,.N] > 0) {
          drawMap(mapPlotData[,max(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_max_M", variable)
          drawMap(mapPlotData[,.N,by=.(LATITUDE, LONGITUDE)], shpData, "outlier_max_cnt_M", variable)
        }
      }
    }
    
    # Use Z-Score to detect outlier values
    zscore <- argv$`z-score`
    if (variable %in% var_dic[unit == "date", name]) {
      outlierReport <- valid_entries[abs((get(header)-as.Date(paste0(PYEAR, "-", mean)))/std) > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
    } else {
      outlierReport <- valid_entries[abs((get(header)-mean)/std) > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
    }
    outlierReport[, (variable) := tempvar]
    outlierReport[, tempvar := NULL]
    row <- c(row,
             paste0(round(outlierReport[,.N]/total*100, 2), "%"),
             paste0(outlierReport[,.N], "/", total))
    if (outlierReport[,.N] > 0) {
      if (outlierReportZ[,.N] == 0) {
        outlierReportZ <- outlierReport
      } else {
        outlierReportZ <- merge(outlierReportZ, outlierReport, by=c("LATITUDE","LONGITUDE","PYEAR","HYEAR","RUN_NAME","CR"), all=T)
      }
    }
    if(!is.na(out_dir) && outlierReport[,.N] > 0) {
      data.table::fwrite(outlierReport, file = file.path(out_dir, paste0(base_file_name, "_", variable, "_outlier_z_score.csv")))
      if (!is.na(shpData)) {
        mapPlotData <- valid_entries[(mean-get(header))/std > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
        if(mapPlotData[,.N] > 0) {
          drawMap(mapPlotData[,min(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_min_Z", variable)
          drawMap(mapPlotData[,.N,by=.(LATITUDE, LONGITUDE)], shpData, "outlier_min_cnt_Z", variable)
        }
        mapPlotData <- valid_entries[(get(header)-mean)/std > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
        if(mapPlotData[,.N] > 0) {
          drawMap(mapPlotData[,max(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_max_Z", variable)
          drawMap(mapPlotData[,.N,by=.(LATITUDE, LONGITUDE)], shpData, "outlier_max_cnt_Z", variable)
        }
      }
    }
  }
  report <- rbind(report, row)
}
print(report)
if(!is.na(out_dir)) {
  data.table::fwrite(report, file = out_file)
  data.table::fwrite(outlierReportM, file = file.path(out_dir, paste0(base_file_name, "_outlier_min_max.csv")))
  data.table::fwrite(outlierReportZ, file = file.path(out_dir, paste0(base_file_name, paste0("_outlier_z_score_",zscore,".csv"))))
}
print("Complete.")