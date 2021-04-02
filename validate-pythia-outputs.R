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
    plotData[,(variable):=V1]
    plotData[,V1:=NULL]
    plotDataSf <- st_as_sf(plotData, coords = c("LONGITUDE", "LATITUDE"), crs =4326) #converting into spatial data
    st_crs(plotDataSf)
    ggplot()+
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
      filename = paste0(base_file_name, "_", variable, "_", cateoryText, ".png"),
      plot = last_plot(),
      path = out_dir
    )
  }
}

p <- argparser::arg_parser("VAlidate Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--output", short = "-o", "Path to the file of validation report")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for validation: [", paste(var_dic[unit != "text", name], collapse = ","), "]"))
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
shp_data <- NA
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
  flist[1] = in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)))
}
df <- data.table::rbindlist(dts)

# Create pre-processed columns
if (!"HYEAR" %in% colnames(df)) {
  df[,`:=`(HYEAR = trunc(HDAT/1000))]
}
if (!"PYEAR" %in% colnames(df)) {
  df[,`:=`(PYEAR = trunc(PDAT/1000))]
}
if (!"GSD" %in% colnames(df)) {
  df[,`:=`(GSD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PDAT), "%Y%j")))]
}
if (!"ETFD" %in% colnames(df) && "ADAT" %in% colnames(df) && "EDAT" %in% colnames(df)) {
  df[,`:=`(ETFD = as.integer(as.Date(paste0(ADAT), "%Y%j") - as.Date(paste0(EDAT), "%Y%j")))]
}
if (!"FTHD" %in% colnames(df) && "ADAT" %in% colnames(df)) {
  df[,`:=`(FTHD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(ADAT), "%Y%j")))]
}

# create report form
print("Starting validation.")
report <- data.table(Variable=rnorm(0),
                     "Invalid_pct" = rnorm(0),
                     "Invalid_cnt" = rnorm(0),
                     "Zero_pct" = rnorm(0),
                     "Zero_cnt" = rnorm(0))
if (argv$min) report[,`:=`(min=rnorm(0))]
if (argv$max) report[,`:=`(max=rnorm(0))]
if (argv$mean) report[,`:=`(mean=rnorm(0))]
if (argv$med) report[,`:=`(median=rnorm(0))]
if (argv$std) report[,`:=`(std=rnorm(0))]
report[,`:=`(outlier_min_max=rnorm(0))]
report[,`:=`(outlier_z_score=rnorm(0))]
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
  } else {
    header <- variable
    invalid <- df[get(header) <= -99, .N]
  }
  
  # count records with valid/invalid values
  if (variable %in% var_dic[unit %in% c("date", "degree_c"), name]) {
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
  if (variable %in% var_dic[unit == "date", name]) {
    valid_entries <- df[!is.na(get(header))]
  } else if (argv$no_zero && !variable %in% var_dic[unit == "degree_c", name]) {
    valid_entries <- df[get(header)> -99 & get(header) != 0]
  } else {
    valid_entries <- df[get(header)> -99]
  }

  # calculate min, max, mean, median and std
  if (argv$min) {
    row <- c(row, paste0(valid_entries[,min(get(header))]))
  }
  if (argv$max) {
    row <- c(row, paste0(valid_entries[,max(get(header))]))
  }
  mean <- valid_entries[,mean(get(header))]
  if (argv$mean) {
    row <- c(row, paste0(mean))
  }
  if (argv$med) {
    row <- c(row, paste0(valid_entries[,median(get(header))]))
  }
  std <- valid_entries[,sd(get(variable))]
  if (argv$std) {
    row <- c(row, paste0(std))
  }

  # Use min/max to detect outlier values
  max <- var_dic[name==variable,max]
  min <- var_dic[name==variable,min]
  outlierReport <- valid_entries[get(header) > max | get(header) < min, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
  outlierReport[, (variable) := tempvar]
  outlierReport[, tempvar := NULL]
  row <- c(row, paste0(outlierReport[,.N]))
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
      drawMap(valid_entries[get(header) < min, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))][,min(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_min_M", variable)
      drawMap(valid_entries[get(header) > max, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))][,max(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, "outlier_max_M", variable)
    }
  }

  # Use Z-Score to detect outlier values
  zscore <- argv$`z-score`
  outlierReport <- valid_entries[abs((get(header)-mean)/std) > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))]
  outlierReport[, (variable) := tempvar]
  outlierReport[, tempvar := NULL]
  row <- c(row, paste0(outlierReport[,.N]))
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
      drawMap(valid_entries[(mean-get(header))/std > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))][,min(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, paste0("outlier_min_Z",zscore), variable)
      drawMap(valid_entries[(get(header)-mean)/std > zscore, .(LATITUDE,LONGITUDE,PYEAR,HYEAR,RUN_NAME,CR,tempvar=get(variable))][,max(tempvar),by=.(LATITUDE, LONGITUDE)], shpData, paste0("outlier_max_Z",zscore), variable)
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
