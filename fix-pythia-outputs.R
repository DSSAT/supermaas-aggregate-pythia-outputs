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

resolveGeoPortion <- function (gadmShape, pixels, longDiff, latDiff, gridNum) {
  # gridNum <- 1
  if (gridNum > 1) {
    latIncr <- latDiff / gridNum
    longIncr <- longDiff / gridNum
    pixelDiffs <- data.table(LONGITUDE_Diff=rnorm(0), LATITUDE_Diff=rnorm(0))
    longArr <- seq(-longDiff/2 + longIncr/2, longDiff/2 - longIncr/2, by=longIncr)
    latArr <- seq(-latDiff/2 + latIncr/2, latDiff/2 - latIncr/2, by=latIncr)
    for (x in 1 : length(latArr)) {
      for (y in 1 : length(longArr)) {
        pixelDiffs <- rbind(pixelDiffs, list(longArr[y], latArr[x]))
      }
    }
    pixelsXs <- pixels[,.SD[,.(LONGITUDE=LONGITUDE+pixelDiffs[,LONGITUDE_Diff],LATITUDE=LATITUDE+pixelDiffs[,LATITUDE_Diff])], by=.(LONGITUDE_ORG, LATITUDE_ORG)]
  } else {
    pixelsXs <- pixels
  }
  pixelsSP <- SpatialPoints(pixelsXs[,.(LONGITUDE,LATITUDE)],  proj4string=proj4str)
  
  # calculate the admin level info
  indices <- over(pixelsSP, gadmShape)
  pixelsXs[,`:=`(ADMLV0=indices$NAME_0,ADMLV1=indices$NAME_1)]
  # Fix incorrect country name for PRC
  pixelsXs[ADMLV0 %in% c("Hong Kong", "Taiwan", "Macao"), `:=`(ADMLV1 = ADMLV0, ADMLV0="China")]
  # reorganize data
  pixelsXs <- pixelsXs[,.SD[!is.na(ADMLV1), .(ADMLV0, ADMLV1, total=.N)], by=.(LONGITUDE_ORG, LATITUDE_ORG)]
  pixelsXs <- pixelsXs[,.(portion=.N/mean(total)),by=.(LONGITUDE_ORG, LATITUDE_ORG, ADMLV0, ADMLV1)]
  setnames(pixelsXs, "LONGITUDE_ORG", "LONGITUDE")
  setnames(pixelsXs, "LATITUDE_ORG", "LATITUDE")
  pixels <- merge(pixels, pixelsXs, by=c("LONGITUDE", "LATITUDE"), all=T)
  return (pixels)
}

p <- argparser::arg_parser("Pre-calculate the extra variable/columns on Pythia outputs in order to do the following validation and aggregation for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--keep_original", short="-o", flag = TRUE, help=paste0("Keep Overwrite the original file. If missing, will use 'modified_' as the prefix for the file name"))
p <- argparser::add_argument(p, "--skip_admlvl", short="-a", flag = TRUE, help=paste0("Skip the calculation for admin level"))
p <- argparser::add_argument(p, "--grid_num", short="-g", default = 5, help=paste0("Provide the number to divide current pixel into smaller grid to calculate the portion of admin level, and provide 1 to skip the calculation"))
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "-o", "test\\data\\case1", "-a"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "-o", "-g", "6"))

suppressWarnings(in_dir <- normalizePath(argv$input))

isKeepOriginal <- argv$keep_original
isSkipAdmlvl <- argv$skip_admlvl
gridNum <- argv$grid_num

if (gridNum < 0) {
  gridNum <- 1
}

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

if (isKeepOriginal) {
  if (!dir.exists(in_dir)) {
    backupDir <- file.path(dirname(in_dir), "original")
  } else {
    backupDir <- file.path(in_dir, "original")
  }
  suppressWarnings(dir.create(backupDir, recursive = TRUE))
}

flist <- list()
dts <- list()
cat("Loading files for fixing extra data columns for aggregation.\n")
if (!dir.exists(in_dir)) {
  flist <- in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
  class(flist)
}
for(f in flist) {
  cat(paste0("Processing ", f))
  valid_entries <- data.table::fread(f)
  
  if (isKeepOriginal) {
    cat("Create backup ...")
    file.rename(f, file.path(backupDir, basename(f)))
    cat("done\n")
  }
  
  colNames <- colnames(valid_entries)
  if (!"HYEAR" %in% colNames) {
    cat("Caculating HYEAR ...")
    valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
    cat("done\n")
  }
  if (!"PYEAR" %in% colNames) {
    cat("Caculating PYEAR ...")
    valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
    cat("done\n")
  }
  if (!"GSD" %in% colNames) {
    cat("Caculating GSD ...")
    valid_entries[,`:=`(GSD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PDAT), "%Y%j")))]
    cat("done\n")
  }
  if (!"ETFD" %in% colNames && "ADAT" %in% colNames && "EDAT" %in% colNames) {
    cat("Caculating ETFD ...")
    valid_entries[,`:=`(ETFD = as.integer(as.Date(paste0(ADAT), "%Y%j") - as.Date(paste0(EDAT), "%Y%j")))]
    cat("done\n")
  }
  if (!"FTHD" %in% colNames && "ADAT" %in% colNames) {
    cat("Caculating FTHD ...")
    valid_entries[,`:=`(FTHD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(ADAT), "%Y%j")))]
    cat("done\n")
  }
  if (!"HARVEST_AREA" %in% colNames) {
    cat("Caculating HARVEST_AREA ...")
    valid_entries[,`:=`(HARVEST_AREA = 1)]
    cat("done\n")
  }
  
  if (!isSkipAdmlvl && (!"ADMLV0" %in% colNames || !"ADMLV1" %in% colNames)) {
    cat("Caculating Admin Levels ...")
    
    # Use GADM whole world shape file to query the country and region names
    gadmShape <- shapefile("gadm_shapes\\gadm36_1.shp")
    
    # proj4str <- CRS(proj4string(gadmShape))
    proj4str <- CRS("+init=epsg:4326")
    
    # prepare the pixles for calculating the admin level info
    pixels <- valid_entries[,.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)]
    latDiff <- pixels[LONGITUDE==pixels[1,LONGITUDE]][order(LATITUDE)][,.(diff = diff(LATITUDE))][,.N, by=diff][N==max(N), diff]
    longDiff <- pixels[LATITUDE==pixels[1,LATITUDE]][order(LONGITUDE)][,.(diff = diff(LONGITUDE))][,.N, by=diff][N==max(N), diff]
    pixels <- resolveGeoPortion(gadmShape, pixels, longDiff, latDiff, gridNum)
    
    # Fix the edge pixels which might be located on the sea caused by resolution
    maxRetry <- 5 # maximum retry 5 times for searching the land
    cnt <- 1
    while (pixels[is.na(ADMLV1), .N] > 0 && cnt <= maxRetry) {
      pixelsFixed <- resolveGeoPortion(gadmShape, pixels[is.na(ADMLV1),.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)], longDiff, latDiff, gridNum + cnt)
      pixels <- rbind(pixels[!is.na(ADMLV1)], pixelsFixed)
      cnt <- cnt + 1
    }
    
    # Create factor column for aggregations
    valid_entries <- merge(pixels[,.(LATITUDE,LONGITUDE,ADMLV0,ADMLV1,ADMLVP=portion)], valid_entries, by=c("LATITUDE","LONGITUDE"), all=T, allow.cartesian=TRUE, allow.by=.EACHI)
    
    # Clear cache
    gadmShape <- NULL
    pixels <- NULL
    pixelsSP <- NULL
    indices <- NULL
    cat("done\n")
  }
  
  data.table::fwrite(valid_entries, file = f)
}
