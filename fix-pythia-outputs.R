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
  
  # pixelsXs2 <- pixelsXs[,.SD[!is.na(ADMLV1),.N], by=.(LONGITUDE_ORG, LATITUDE_ORG)]
  # pixelsXs2[V1>1]
  pixelsXs <- pixelsXs[,.SD[!is.na(ADMLV1), .(ADMLV0, ADMLV1, total=.N)], by=.(LONGITUDE_ORG, LATITUDE_ORG)]
  pixelsXs <- pixelsXs[,.(portion=.N/mean(total)),by=.(LONGITUDE_ORG, LATITUDE_ORG, ADMLV0, ADMLV1)]
  setnames(pixelsXs, "LONGITUDE_ORG", "LONGITUDE")
  setnames(pixelsXs, "LATITUDE_ORG", "LATITUDE")
  # pixelsXs[portion < 1,.(ADMLV0, ADMLV1), by=.(LONGITUDE,LATITUDE)]
  # pixelsXs[is.na(portion),.(ADMLV0, ADMLV1), by=.(LONGITUDE,LATITUDE)]
  pixels <- merge(pixels, pixelsXs, by=c("LONGITUDE", "LATITUDE"), all=T)
  pixels[is.na(portion)]
  return (pixels)
}

p <- argparser::arg_parser("Pre-calculate the extra variable/columns on Pythia outputs in order to do the following validation and aggregation for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--keep_original", short="-o", flag = TRUE, help=paste0("Keep Overwrite the original file. If missing, will use 'modified_' as the prefix for the file name"))
p <- argparser::add_argument(p, "--skip_admlvl", short="-a", flag = TRUE, help=paste0("Skip the calculation for admin level"))
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "-o", "test\\data\\case1", "-a"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "-o"))
# argv <- argparser::parse_args(p, c("test\\data\\case11\\pp_GGCMI_SWH_SWheat_rf.csv", "test\\data\\case11\\agg_pp_GGCMI_SWH_SWheat_rf_country.csv", "-a", "PRCP", "HWAH", "-f","ADMLV0"))

suppressWarnings(in_dir <- normalizePath(argv$input))

isKeepOriginal <- argv$keep_original
isSkipAdmlvl <- argv$skip_admlvl
variables <- argv$variables

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
    # pixels <- valid_entries[,.N,by=.(LONGITUDE,LATITUDE)]
    # pixels[,N:=NULL]
    
    # prepare the pixles for calculating the admin level info
    pixels <- valid_entries[,.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)]
    latDiff <- pixels[LONGITUDE==pixels[1,LONGITUDE]][order(LATITUDE)][,.(diff = diff(LATITUDE))][,.N, by=diff][N==max(N), diff]
    longDiff <- pixels[LATITUDE==pixels[1,LATITUDE]][order(LONGITUDE)][,.(diff = diff(LONGITUDE))][,.N, by=diff][N==max(N), diff]
    gridNum <- 1
    pixels <- resolveGeoPortion(gadmShape, pixels, longDiff, latDiff, gridNum)
    # pixels <- resolveGeoPortion(gadmShape, valid_entries[,.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)], longDiff, latDiff, 1)
    
    # Fix the edge pixels which might be located on the sea caused by resolution
    maxRetry <- 5 # maximum retry 5 times for searching the land
    cnt <- 1
    while (pixels[is.na(ADMLV1), .N] > 0 && cnt <= maxRetry) {
      pixelsFixed <- resolveGeoPortion(gadmShape, pixels[is.na(ADMLV1),.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)], longDiff, latDiff, gridNum + cnt)
      pixels <- rbind(pixels[!is.na(ADMLV1)], pixelsFixed)
      cnt <- cnt + 1
    }
    # pixels[portion < 1, .N]
    
    # pixelsFixed <- pixels[is.na(ADMLV1),.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)]
    # incr <- 0.05 # degree increment used for finding nearby location
    # 
    # cnt <- 1
    # while (pixelsFixed[,.N] > 0 && cnt <= maxRetry) {
    #   diff <- incr * cnt
    #   cnt <- cnt + 1
    #   # pixelsFixed[,`:=`(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE)]
    #   pixelsXs <- NULL
    #   for (i in 1 : pixelsFixed[,.N]) {
    #     pixelsX <- pixelsFixed[i,.(LONGITUDE,LATITUDE,LONGITUDE_ORG,LATITUDE_ORG)]
    #     
    #     pixelsX <- pixelsX[,.(LONGITUDE=LONGITUDE+c(-diff,0,diff,0,-diff,diff,diff,-diff),LATITUDE=LATITUDE+c(0,diff,0,-diff,diff,diff,-diff,-diff), LONGITUDE_ORG = LONGITUDE_ORG, LATITUDE_ORG = LATITUDE_ORG  )]
    #     if (is.null(pixelsXs)) {
    #       pixelsXs <- pixelsX
    #     } else {
    #       pixelsXs <- rbind(pixelsXs, pixelsX)
    #     }
    #   }
    #   pixelsSPXs <- SpatialPoints(pixelsXs,  proj4string=proj4str)
    #   indicesXs <- over(pixelsSPXs, gadmShape)
    #   pixelsXs[,`:=`(ADMLV0=indicesXs$NAME_0,ADMLV1=indicesXs$NAME_1)]
    #   if (pixelsXs[!is.na(ADMLV1), .N] > 0) {
    #     pixelsXsResult <- pixelsXs[!is.na(ADMLV1),.(.N, maxN=max(.N)),by=.(ADMLV0,ADMLV1,LONGITUDE_ORG,LATITUDE_ORG)][N==maxN]
    #     pixelsXsResult <- unique(pixelsXsResult, by=c("LONGITUDE_ORG", "LATITUDE_ORG"))
    #     pixels[paste0(LONGITUDE, "_", LATITUDE) %in% pixelsXsResult[,paste0(LONGITUDE_ORG, "_", LATITUDE_ORG)], `:=`(ADMLV0 = pixelsXsResult[,ADMLV0], ADMLV1 = pixelsXsResult[,ADMLV1])]
    #     pixelsFixed <- pixels[is.na(ADMLV1)]
    #   }
    # }
    # 
    # # Fix incorrect country name for PRC
    # pixels[ADMLV0 %in% c("Hong Kong", "Taiwan", "Macao"), `:=`(ADMLV1 = ADMLV0, ADMLV0="China")]
    
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