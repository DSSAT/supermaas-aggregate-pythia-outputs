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

resolveGeoPortion <- function (gadmShape, pixels, longDiff, latDiff, gridNum, maxAdmLv) {
  # gridNum <- 1
  admVars <- paste0("ADMLV", 0:maxAdmLv)
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
  for (i in 0:maxAdmLv) {
    pixelsXs[,(paste0("ADMLV", i)):=indices[paste0("NAME_", i)]]
  }
  # pixelsXs[,`:=`(ADMLV0=indices$NAME_0,ADMLV1=indices$NAME_1)]
  # Fix incorrect country name for PRC
  if (maxAdmLv > 0) {
    pixelsXs[ADMLV0 %in% c("Hong Kong", "Taiwan", "Macao"), `:=`(ADMLV1 = ADMLV0, ADMLV0="China")]
  }
  
  # reorganize data
  pixelsXs <- pixelsXs[,.SD[!is.na(get(paste0("ADMLV", maxAdmLv))),.(cnt=.N), by=c(admVars, "LONGITUDE_ORG", "LATITUDE_ORG")]]
  pixelsXs <- merge(pixelsXs, pixelsXs[,.(total=sum(cnt)),by=.(LONGITUDE_ORG, LATITUDE_ORG)], by=c("LONGITUDE_ORG", "LATITUDE_ORG"), sort = F)
  pixelsXs[,portion := cnt / total][,cnt := NULL][,total := NULL]
  setnames(pixelsXs, "LONGITUDE_ORG", "LONGITUDE")
  setnames(pixelsXs, "LATITUDE_ORG", "LATITUDE")
  pixels <- merge(pixels, pixelsXs, by=c("LONGITUDE", "LATITUDE"), all=T, sort=F)
  return (pixels)
}

p <- argparser::arg_parser("Pre-calculate the extra variable/columns on Pythia outputs in order to do the following validation and aggregation for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "--keep_original", short="-o", flag = TRUE, help=paste0("Keep Overwrite the original file. If missing, will use 'modified_' as the prefix for the file name"))
p <- argparser::add_argument(p, "--skip_admlvl", short="-a", flag = TRUE, help=paste0("Skip the calculation for admin level"))
p <- argparser::add_argument(p, "--grid_num", short="-g", default = 5, help=paste0("Provide the number to divide current pixel into smaller grid to calculate the portion of admin level, and provide 1 to skip the calculation"))
p <- argparser::add_argument(p, "--max_adm_level", short="-l", default = 1, help=paste0("Provide the maximum admin level you want to reach, the higher level will consume more time to finish"))
p <- argparser::add_argument(p, "--filter_variable", short="-v", nargs = 1, help=paste0("Provide the name of variable used for filter the final output"))
p <- argparser::add_argument(p, "--filter_values", short="-u", nargs = Inf, help=paste0("Provide the values of variable used for filter the final output"))
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "-o", "test\\data\\case1", "-a"))
# argv <- argparser::parse_args(p, c("test\\data\\case10\\pp_GGCMI_Maize_ir.csv", "-o", "-g", "6"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg", "-o", "-g", "6"))
# argv <- argparser::parse_args(p, c("test\\data\\case16", "-o", "-g", "6", "-l", "2", "-v", "ADMLV0"))

suppressWarnings(in_dir <- normalizePath(argv$input))

isKeepOriginal <- argv$keep_original
isSkipAdmlvl <- argv$skip_admlvl
gridNum <- argv$grid_num
maxAdmLv <- argv$max_adm_level
fltVar <- argv$filter_variable
fltValues <- argv$filter_values

admVars <- paste0("ADMLV", 0:maxAdmLv)

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
}
for(f in flist) {
  cat(paste0("Processing ", f, "\n"))
  valid_entries <- data.table::fread(f)
  
  colNames <- colnames(valid_entries)
  if (!"HYEAR" %in% colNames) {
    cat("Caculating HYEAR ...")
    valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
    cat("done\n")
  }
  if (!"HMONTH" %in% colNames) {
    cat("Caculating HMONTH ...")
    valid_entries[,HMONTH:=format(as.Date(paste0(HDAT), "%Y%j"), "%m")]
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
  if (!isSkipAdmlvl && (F %in% (admVars %in% colNames))) {
    cat("Caculating Admin Levels ...")
    
    # Use GADM whole world shape file to query the country and region names
    gadmShape <- shapefile(file.path("gadm_shapes", paste0("gadm36_", maxAdmLv, ".shp")))
    
    # proj4str <- CRS(proj4string(gadmShape))
    proj4str <- CRS("+init=epsg:4326")
    
    # prepare the pixles for calculating the admin level info
    pixels <- valid_entries[,.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)]
    
    latDiff <- pixels[,.SD[order(LATITUDE)][,.(diff = diff(LATITUDE))], by=LONGITUDE][,.N, by=diff][N==max(N), diff][1]
    longDiff <- pixels[,.SD[order(LONGITUDE)][,.(diff = diff(LONGITUDE))], by=LATITUDE][,.N, by=diff][N==max(N), diff][1]
    pixels <- resolveGeoPortion(gadmShape, pixels, longDiff, latDiff, gridNum, maxAdmLv)
    
    # Fix the edge pixels which might be located on the sea caused by resolution
    maxRetry <- 5 # maximum retry 5 times for searching the land
    cnt <- 1
    while (pixels[is.na(get(paste0("ADMLV", maxAdmLv))), .N] > 0 && cnt <= maxRetry) {
      pixelsFixed <- resolveGeoPortion(gadmShape, pixels[is.na(get(paste0("ADMLV", maxAdmLv))),.(LONGITUDE_ORG = LONGITUDE, LATITUDE_ORG = LATITUDE),by=.(LONGITUDE,LATITUDE)], longDiff, latDiff, gridNum + cnt, maxAdmLv)
      pixels <- rbind(pixels[!is.na(get(paste0("ADMLV", maxAdmLv)))], pixelsFixed)
      cnt <- cnt + 1
    }
    
    # Create factor column for aggregations
    pixels[, LONGITUDE_ORG := NULL][, LATITUDE_ORG := NULL]
    setnames(pixels, "portion", "ADMLVP")
    
    for (admVar in admVars) {
      if (admVar %in% colNames) {
        valid_entries[, (admVar) := NULL]
      }
    }
    if ("ADMLVP" %in% colNames) {
      valid_entries[,HARVEST_AREA := round(HARVEST_AREA/ADMLVP, digit = 1)]
      valid_entries[,ADMLVP := NULL]
      valid_entries[,HYEAR:=NULL][,HMONTH:=NULL][,PYEAR:=NULL][,GSD:=NULL]
      valid_entries <- unique(valid_entries)
    }
    
    valid_entries <- merge(pixels, valid_entries, by=c("LATITUDE","LONGITUDE"), all=T, allow.cartesian=TRUE, allow.by=.EACHI, sort = F)
    
    # update harvest area based on portion
    valid_entries[,HARVEST_AREA := HARVEST_AREA * ADMLVP]
    
    # Clear cache
    gadmShape <- NULL
    pixels <- NULL
    pixelsSP <- NULL
    indices <- NULL
    cat("done\n")
  }
  
  if (!is.na(fltVar)) {
    if (is.na(fltValues)) {
      fltValues <- valid_entries[,.N, by = fltVar][N==max(N), get(fltVar)]
    }

    valid_entries <- valid_entries[get(fltVar) %in% fltValues]
  }
  
  if (isKeepOriginal) {
    cat("Create backup ...")
    file.rename(f, file.path(backupDir, basename(f)))
    cat("done\n")
  }
  
  data.table::fwrite(valid_entries, file = f)
}
print("Complete.")