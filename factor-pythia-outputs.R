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


p <- argparser::arg_parser("Pre-parsing the factor from RUN_NAME column on Pythia outputs in order to do the following validation and aggregation for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output directory or file to aggregate")
p <- argparser::add_argument(p, "title", help=paste0("Title for the new factor"))
p <- argparser::add_argument(p, "--values", short="-v", nargs=Inf, help=paste0("Provide the list of values for this factor"))
p <- argparser::add_argument(p, "--unit", short="-v", default="text", help=paste0("Provide the unit for this factor"))
p <- argparser::add_argument(p, "--keep_original", short="-o", flag = TRUE, help=paste0("Keep Overwrite the original file. If missing, will use 'modified_' as the prefix for the file name"))

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case1", "SEASON", "-v", "Belg", "Dummy"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg", "SEASON", "--values", "Belg", "-o"))

suppressWarnings(in_dir <- normalizePath(argv$input))

title <- argv$title
values <- argv$values
unit <- argv$unit

isKeepOriginal <- argv$keep_original
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
  cat(paste0("Processing ", f, "\n"))
  valid_entries <- data.table::fread(f)
  
  if (isKeepOriginal) {
    cat("Create backup ...")
    file.rename(f, file.path(backupDir, basename(f)))
    cat("done\n")
  }
  
  colNames <- colnames(valid_entries)
  if (!"RUN_NAME" %in% colNames) {
    cat("RUN_NAME is missing, skip this file\n")
    next
  }
  
  for (value in values) {
    valid_entries[grepl(tolower(value), tolower(RUN_NAME), fixed=T), (title) := value]
  }

  data.table::fwrite(valid_entries, file = f)
}

if (var_dic[name==title,.N] == 0) {
  cat(paste0("Update DATA_CDE.csv file to add factor ", title))
  var_dic <- rbind(var_dic, list(title, unit,NA,NA,tolower(title)), fill=T)
  headers <- colnames(var_dic)
  
  for (header in headers[5:length(headers)]) {
    var_dic[get(header)=="",(header):=NA]
  }
  data.table::fwrite(var_dic, file = data_cde_file)
}


