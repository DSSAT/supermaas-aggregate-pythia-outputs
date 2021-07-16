#library(here)
library(argparser)
library(data.table)
library(ggplot2)

setwd(".")

data_cde_file <- "DATA_CDE.csv"
if (file.exists(data_cde_file)) {
  var_dic <- data.table::fread(data_cde_file)
} else {
  # const_ha_vars <- c("DWAP", "CWAM", "HWAM", "HWAH", "BWAH", "PWAM", "")
  # const_temp_vars <- c("TMAXA", "TMINA")
  # const_date_vars <- c("SDAT", "PDAT", "EDAT", "ADAT", "MDAT", "HDAT")
}

# predefined_vars <- c("PRODUCTION")
predefined_vars <- c("HWAH")
# default_factors <- c("file")
predefined_factors <- c("file", "pixel")

p <- argparser::arg_parser("Generate box plotting report based on Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Pythia output result directory or file for statistics")
p <- argparser::add_argument(p, "output", "File Path to generaete box plot graph")
p <- argparser::add_argument(p, "--csv", short="-c", help = "CSV file with final table used for plotting")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for plotting: [", paste(c(predefined_vars, var_dic[!unit %in% c("text", "date"), name]), collapse = ","), "]"))
p <- argparser::add_argument(p, "--factor", short="-f", nargs=1, default="file", help=paste0("Factor names for plotting: [", paste(c(predefined_factors, unique(var_dic[factor!="", name])), collapse=","), "]"))
p <- argparser::add_argument(p, "--keep_invalid", short="-i", flag = TRUE, help=paste0("Keep Invalid value for plotting. If missing, will skip all invalid value"))
p <- argparser::add_argument(p, "--keep_zero", short="-z", flag = TRUE, help=paste0("Keep zero value for plotting. If missing, will skip all zero value"))
p <- argparser::add_argument(p, "--max_bar_num", short="-n", default = 25, help = "Maximum number of box bar per graph")

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg\\original", "test\\data\\case12\\test_file.png", "-v", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg\\original", "test\\data\\case12\\test_hyear.png", "-v", "HWAH", "-f", "HYEAR", "-n", "17"))
# argv <- argparser::parse_args(p, c("test\\data\\case12\\Maize_Belg\\original", "test\\data\\case12\\by_pixel\\test_pixel.png", "-v", "HWAH", "-f", "pixel"))


suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- predefined_vars
})
factor <- argv$factor
# suppressWarnings(if (is.na(factors)) {
#   factors <- default_factors
# })

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

out_dir <- dirname(out_file)
base_file_name <- tools::file_path_sans_ext(basename(out_file))
extension <- tools::file_ext(basename(out_file))

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

csv_file <- argv$csv
suppressWarnings(if (!is.na(csv_file)) {
    if (!dir.exists(dirname(csv_file))) {
      dir.create(dirname(csv_file), recursive = TRUE)
    }
  }
)
maxBarNum <- argv$max_bar_num

flist <- list()
dts <- list()
if (factor == "file") {
  factorlist <- data.table(factor_id=rnorm(0), FILE_NAME=rnorm(0), FILE_PATH=rnorm(0))
} else {
  factorlist <- NA
}
print("Loading files for plotting")
if (!dir.exists(in_dir)) {
  flist <- in_dir
} else {
  flist <- list.files(path = in_dir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}

for(f in flist) {
  table <- data.table::fread(f)
  valid_entries <- table
  colNames <- colnames(valid_entries)
  
  if (!argv$keep_invalid) {
    if ("EDAT" %in% colNames) {
      valid_entries <- valid_entries[!is.na(as.Date(paste0(EDAT), "%Y%j"))]
    }
    if ("MDAT" %in% colNames) {
      valid_entries <- valid_entries[!is.na(as.Date(paste0(MDAT), "%Y%j"))]
    }
    if ("ADAT" %in% colNames) {
      valid_entries <- valid_entries[!is.na(as.Date(paste0(ADAT), "%Y%j"))]
    }
    if ("HDAT" %in% colNames) {
      valid_entries <- valid_entries[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
    }
    if ("HWAH" %in% colNames) {
      valid_entries <- valid_entries[HWAH >= 0]
    }
  }
  
  if (!"HYEAR" %in% colNames && ("HYEAR" %in% variables || "HYEAR" %in% factor)) {
    valid_entries[,`:=`(HYEAR = trunc(HDAT/1000))]
  }
  if (!"PYEAR" %in% colNames && "PYEAR" %in% variables) {
    valid_entries[,`:=`(PYEAR = trunc(PDAT/1000))]
  }
  if (!"GSD" %in% colNames && "GSD" %in% variables) {
    valid_entries[,`:=`(GSD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(PDAT), "%Y%j")))]
  }
  if (!"ETFD" %in% colNames && "ETFD" %in% variables && "ADAT" %in% colNames && "EDAT" %in% colNames) {
    valid_entries[,`:=`(ETFD = as.integer(as.Date(paste0(ADAT), "%Y%j") - as.Date(paste0(EDAT), "%Y%j")))]
  }
  if (!"FTHD" %in% colNames && "FTHD" %in% variables && "ADAT" %in% colNames) {
    valid_entries[,`:=`(FTHD = as.integer(as.Date(paste0(HDAT), "%Y%j") - as.Date(paste0(ADAT), "%Y%j")))]
  }
  
  if (factor == "file") {
    factorlist <- rbind(factorlist, list(factorlist[,.N] + 1, basename(f), dirname(f)))
    dts <- c(dts, list(valid_entries[,factor_id:=toString(factorlist[,.N])][,c(..variables, "factor_id")]))
    # dts <- c(dts, list(valid_entries[,`:=`(factor_id=toString(factorlist[,.N]), file=basename(f))][,c(..variables, "factor_id", ..factor)]))
  } else if (factor == "pixel") {
    dts <- c(dts, list(valid_entries[,c(..variables, "LATITUDE", "LONGITUDE")]))
  } else {
    dts <- c(dts, list(valid_entries[,c(..variables, ..factor)]))
  }
}

valid_entries <- data.table::rbindlist(dts)
if (factor == "file") {
  factorNum <- factorlist[,.N]
} else if (factor == "pixel") {
  rows <- unique(valid_entries[,.(LATITUDE, LONGITUDE)])
  rows[,factor_id:=1:rows[,.N]]
  valid_entries <- merge(valid_entries, rows, by=c("LATITUDE","LONGITUDE"), all=T)
  factorlist <- rows
  setcolorder(factorlist, c("factor_id", "LATITUDE","LONGITUDE"))
  factorNum <- factorlist[,.N]
} else {
  rows <- unique(valid_entries[,c(..factor), ])[order(get(factor))]
  rows[,factor_id:=1:rows[,.N]]
  valid_entries <- merge(valid_entries, rows, by=c(factor), all=T)
  factorlist <- rows
  setcolorder(factorlist, c("factor_id", factor))
  factorNum <- factorlist[,.N]
}

valid_entries$factor_id <- factor(valid_entries$factor_id, 1:factorNum)

print("Starting plotting")
if (factor != "file" && factor != "pixel") {
  xLab <- toupper(factor)
  xColName <- factor
  xLaxAngel <- 90
} else {
  xLab <- paste0(toupper(factor), "_ID")
  xColName <- "factor_id"
  xLaxAngel <- 0
}
for (variable in variables) {
  
  if (argv$keep_zero) {
    plotData <- valid_entries
  } else {
    plotData <- valid_entries[get(variable) != 0]
  }
  
  for (i in 1:ceiling(factorNum/maxBarNum)) {
    
    df <- plotData[factor_id %in% (1 + (i-1) * maxBarNum) : (i*maxBarNum)]
    if (xColName != "factor_id") {
      df[,(xColName):=as.character(get(xColName))]
    }
    ggplot(data = df, aes(x = get(xColName), y = get(variable))) +
      geom_boxplot(
        # aes(fill = HWAH),
        outlier.colour = NA,
        color = "darkgrey"
      )  +
      stat_boxplot(geom ='errorbar')+
      geom_boxplot()+
      
      coord_cartesian(ylim = range(valid_entries[,..variable])) +
      # theme_light() +
      theme(legend.text = element_text(size = 13),
            legend.title = element_text(size = 13)) +
      # theme(axis.text = element_text(size = 13)) +
      theme(axis.title = element_text(size = 13, face = "bold")) +
      labs(x = xLab, y = variable, colour = "Legend", title = paste0("BoxPlot for ", variable)) +
      theme(axis.text.x = element_text(angle = xLaxAngel, vjust = 0.5, hjust = 1)) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
      theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
      
    # scale_fill_manual(values=colors)
    
    
    if (ceiling(factorNum/maxBarNum) == 1) {
      file_name <- paste0(base_file_name, "_", variable, ".", extension)
    } else {
      file_name <- paste0(base_file_name, "_", variable, "_", i, ".", extension)
    }
    
    ggsave(
      filename = file_name,
      plot = last_plot(),
      path = out_dir,
    )
  }
  
}

if (factor == "file" || factor == "pixel") {
  data.table::fwrite(factorlist, file = file.path(out_dir, paste0(base_file_name, "_", factor, "_list.csv")))
}

print("Complete.")

