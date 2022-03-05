#library(here)
library(argparser)
library(stringr)
library(png)
library(grid)

setwd(".")

p <- argparser::arg_parser("Generate PDF file with merged plot graphs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "the path to the directory with plot graph files")
p <- argparser::add_argument(p, "output", "Output directory or file for PDF file")
p <- argparser::add_argument(p, "--file_filter", short = "-n", nargs = 1, default = "*.png", help = "The naming pattern of plot files which want to merged")
p <- argparser::add_argument(p, "--is_DSSAT_naming_rule", short="-d", flag = TRUE, help=paste0("Flag for if the file names of plots follow the current DSSAT Pythia naming rule <variable>-<admin level name>.png"))

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\images", "test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\pdf", "-d"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

fileFilter <- argv$file_filter
isDSSATNamingRule <- argv$is_DSSAT_naming_rule

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

if (endsWith(tolower(out_file), ".pdf")) {
  out_dir <- dirname(out_file)
  file_name <- basename(out_file)
  base_file_name <- tools::file_path_sans_ext(file_name)
} else {
  out_dir <- out_file
  file_name <- paste0(basename(out_file), ".pdf")
  base_file_name <- ""
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

flist <- list()
print("Loading plot files for merging.")
if (!dir.exists(in_dir)) {
  flist <- in_dir
} else {
  flist <- list.files(path = in_dir, pattern = fileFilter, recursive = FALSE, full.names = TRUE)
}

if (isDSSATNamingRule) {
  locations <- str_locate(tools::file_path_sans_ext(basename(flist)), "-")
  starts <- locations[seq(1, length(locations)/2)]
  # plotVars <- str_sub(tools::file_path_sans_ext(basename(flist)), 1, starts - 1)
  factors <- unique(str_sub(tools::file_path_sans_ext(basename(flist)), starts + 1))
  
  for (factor in factors) {
    cat(paste0("Merging plots for ", factor, "..."))
    flistFB <- list.files(path = in_dir, pattern = paste0("*-", factor, ".png"), recursive = FALSE, full.names = TRUE)
    pdf(file.path(out_dir, paste0(factor, ".pdf")))
    for (i in 1:length(flistFB)) {
      f <- flistFB[i]
      if (startsWith(basename(f), "monthly_")) {
        grid.raster(readPNG(f), width=unit(0.9, "npc"), height= unit(0.45, "npc"))
      } else {
        grid.raster(readPNG(f), width=unit(0.7, "npc"), height= unit(0.7, "npc"))
      }
      if (i < length(flistFB)) {
        plot.new()
      }
    }
    dev.off()
    cat("done\r\n")
  }
} else {
  cat(paste0("Merging plots ..."))
  pdf(file.path(out_dir, file_name))
  for (i in 1:length(flist)) {
    f <- flist[i]
    if (startsWith(basename(f), "monthly_")) {
      grid.raster(readPNG(f), width=unit(0.9, "npc"), height= unit(0.45, "npc"))
    } else {
      grid.raster(readPNG(f), width=unit(0.7, "npc"), height= unit(0.7, "npc"))
    }
    f<-flist[72]
    if (i < length(flist)) {
      plot.new()
    }
  }
  dev.off()
  cat("done\r\n")
}

print("Complete.")
