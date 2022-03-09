#library(here)
library(argparser)
library(stringr)
library(png)
library(grid)
library(ggplot2)
library(gridExtra)

setwd(".")

defaultPlotOrders <- c("production", "monthly_production", "average_yield", "crop_per_drop", "crop_per_person", "crop_failure_area", "total_nitrogen_applied")
# defaultPNGLayout <- rbind(c(1,2,2),c(3,4,5),c(6,7,8))
defaultPNGLayout <- rbind(c(2,2,2),c(1,3,4),c(5,6,7))
supportFileTypes <- c("png", "pdf")

p <- argparser::arg_parser("Generate single file with merged plot graphs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "the path to the directory with plot graph files")
p <- argparser::add_argument(p, "output", "Output directory or file for PDF file")
p <- argparser::add_argument(p, "--output_type", short = "-t", nargs = 1, default = "png", help = "The type of merged file, now support png and pdf")
p <- argparser::add_argument(p, "--file_filter", short = "-n", nargs = 1, default = "*.png", help = "The naming pattern of plot files which want to merged")
p <- argparser::add_argument(p, "--is_DSSAT_naming_rule", short="-d", flag = TRUE, help=paste0("Flag for if the file names of plots follow the current DSSAT Pythia naming rule <variable>-<admin level name>.png"))
p <- argparser::add_argument(p, "--output_prefix", short = "-p", nargs = 1, default = "", help = "The prefix text used for generating file name")
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\images", "test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\images2", "-d"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_file <- normalizePath(argv$output))

fileFilter <- argv$file_filter
isDSSATNamingRule <- argv$is_DSSAT_naming_rule
extension <- paste0(".", argv$output_type)
prefix <- argv$output_prefix

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

if (!argv$output_type %in% supportFileTypes) {
  stop(sprintf("Output %s is not supported.", argv$output_type))
}

if (file_test("-f", out_file) && !endsWith(tolower(out_file), extension)) {
  stop(sprintf("Output file name %s does not match with the file type you requested", basename(out_file)))
}

if (endsWith(tolower(out_file), extension)) {
  out_dir <- dirname(out_file)
  file_name <- basename(out_file)
  base_file_name <- tools::file_path_sans_ext(file_name)
} else {
  out_dir <- out_file
  file_name <- paste0(prefix, basename(out_file), extension)
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
  lay <- defaultPNGLayout
  
  for (factor in factors) {
    cat(paste0("Merging plots for ", factor, "..."))
    flistFB <- list.files(path = in_dir, pattern = paste0("*-", factor, ".png"), recursive = FALSE, full.names = TRUE)
    flistFBOrdered <- sort(factor(flistFB, levels = file.path(in_dir, paste0(defaultPlotOrders, "-", factor, ".png"))))
    flistFBOrdered <- as.character(flistFBOrdered)
    
    if (extension == ".png") {
      
      plots <- lapply(flistFBOrdered, function(x){
        img <- as.raster(readPNG(x))
        rasterGrob(img)
      })
      ggsave(file.path(out_dir, paste0(prefix, factor, ".png")),width=20.4, height=26.4, 
             arrangeGrob(grobs = plots, layout_matrix = lay))
      cat("done\r\n")
      
    } else if (extension == ".pdf") {
      
      pdf(file.path(out_dir, paste0(prefix, factor, extension)))
      for (i in 1:length(flistFBOrdered)) {
        f <- flistFBOrdered[i]
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
      
    } else {
      cat("skipped\r\n")
    }
    
  } # end of factor loop
  
} else {
  
  cat(paste0("Merging plots ..."))
  
  if (extension == ".png") {
    
    plots <- lapply(flist, function(x){
      img <- as.raster(readPNG(x))
      rasterGrob(img)
    })
    ggsave(file.path(out_dir, file_name),width=20.4, height=26.4, 
           arrangeGrob(grobs = plots, layout_matrix = lay))
    cat("done\r\n")
    
  } else if (extension == ".pdf") {
    
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
    
  } else {
    cat("skipped\r\n")
  }
  
}

print("Complete.")
