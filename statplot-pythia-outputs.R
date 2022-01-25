#library(here)
library(argparser)
library(data.table)
library(ggplot2)
library(stringr)

setwd(".")

data_cde_file <- "DATA_CDE.csv"
if (file.exists(data_cde_file)) {
  var_dic <- data.table::fread(data_cde_file)
} else {
  # const_ha_vars <- c("DWAP", "CWAM", "HWAM", "HWAH", "BWAH", "PWAM", "")
  # const_temp_vars <- c("TMAXA", "TMINA")
  # const_date_vars <- c("SDAT", "PDAT", "EDAT", "ADAT", "MDAT", "HDAT")
}

crop_cde_file <- "crop_codes.csv"
if (file.exists(crop_cde_file)) {
  crop_dic <- data.table::fread(crop_cde_file)
}

# predefined_vars <- c("PRODUCTION")
predefined_vars <- c("HWAH")
plotXVarHeader <- var_dic[name == "FILE", factor]

p <- argparser::arg_parser("Generate statistics boxplot based on two aggregation results from Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input_base", "Aggregation result file for baseline data")
p <- argparser::add_argument(p, "input_scenario", "Aggregation result file for scenario data")
p <- argparser::add_argument(p, "output", "folder Path to generaete box plot graphs")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable HEADER names for comparison, if not given, then comparing all non-factor columns"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor != "" & factor != "file", factor]), collapse=","), "]"))
# p <- argparser::add_argument(p, "--max_bar_num", short="-n", default = 25, help = "Maximum number of box bar per graph")

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case17\\agg_result\\agg_crop_per_person_base_adm1.csv", "test\\data\\case17\\agg_result\\agg_crop_per_person_scenario_adm1.csv", "test\\data\\case17\\boxplot", "-f", "ADMLV1"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\baseline\\analysis_out\\stage_7_admlv0.csv", "test\\data\\case18\\scenario\\analysis_out\\stage_7_admlv0.csv", "test\\data\\case18\\debug", "-f", "ADMLV0"))

suppressWarnings(in_dir_base <- normalizePath(argv$input_base))
suppressWarnings(in_dir_scenario <- normalizePath(argv$input_scenario))
suppressWarnings(out_dir <- normalizePath(argv$output))

variables <- argv$variables
factors <- argv$factors
# maxBarNum <- argv$max_bar_num
maxBarNum <- 25

if (!dir.exists(in_dir_base) && !file.exists(in_dir_base)) {
  stop(sprintf("%s does not exist.", in_dir_base))
}

if (!dir.exists(in_dir_scenario) && !file.exists(in_dir_scenario)) {
  stop(sprintf("%s does not exist.", in_dir_scenario))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Process baseline data and calculate threshold
print("Loading files for relative difference comparison")
df_base <- data.table::fread(in_dir_base)[,(plotXVarHeader):="Baseline"]
df_scenario <- data.table::fread(in_dir_scenario)[,(plotXVarHeader):="Scenario"]
df <- data.table::rbindlist(list(df_base, df_scenario))

if (is.na(factors)) {
  headers <- colnames(df)
  plotFactorHeaders <- headers[headers %in% var_dic[factor != "" & factor != "file", factor]]
} else {
  if ("ADMLV1" %in% factors) {
    factors <- unique(c("ADMLV0", factors))
  }
  if ("ADMLV2" %in% factors) {
    factors <- unique(c("ADMLV0", "ADMLV1", factors))
  }
  plotFactorHeaders <- var_dic[name %in% factors, factor]
}

if (is.na(variables)) {
  headers <- colnames(df)
  variables <- headers[!headers %in% var_dic[factor != "", factor]]
  variables <- variables[!variables %in% plotFactorHeaders]
}

print("Generating boxplot graphs...")
extension <- "png"
xLaxAngel <- 90
plotDatas <- split(df, by=plotFactorHeaders, keep.by=FALSE, collapse="__")
plotKeys <- names(plotDatas)

for (variable in variables) {
  for (key in plotKeys) {
    print(paste0("Processing ", variable, " for ", key))
    plotData <- plotDatas[key][[1]]
    rows <- unique(plotData[,c(..plotXVarHeader), ])[order(get(plotXVarHeader))]
    rows[,factor_id:=1:rows[,.N]]
    plotData <- merge(plotData, rows, by=c(plotXVarHeader), all=T)
    factorlist <- rows
    setcolorder(factorlist, c("factor_id", plotXVarHeader))

    factorNum <- factorlist[,.N]
    if (class(plotData[,get(plotXVarHeader)]) != "character") {
      plotData[,(plotXVarHeader):=as.character(get(plotXVarHeader))]
    }
    
    # Title rule: factors list, crop name, variable name (e.g. average yield)
    if (var_dic[name=="CR", factor] %in% colnames(plotData)) {
      crop <- crop_dic[DSSAT_code==plotData[,get(var_dic[name=="CR", factor])][1], Common_name]
    } else {
      crop <- "crop"
    }
    
    if (var_dic[average==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("average ", var_dic[average==variable, boxplot]), sep=", ")
      variableInFile <- paste0("average_", var_dic[average==variable, boxplot])
    } else if (var_dic[total==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("total ", var_dic[total==variable, boxplot]), sep=", ")
      variableInFile <- paste0("total_", var_dic[average==variable, boxplot])
      # } else if (var_dic[total_ton==variable, N] > 0) {
      #   plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("total ", var_dic[total_ton==variable, boxplot], " (ton)"), sep=", ")
    } else {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, variable, sep=", ")
      variableInFile <- variable
    }
    
    for (i in 1:ceiling(factorNum/maxBarNum)) {
      
      plotSubData <- plotData[factor_id %in% (1 + (i-1) * maxBarNum) : (i*maxBarNum)]
      
      plot <- ggplot(data = plotSubData, aes(x = get(plotXVarHeader), y = get(variable))) +
        geom_boxplot(
          # aes(fill = HWAH),
          outlier.colour = NA,
          color = "darkgrey"
        )  +
        stat_boxplot(geom ='errorbar')+
        geom_boxplot()+
        
        coord_cartesian(ylim = range(df[,..variable])) +
        # theme_light() +
        theme(legend.text = element_text(size = 13),
              legend.title = element_text(size = 13)) +
        # theme(axis.text = element_text(size = 13)) +
        theme(axis.title = element_text(size = 13, face = "bold")) +
        labs(y = variableInFile, colour = "Legend", title = plotTitle) +
        theme(axis.text.x = element_text(angle = xLaxAngel, vjust = 0.5, hjust = 1)) +
        theme(panel.grid.minor = element_blank()) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
        theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
      
      # scale_fill_manual(values=colors)
      
      if (ceiling(factorNum/maxBarNum) == 1) {
        file_name <- paste0(variableInFile, "-", str_replace(key, "\\.", "__"), ".", extension)
      } else {
        file_name <- paste0(variableInFile, "-", str_replace(key, "\\.", "__"), "_", i, ".", extension)
      }
      
      ggsave(
        plot,
        filename = file_name,
        # plot = last_plot(),
        path = out_dir
      )
    }
    
  }
}

print("Complete.")