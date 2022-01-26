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

p <- argparser::arg_parser("Generate monthly boxplot based on aggregation result from Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Aggregation result file which includes harvest month (HMONTH) as aggregation factor")
p <- argparser::add_argument(p, "output", "Output directory for monthly plot")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable HEADER names for plot, if not given, then plotting all non-factor columns"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor!="", factor]), collapse=","), "]"))

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case17\\agg_result_monthly\\agg_base_monthly_production_Ghana", "test\\data\\case17\\agg_result_monthly\\", "-f","ADMLV0"))
# argv <- argparser::parse_args(p, c("test\\data\\case18\\test\\baseline\\analysis_out\\stage_14_admlv1.csv", "test\\data\\case18\\test\\baseline\\analysis_out\\images", "-f","ADMLV1"))

suppressWarnings(in_file <- normalizePath(argv$input))
suppressWarnings(out_dir <- normalizePath(argv$output))

variables <- argv$variables
factors <- argv$factors

if (!dir.exists(in_file) && !file.exists(in_file)) {
  stop(sprintf("%s does not exist.", in_file))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

print("Loading aggregation result.")
df <- data.table::fread(in_file)[,file := tools::file_path_sans_ext(basename(in_file))]

if ("date" %in% colnames(df)) {
  df[,date_md:=format(as.Date(paste0(date), "%Y%j"), "%m-%d")]
  df <- df[order(rank(date_md))]
} else if ("month" %in% colnames(df)) {
  df <- df[order(rank(month))]
}

suppressWarnings(if (is.na(factors)) {
  headers <- colnames(df)
  plotFactorHeaders <- headers[headers %in% var_dic[factor != "", factor]]
} else {
  if (T %in% (paste0("ADMLV", 0:5) %in% factors)) {
    factors <- unique(c(paste0("ADMLV", 0:c(5:0)[match(T,paste0("ADMLV", 5:0) %in% factors)]), factors))
  }
  plotFactorHeaders <- var_dic[name %in% factors, factor]
})

suppressWarnings(if (is.na(variables)) {
  headers <- colnames(df)
  variables <- headers[!headers %in% var_dic[factor != "", factor]]
})

print("Generating boxplot graphs...")
extension <- "png"
xLaxAngel <- 0
plotDatas <- split(df, by=plotFactorHeaders, keep.by=FALSE, collapse="__")
plotKeys <- names(plotDatas)

for (variable in variables) {
  for (key in plotKeys) {
    print(paste0("Processing ", variable, " for ", key))
    plotData <- plotDatas[key][[1]]
    plotData_mean <- plotData[,.(mean = mean(get(variable))), by = .(month)]
    
    if (var_dic[name=="CR", factor] %in% colnames(plotData)) {
      crop <- crop_dic[DSSAT_code==plotData[,get(var_dic[name=="CR", factor])][1], Common_name]
    } else {
      crop <- "crop"
    }
    
    if (var_dic[average==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("monthly average ", var_dic[average==variable, boxplot]), sep=", ")
      variableInFile <- paste0("monthly_average_", var_dic[average==variable, boxplot])
    } else if (var_dic[total==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("monthly total ", var_dic[total==variable, boxplot]), sep=", ")
      variableInFile <- paste0("monthly_total_", var_dic[average==variable, boxplot])
      # } else if (var_dic[total_ton==variable, N] > 0) {
      #   plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("total ", var_dic[total_ton==variable, boxplot], " (ton)"), sep=", ")
    } else {
      plotTitle <- paste(str_replace_all(key, "__", ", "), crop, paste0("monthly ", variable), sep=", ")
      variableInFile <- paste0("monthly_", variable)
    }
    plotTitle <- str_wrap(plotTitle, 40)
    
    plot <- ggplot(data = plotData, aes(x = month, y = get(variable), group = month)) +
      geom_boxplot(
        outlier.colour = NA,
        color = "darkgrey"
      )  +
      stat_boxplot(geom ='errorbar')+
      geom_boxplot()+
      geom_point(data = plotData_mean, 
                 mapping = aes(x = month, y = mean),
                 color="red")+
      geom_line(data = plotData_mean, 
                mapping = aes(x = month, y = mean, group=1)) +
      
      coord_cartesian(ylim = range(plotData[,..variable])) +
      # coord_cartesian(xlim = c(1,12), ylim = range(plotData[,..variable])) +
      scale_x_continuous(breaks = seq(1, 12, by = 1)) +
      scale_y_continuous(breaks = seq(0, max(plotData[,..variable]), by = signif(max(plotData[,..variable])/10, 1))) +
      
      # theme_light() +
      theme(legend.text = element_text(size = 13),
            legend.title = element_text(size = 13)) +
      theme(axis.title = element_text(size = 13, face = "bold")) +
      labs(x = "Month", y = variableInFile, colour = "Legend", title = plotTitle) +
      theme(axis.text.x = element_text(angle = xLaxAngel, vjust = 0.5, hjust = 1)) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
      theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
    
    
    file_name <- paste0(variableInFile, "-", str_replace(key, "\\.", "__"), ".", extension)
    ggsave(
      plot,
      filename = file_name,
      path = out_dir
    )
  }
}

print("Complete.")