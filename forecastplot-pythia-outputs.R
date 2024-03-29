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
p <- argparser::add_argument(p, "--group", short="-g", nargs=1, help=paste0("Group name for sub-grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor != "", factor]), collapse=","), "]"))
p <- argparser::add_argument(p, "--same_y_scale", short="-i", flag = TRUE, help=paste0("Flag to apply same scale setup on y axis among the plots"))

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case22\\analysis_out\\ETH_MZ_Mar22_Forecast_Ar\\stage_14_admlv0.csv", "test\\data\\case22\\analysis_out\\ETH_MZ_Mar22_Forecast_Ar\\images_debug", "-f", "ADMLV0"))

suppressWarnings(in_file <- normalizePath(argv$input))
suppressWarnings(out_dir <- normalizePath(argv$output))

variables <- argv$variables
factors <- argv$factors
group <- argv$group
groupHeader <- var_dic[name == group, factor]
isSameYScale <- argv$same_y_scale

if (!dir.exists(in_file) && !file.exists(in_file)) {
  stop(sprintf("%s does not exist.", in_file))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

print("Loading aggregation result.")
df <- data.table::fread(in_file)[,file := tools::file_path_sans_ext(basename(in_file))]

if ("year_month" %in% colnames(df)) {
  df <- df[order(rank(year_month))]
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
xLaxAngel <- 75

df[,year_month := factor(as.character(df[,year_month]), levels=unique(df[,year_month]))]

groupUnit <- ""
if (!is.na(group) && group == "SCENARIO") {
  cnt <- 0
  groupHeader2 <- ""
  for (subGroupName in var_dic[scenario!="", scenario]) {
    locations <- str_locate_all(unique(df[,get(groupHeader)]), subGroupName)
    if (length(unlist(locations)) > 0) {
      cnt <- cnt + 1
      groupHeader2 <- subGroupName
      groupUnit <- var_dic[scenario==groupHeader2,unit]
    }
  }
  if (cnt == 1) {
    df[, (groupHeader2) := str_replace(str_replace(df[,get(groupHeader)], paste0(groupHeader2, " "), ""), paste0(" ", groupUnit), "")]
    groupHeader <- groupHeader2
  }
}
if (!is.na(group)) {
  df[,(groupHeader) := factor(get(groupHeader), levels=unique(df[,get(groupHeader)]))]
  groupNum <- length(levels(df[,get(groupHeader)]))
} else {
  groupNum <- 1
}
plotDatas <- split(df, by=plotFactorHeaders, keep.by=FALSE, collapse="__")
plotKeys <- names(plotDatas)


cbPalette9 <-  c("#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7", "#000000", "#FFFFFF")
cbPalette10 <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933', '#CC6677', '#882255', '#AA4499', '#DDDDDD')
cbPalette11 <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")

for (variable in variables) {
  for (key in plotKeys) {
    print(paste0("Processing ", variable, " for ", key))
    plotData <- plotDatas[key][[1]]
    
    if (var_dic[name=="CR", factor] %in% colnames(plotData)) {
      crop <- crop_dic[DSSAT_code==plotData[,get(var_dic[name=="CR", factor])][1], Common_name]
    } else {
      crop <- "crop"
    }
    
    if (var_dic[average==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("monthly average ", var_dic[average==variable, boxplot], " forecast"), sep=", ")
      variableInFile <- paste0("monthly average ", var_dic[average==variable, boxplot])
      unitStr <- paste0(" (", var_dic[average==variable, unit], ")")
    } else if (var_dic[total==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("monthly total ", var_dic[total==variable, boxplot], " forecast"), sep=", ")
      variableInFile <- paste0("monthly total ", var_dic[total==variable, boxplot])
      unitStr <- paste0(" (", str_replace_all(var_dic[total==variable, unit], "/ha", ""), ")")
    } else if (var_dic[total_ton==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("monthly total ", var_dic[total_ton==variable, boxplot], " forecast"), sep=", ")
      variableInFile <- paste0("monthly total ", var_dic[total_ton==variable, boxplot])
      unitStr <- paste0(" (ton)")
    } else if (var_dic[name==toupper(variable), .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("monthly ",var_dic[name==toupper(variable), boxplot], " forecast"), sep=", ")
      variableInFile <- paste0("monthly ", var_dic[name==toupper(variable), boxplot])
      unitStr <- paste0(" (", var_dic[name==toupper(variable), unit], ")")
    } else {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("monthly ", variable), sep=", ")
      variableInFile <- paste0("monthly ", tolower(variable), " forecast")
      unitStr <- ""
    }
    if (groupUnit != "") {
      groupUnitStr <- paste0(" (", groupUnit, ")")
    } else {
      groupUnitStr <- ""
    }
    plotTitle <- str_wrap(plotTitle, 50)
    if (length(unlist(str_locate_all(plotTitle, "\n"))) == 0) {
      plotTitle <- paste0(plotTitle, "\n")
    }
    
    if (!is.na(group)) {
      plot <- ggplot(data = plotData, aes(x = year_month, y = get(variable), fill = get(groupHeader)))
    } else {
      plot <- ggplot(data = plotData, aes(x = year_month, y = get(variable), fill = file))
    }
    
    plot <- plot + geom_boxplot(
        # color = "darkgray",  
        outlier.size = 0.1,
        lwd = 0.1
      )  +
      stat_boxplot(geom ='errorbar',
                   # color = "darkgray",
                   size = 0.1)
    if (isSameYScale) {
      plot <- plot + coord_cartesian(ylim = range(df[,..variable])) +
        scale_y_continuous(breaks = seq(0, max(df[,..variable]), by = signif(max(df[,..variable])/10, 1)))
    } else {
      if (!F %in% (range(plotData[,..variable]) == 0)) {
        plot <- plot + coord_cartesian(ylim = range(c(0, 10))) +
          scale_y_continuous(breaks = seq(0, 10, by = 5))
      } else {
        plot <- plot + coord_cartesian(ylim = range(plotData[,..variable])) +
          scale_y_continuous(breaks = seq(0, max(plotData[,..variable]), by = signif(max(plotData[,..variable])/10, 1)))
      }
    }
    plot <- plot + 
      theme(legend.text = element_text(size = 13),
            legend.title = element_text(size = 13)) +
      theme(axis.title = element_text(size = 13, face = "bold")) +
      labs(x = "", y = paste0(variableInFile, unitStr), colour = "Legend", title = plotTitle) +
      theme(axis.text.x = element_text(angle = xLaxAngel, vjust = 0.25, hjust = 0.25)) +
      theme(axis.text.y = element_text(size = 6)) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
      theme(plot.title = element_text(size=18, face="bold", hjust = 0.5))
    
    if (groupNum <= 8) {
      plot <- plot + scale_fill_manual(values=cbPalette9, drop=F)
    } else if (groupNum <= 10) {
      plot <- plot + scale_fill_manual(values=cbPalette10, drop=F)
    } else if (groupNum <= 11) {
      plot <- plot + scale_fill_manual(values=cbPalette11, drop=F)
    }
    
    if (!is.na(group)) {
      plot <- plot + theme(legend.text = element_text(size=5)) +
        theme(legend.title = element_text(size=5, face="bold")) +
        guides(fill=guide_legend(title=str_wrap(paste0(groupHeader, groupUnitStr), 15)))
    } else {
      plot <- plot + theme(legend.position="none")
    }
    
    file_name <- paste0("forecast_", str_replace_all(variableInFile, " ", "_"), "-", str_replace_all(key, "\\.", "__"), ".", extension)
    if (!is.na(group)) {
      ggsave(
        plot,
        filename = file_name,
        path = out_dir,
        width=8, height=4, dpi=300
      )
    } else {
      ggsave(
        plot,
        filename = file_name,
        path = out_dir
      )
    }
  }
}

print("Complete.")
