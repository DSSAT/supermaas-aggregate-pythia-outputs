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

p <- argparser::arg_parser("Generate statistics boxplot based on merged aggregation results from Pythia outputs for World Modelers(fixed)")
p <- argparser::add_argument(p, "input", "Aggregation result file, includes all the scenarios")
p <- argparser::add_argument(p, "output", "folder Path to generaete box plot graphs")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable HEADER names for comparison, if not given, then comparing all non-factor columns"))
p <- argparser::add_argument(p, "--factors", short="-f", nargs=Inf, help=paste0("Factor names for grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor != "" & name != "SCENARIO", factor]), collapse=","), "]"))
p <- argparser::add_argument(p, "--group", short="-g", nargs=1, help=paste0("Group name for sub-grouping the comparison result: if not given, then any header in the following list will be considered as factor [", paste(unique(var_dic[factor != "" & name != "SCENARIO", factor]), collapse=","), "]"))
p <- argparser::add_argument(p, "--x_var", short = "-x", nargs = 1, default="SCENARIO", help = paste("Variable used for x-axit in plotting graph"))
# p <- argparser::add_argument(p, "--max_bar_num", short="-n", default = 25, help = "Maximum number of box bar per graph")

argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\stage_8_admlv0.csv", "test\\data\\case21\\analysis_out\\ETH_MZ_2022_N\\images2", "-f", "ADMLV0", "-g", "SEASON"))

suppressWarnings(in_dir <- normalizePath(argv$input))
suppressWarnings(out_dir <- normalizePath(argv$output))

variables <- argv$variables
factors <- argv$factors
group <- argv$group
groupHeader <- var_dic[name == group, factor]

# maxBarNum <- argv$max_bar_num
maxBarNum <- 25
plotXVar <- argv$x_var
plotXVarHeader <- var_dic[name == plotXVar, factor]
plotXVarHeaderOrdered <- paste0(plotXVarHeader, "_ordered")

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Process baseline data and calculate threshold
print("Loading files for statistic plotting")
df <- data.table::fread(in_dir)

suppressWarnings(if (is.na(factors)) {
  headers <- colnames(df)
  plotFactorHeaders <- headers[headers %in% var_dic[factor != "" & name != plotXVar, factor]]
} else {
  if (T %in% (paste0("ADMLV", 0:5) %in% factors)) {
    factors <- unique(c(paste0("ADMLV", 0:c(5:0)[match(T,paste0("ADMLV", 5:0) %in% factors)]), factors))
  }
  plotFactorHeaders <- var_dic[name %in% factors, factor]
})

suppressWarnings(if (is.na(variables)) {
  headers <- colnames(df)
  variables <- headers[!headers %in% var_dic[factor != "", factor]]
  variables <- variables[!variables %in% plotFactorHeaders]
})

# if (plotXVar == "SCENARIO" && length(unlist(str_locate_all(df[,scenario], "__"))) == 0) {
#   locations <- str_locate_all(df[,scenario], "[-+]?\\d+")
#   suppressWarnings(start <- unlist(lapply(locations, min)))
#   suppressWarnings(end <- unlist(lapply(locations, max)))
#   df[,scenario_offset := as.numeric(str_sub(df[,scenario], start, end))]
#   plotXVarHeader <- "scenario_offset"
#   plotXVar <- var_dic[name==plotXVar, scenario]
# }

print("Generating boxplot graphs...")
extension <- "png"
if (plotXVar == "SCENARIO") {
  xLaxAngel <- 345
} else {
  xLaxAngel <- 90
}

rows <- unique(df[,c(..plotXVarHeader)])
rows[,factor_id:=1:rows[,.N]]
df <- merge(df, rows, by=c(plotXVarHeader), all=T, sort=F)
factorNum <- rows[,.N]

if (class(df[,get(plotXVarHeader)]) != "character") {
  df[,(plotXVarHeader):=as.character(get(plotXVarHeader))]
}

df[,(plotXVarHeaderOrdered) := factor(get(plotXVarHeader), levels=unique(df[,get(plotXVarHeader)]))]
df[,(groupHeader) := factor(get(groupHeader), levels=unique(df[,get(groupHeader)]))]

plotDatas <- split(df, by=plotFactorHeaders, keep.by=FALSE, collapse="__")
plotKeys <- names(plotDatas)

groupNum <- length(levels(df[,get(groupHeader)]))
cbPalette9 <-  c("#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7", "#000000", "#FFFFFF")
cbPalette10 <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933', '#CC6677', '#882255', '#AA4499', '#DDDDDD')
cbPalette11 <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")

for (variable in variables) {
  for (key in plotKeys) {
    print(paste0("Processing ", variable, " for ", key))
    plotData <- plotDatas[key][[1]]
    
    # Title rule: factors list, crop name, variable name (e.g. average yield)
    if (var_dic[name=="CR", factor] %in% colnames(plotData)) {
      crop <- crop_dic[DSSAT_code==plotData[,get(var_dic[name=="CR", factor])][1], Common_name]
    } else {
      crop <- "crop"
    }
    
    if (var_dic[average==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("average ", var_dic[average==variable, boxplot], " (", var_dic[average==variable, unit], ")"), sep=", ")
      variableInFile <- paste0("average ", var_dic[average==variable, boxplot])
    } else if (var_dic[total==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("total ", var_dic[total==variable, boxplot], " (", str_replace_all(var_dic[total==variable, unit], "/ha", ""), ")"), sep=", ")
      variableInFile <- paste0("total ", var_dic[total==variable, boxplot])
    } else if (var_dic[total_ton==variable, .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0("total ", var_dic[total_ton==variable, boxplot], " (ton)"), sep=", ")
      variableInFile <- paste0("total ", var_dic[total_ton==variable, boxplot])
    } else if (var_dic[name==toupper(variable), .N] > 0) {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, paste0(var_dic[name==toupper(variable), boxplot], " (", var_dic[name==toupper(variable), unit], ")"), sep=", ")
      variableInFile <- var_dic[name==toupper(variable), boxplot]
    } else {
      plotTitle <- paste(str_replace_all(key, "\\.", "_"), crop, variable, sep=", ")
      variableInFile <- tolower(variable)
    }
    
    plotTitle <- str_wrap(plotTitle, 40)
    
    for (i in 1:ceiling(factorNum/maxBarNum)) {
      
      plotSubData <- plotData[factor_id %in% (1 + (i-1) * maxBarNum) : (i*maxBarNum)]
      if (!is.na(group)) {
        plot <- ggplot(data = plotSubData, aes(x = get(plotXVarHeaderOrdered), y = get(variable), fill = get(groupHeader)))
      } else {
        plot <- ggplot(data = plotSubData, aes(x = get(plotXVarHeaderOrdered), y = get(variable)))
      }
      
      plot <- plot +geom_boxplot(
          outlier.colour = NA,
          color = "darkgrey",
          outlier.size = 0.2,
          lwd = 0.2
        )  +
        stat_boxplot(geom ='errorbar',
                     size = 0.2)+
        coord_cartesian(ylim = range(df[,..variable])) +
        # theme_light() +
        theme(legend.text = element_text(size = 13),
              legend.title = element_text(size = 13)) +
        # theme(axis.text = element_text(size = 13)) +
        theme(axis.title = element_text(size = 13, face = "bold")) +
        labs(x = plotXVarHeader, y = variableInFile, colour = "Legend", title = plotTitle) +
        theme(axis.text.x = element_text(angle = xLaxAngel, vjust = 0.5, hjust = 0)) +
        theme(panel.grid.minor = element_blank()) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
        theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
      
      if (groupNum <= 8) {
        plot <- plot + scale_fill_manual(values=cbPalette9, drop=F)
      } else if (groupNum <= 10) {
        plot <- plot + scale_fill_manual(values=cbPalette10, drop=F)
      } else if (groupNum <= 11) {
        plot <- plot + scale_fill_manual(values=cbPalette11, drop=F)
      }
      
      if (!is.na(group)) {
        plot <- plot + theme(legend.text = element_text(size=8)) +
          theme(legend.title = element_text(size=9, face="bold")) +
          guides(fill=guide_legend(title=group))
      }
      
      if (ceiling(factorNum/maxBarNum) == 1) {
        file_name <- paste0(str_replace_all(variableInFile, " ", "_"), "-", str_replace_all(key, "\\.", "__"), ".", extension)
      } else {
        file_name <- paste0(str_replace_all(variableInFile, " ", "_"), "-", str_replace_all(key, "\\.", "__"), "_", i, ".", extension)
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