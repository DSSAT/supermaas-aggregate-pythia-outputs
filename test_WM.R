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

setAdminInfo <- function(final) {
  for (i in 5:0) {
    if (paste0("ADMLV", i) %in% colnames(final)) {
      if (i == 5) {
        final[, admlv_info := paste(ADMLV0, ADMLV1, ADMLV2, ADMLV3, ADMLV4, ADMLV5, sep = "_")]
      } else if (i == 4) {
        final[, admlv_info := paste(ADMLV0, ADMLV1, ADMLV2, ADMLV3, ADMLV4, sep = "_")]
      } else if (i == 3) {
        final[, admlv_info := paste(ADMLV0, ADMLV1, ADMLV2, ADMLV3, sep = "_")]
      } else if (i == 2) {
        final[, admlv_info := paste(ADMLV0, ADMLV1, ADMLV2, sep = "_")]
      } else if (i == 1) {
        final[, admlv_info := paste(ADMLV0, ADMLV1, sep = "_")]
      } else {
        final[, admlv_info := ADMLV0]
      }
      setcolorder(final, c("admlv_info", colnames(final)[-length(colnames(final))]))
      break
    }
  }
}

finalize <- function(stage, factors) {
  final <- stage[, .(production = round(production, 2),
                     crop_per_person = round(crop_per_person, 1),
                     crop_per_drop = round(crop_per_drop, 2),
                     crop_failure_area = round(crop_failure_area, 2),
                     total_water = total_water * 10,
                     HAREA_TOT = HAREA_TOT,
                     population = population,
                     HWAM_AVE = HWAM_AVE,
                     TOTAL_NITROGEN_APPLIED = round(TOTAL_NITROGEN_APPLIED, 2)
  ), by = factors]
  setAdminInfo(final)
  for (factorVar in factors) {
    setnames(final, factorVar, var_dic[name == factorVar, factor], skip_absent = T)
  }
  return(final)
}

finalizeMonthly <- function(stage, factors) {
  final <- unique(stage[,mget(factors[!factors %in% "HMONTH"])])
  groupSize <- nrow(final)
  final <- final[rep(seq_len(groupSize), 12), ]
  monthsList <- c()
  for (i in 1:12) {
    monthsList <- c(monthsList, rep(i, groupSize))
  }
  final[, HMONTH := monthsList]
  setcolorder(final, factors)
  final <- merge(final, stage, by = factors, sort = F, all.x = T)
  final[is.na(production), production := 0]
  final[, production := round(production, 2)]
  setAdminInfo(final)
  for (factorVar in factors) {
    setnames(final, factorVar, var_dic[name == factorVar, factor], skip_absent = T)
  }
  return(final)
}

diffCheck <- function(stage_final, stage_org, factors, reportFile) {
  # stage_final <- stage3_final
  # stage_org <- stage3_org
  # factors <- factors3
  stage_check <- stage_org[,mget(var_dic[name %in% factors, factor])]
  isSame <- T
  isSimilar <- T
  for (col in colnames(stage_org)) {
    if (!col %in% var_dic[name %in% factors, factor]) {
      
      stage_check[, diff := (stage_final[,get(col)] - stage_org[,get(col)])]
      stage_check[, org := stage_org[,get(col)]]
      stage_check[, (col) := diff/org * 100]
      stage_check[org==0&diff==0, (col):=0]
      if (stage_check[get(col)>0, .N] > 0) {
        maxDiff <- round(stage_check[,max(abs(get(col)))], 2)
        if (maxDiff  == 0) {
          # cat("similar\n")
          isSame <- F
        } else {
          cat(paste0("\n  Found ", col, "...\t"))
          cat(paste0(" max diff=", maxDiff, "%"))
          isSame <- F
          isSimilar <- F
        }
        
      } else {
        # cat("same\n")
        stage_check[,(col):=NULL]
      }
    }
  }
  stage_check[,diff:=NULL][,org:=NULL]
  if (isSame) {
    cat("same\n")
  } else if (isSimilar) {
    cat("similar\n")
  } else {
    data.table::fwrite(stage_check, file = reportFile)
    cat(paste0("\n  Generat report to ", reportFile),"\n")
  }
}

p <- argparser::arg_parser("Unit test for WM result (stage 2 ~ 15)")
p <- argparser::add_argument(p, "input", "Aggregation script output directory")
p <- argparser::add_argument(p, "--crop_failure_threshold", short="-c", default = 100, nargs=Inf, help="Threshold to determine if the crop is failure, (kg/ha)")
# p <- argparser::add_argument(p, "output", "final output of the aggregated files")

argv <- argparser::parse_args(p)
# argv <- argparser::parse_args(p, c("test\\data\\case19\\test2\\baseline", "-c", "200"))

# "-v", "PRODUCTION", "CROP_PER_PERSON", "CROP_PER_DROP", "CROP_FAILURE_AREA", "HUNGRY_PEOPLE", "-t", "HARVEST_AREA", "-o", "NICM", "-a", "HWAM"

suppressWarnings(in_dir <- normalizePath(argv$input))
cfThreshold <- argv$crop_failure_threshold

if (!dir.exists(in_dir) && !file.exists(in_dir)) {
  stop(sprintf("%s does not exist.", in_dir))
}

pythiaOutDir <- file.path(in_dir, "pythia_out")
analysisOutDir <- file.path(in_dir, "analysis_out")
analysisPlotOutDir <- file.path(analysisOutDir, "images")
testOutDir <- file.path(in_dir, "test_out")

if (dir.exists(testOutDir)) {
  print("Clean previous test result.")
  unlink(testOutDir, recursive = TRUE)
}
dir.create(testOutDir, recursive = TRUE)

flist <- list()
dts <- list()
print("Loading files for aggregation.")
if (!dir.exists(pythiaOutDir)) {
  flist <- pythiaOutDir
} else {
  flist <- list.files(path = pythiaOutDir, pattern = "*.csv", recursive = FALSE, full.names = TRUE)
}
for(f in flist) {
  dts <- c(dts, list(data.table::fread(f)[,FILE := tools::file_path_sans_ext(basename(f))]))
}
df <- data.table::rbindlist(dts)
stage1a <- df[!is.na(as.Date(paste0(HDAT), "%Y%j"))]
stage1a[HWAH < 0, HWAH := 0]
colNames <- colnames(stage1a)
yearFactor <- "PYEAR"
if ("SEASON" %in% colNames) {
  factors2 <- c("LATITUDE", "LONGITUDE", "RUN_NAME", yearFactor, "CR", "SEASON")
  factors3 <- c("LATITUDE", "LONGITUDE", yearFactor, "CR", "SEASON")
  factors4 <- c("LATITUDE", "LONGITUDE", "RUN_NAME", "CR", "SEASON")
  factors5a <- c("LATITUDE", "LONGITUDE", yearFactor, "CR")
  factors5 <- c("LATITUDE", "LONGITUDE", "CR")
  factors70 <- c("ADMLV0", "RUN_NAME", yearFactor, "CR", "SEASON")
  factors71 <- c("ADMLV0", "ADMLV1", "RUN_NAME", yearFactor, "CR", "SEASON")
  factors72 <- c("ADMLV0", "ADMLV1", "ADMLV2", "RUN_NAME", yearFactor, "CR", "SEASON")
  factors80 <- c("ADMLV0", yearFactor, "CR", "SEASON")
  factors81 <- c("ADMLV0", "ADMLV1", yearFactor, "CR", "SEASON")
  factors82 <- c("ADMLV0", "ADMLV1", "ADMLV2", yearFactor, "CR", "SEASON")
  factors90 <- c("ADMLV0", "RUN_NAME", "CR", "SEASON")
  factors91 <- c("ADMLV0", "ADMLV1", "RUN_NAME", "CR", "SEASON")
  factors92 <- c("ADMLV0", "ADMLV1", "ADMLV2", "RUN_NAME", "CR", "SEASON")
  factors100a <- c("ADMLV0", yearFactor, "CR")
  factors101a <- c("ADMLV0", yearFactor, "ADMLV1", "CR")
  factors102a <- c("ADMLV0", yearFactor, "ADMLV1", "ADMLV2", "CR")
  factors100 <- c("ADMLV0", "CR")
  factors101 <- c("ADMLV0", "ADMLV1", "CR")
  factors102 <- c("ADMLV0", "ADMLV1", "ADMLV2", "CR")
  populationVars <- c("LATITUDE", "LONGITUDE", "ADMLV0", "ADMLV1", "ADMLV2", yearFactor, "POPULATION", "CR", "SEASON")
} else {
  factors2 <- c("LATITUDE", "LONGITUDE", "RUN_NAME", yearFactor, "CR")
  factors3 <- c("LATITUDE", "LONGITUDE", yearFactor, "CR")
  factors4 <- c("LATITUDE", "LONGITUDE", "RUN_NAME", "CR")
  factors5a <- c("LATITUDE", "LONGITUDE", yearFactor, "CR")
  factors5 <- c("LATITUDE", "LONGITUDE", "CR")
  factors70 <- c("ADMLV0", "RUN_NAME", yearFactor, "CR")
  factors71 <- c("ADMLV0", "ADMLV1", "RUN_NAME", yearFactor, "CR")
  factors72 <- c("ADMLV0", "ADMLV1", "ADMLV2", "RUN_NAME", yearFactor, "CR")
  factors80 <- c("ADMLV0", yearFactor, "CR")
  factors81 <- c("ADMLV0", "ADMLV1", yearFactor, "CR")
  factors82 <- c("ADMLV0", "ADMLV1", "ADMLV2", yearFactor, "CR")
  factors90 <- c("ADMLV0", "RUN_NAME", "CR")
  factors91 <- c("ADMLV0", "ADMLV1", "RUN_NAME", "CR")
  factors92 <- c("ADMLV0", "ADMLV1", "ADMLV2", "RUN_NAME", "CR")
  factors100a <- c("ADMLV0", yearFactor, "CR")
  factors101a <- c("ADMLV0", yearFactor, "ADMLV1", "CR")
  factors102a <- c("ADMLV0", yearFactor, "ADMLV1", "ADMLV2", "CR")
  factors100 <- c("ADMLV0", "CR")
  factors101 <- c("ADMLV0", "ADMLV1", "CR")
  factors102 <- c("ADMLV0", "ADMLV1", "ADMLV2", "CR")
  populationVars <- c("LATITUDE", "LONGITUDE", "ADMLV0", "ADMLV1", "ADMLV2", yearFactor, "POPULATION", "CR")
}
factors13 <- c("LATITUDE", "LONGITUDE", "HMONTH", yearFactor, "CR")
factors140 <- c("ADMLV0", "HMONTH", yearFactor, "CR")
factors141 <- c("ADMLV0", "ADMLV1", "HMONTH", yearFactor, "CR")
factors142 <- c("ADMLV0", "ADMLV1", "ADMLV2", "HMONTH", yearFactor, "CR")

# stage 2
cat("checking stage2...")
stage1a[,crop_failure_area := 0]
stage1a[HWAH<cfThreshold, crop_failure_area := HARVEST_AREA]
stage2 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000,
                      crop_per_person = 0,
                      total_water = sum((PRCP + IRCM) * HARVEST_AREA),
                      crop_per_drop = sum(HWAH * HARVEST_AREA)/sum((PRCP + IRCM) * HARVEST_AREA),
                      crop_failure_area = sum(crop_failure_area),
                      HAREA_TOT = sum(HARVEST_AREA),
                      population = 0,
                      HWAM_AVE = sum(HWAM * HARVEST_AREA)/sum(HARVEST_AREA),
                      TOTAL_NITROGEN_APPLIED = sum(NICM * HARVEST_AREA)/1000
                      ), by = factors2]
stage2[,population := unique(stage1a[,mget(c(unique(c(populationVars, factors2))))])[,sum(POPULATION),by=factors2][,V1]]
stage2[,crop_per_person := production/population*1000]
stage2[is.na(crop_per_drop),crop_per_drop:=0]

stage2_final <- finalize(stage2, factors2)
data.table::fwrite(stage2_final, file = file.path(testOutDir, "stage_2.csv"))

stage2_org <- data.table::fread(file.path(analysisOutDir, "stage_2.csv"))
diffCheck(stage2_final, stage2_org, factors2, file.path(testOutDir, "stage_2_diff.csv"))


# stage 3
cat("checking stage3...")
stage3 <- stage2[, .(production = sum(production),
                     crop_per_person = sum(production)/mean(population)*1000,
                     total_water = sum(total_water),
                     crop_per_drop = sum(production)/sum(total_water)*1000,
                     crop_failure_area = sum(crop_failure_area),
                     HAREA_TOT = sum(HAREA_TOT),
                     population = mean(population),
                     HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                     TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
                     ), by = factors3]
stage3[is.na(crop_per_drop),crop_per_drop:=0]

stage3_final <- finalize(stage3, factors3)
data.table::fwrite(stage3_final, file = file.path(testOutDir, "stage_3.csv"))

stage3_org <- data.table::fread(file.path(analysisOutDir, "stage_3.csv"))
diffCheck(stage3_final, stage3_org, factors3, file.path(testOutDir, "stage_3_diff.csv"))


# stage 4
cat("checking stage4...")
stage4 <- stage2[, .(production = mean(production),
                     crop_per_person = mean(crop_per_person),
                     total_water = mean(total_water),
                     crop_per_drop = mean(crop_per_drop),
                     crop_failure_area = mean(crop_failure_area),
                     HAREA_TOT = mean(HAREA_TOT),
                     population = mean(population),
                     HWAM_AVE = mean(HWAM_AVE),
                     TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors4]

stage4_final <- finalize(stage4, factors4)
data.table::fwrite(stage4_final, file = file.path(testOutDir, "stage_4.csv"))

stage4_org <- data.table::fread(file.path(analysisOutDir, "stage_4.csv"))
diffCheck(stage4_final, stage4_org, factors4, file.path(testOutDir, "stage_4_diff.csv"))


# stage 5
cat("checking stage5...")
stage5 <- stage3[, .(production = sum(production),
                     crop_per_person = sum(production)/mean(population)*1000,
                     total_water = sum(total_water),
                     crop_per_drop = sum(production)/sum(total_water)*1000,
                     crop_failure_area = sum(crop_failure_area),
                     HAREA_TOT = sum(HAREA_TOT),
                     population = mean(population),
                     HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                     TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors5a]
stage5[is.na(crop_per_drop),crop_per_drop:=0]
stage5 <- stage5[, .(production = mean(production),
                     crop_per_person = mean(crop_per_person),
                     total_water = mean(total_water),
                     crop_per_drop = mean(crop_per_drop),
                     crop_failure_area = mean(crop_failure_area),
                     HAREA_TOT = mean(HAREA_TOT),
                     population = mean(population),
                     HWAM_AVE = mean(HWAM_AVE),
                     TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors5]

stage5_final <- finalize(stage5, factors5)
data.table::fwrite(stage5_final, file = file.path(testOutDir, "stage_5.csv"))

stage5_org <- data.table::fread(file.path(analysisOutDir, "stage_5.csv"))
diffCheck(stage5_final, stage5_org, factors5, file.path(testOutDir, "stage_5_diff.csv"))


# stage 6
if (file.exists(file.path(analysisOutDir, "stage_6.csv"))) {
  cat("checking stage6...")
} else {
  cat("checking stage6...skipped")
}


# stage 7 admin 0
cat("checking stage7 admin 0 ...")
stage70 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000,
                      crop_per_person = sum(HWAH * HARVEST_AREA)/sum(POPULATION),
                      total_water = sum((PRCP + IRCM) * HARVEST_AREA),
                      crop_per_drop = sum(HWAH * HARVEST_AREA)/sum((PRCP + IRCM) * HARVEST_AREA),
                      crop_failure_area = sum(crop_failure_area),
                      HAREA_TOT = sum(HARVEST_AREA),
                      population = sum(POPULATION),
                      HWAM_AVE = sum(HWAM * HARVEST_AREA)/sum(HARVEST_AREA),
                      TOTAL_NITROGEN_APPLIED = sum(NICM * HARVEST_AREA)/1000
), by = factors70]
stage70[is.na(crop_per_drop),crop_per_drop:=0]

stage70_final <- finalize(stage70, factors70)
data.table::fwrite(stage70_final, file = file.path(testOutDir, "stage_7_admlv0.csv"))

stage70_org <- data.table::fread(file.path(analysisOutDir, "stage_7_admlv0.csv"))
diffCheck(stage70_final, stage70_org, c("ADMLV_INFO", factors70), file.path(testOutDir, "stage_7_admlv0_diff.csv"))


# stage 7 admin 1
cat("checking stage7 admin 1 ...")
stage71 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000,
                       crop_per_person = sum(HWAH * HARVEST_AREA)/sum(POPULATION),
                       total_water = sum((PRCP + IRCM) * HARVEST_AREA),
                       crop_per_drop = sum(HWAH * HARVEST_AREA)/sum((PRCP + IRCM) * HARVEST_AREA),
                       crop_failure_area = sum(crop_failure_area),
                       HAREA_TOT = sum(HARVEST_AREA),
                       population = sum(POPULATION),
                       HWAM_AVE = sum(HWAM * HARVEST_AREA)/sum(HARVEST_AREA),
                       TOTAL_NITROGEN_APPLIED = sum(NICM * HARVEST_AREA)/1000
), by = factors71]
stage71[is.na(crop_per_drop),crop_per_drop:=0]

stage71_final <- finalize(stage71, factors71)
data.table::fwrite(stage71_final, file = file.path(testOutDir, "stage_7_admlv1.csv"))

stage71_org <- data.table::fread(file.path(analysisOutDir, "stage_7_admlv1.csv"))
diffCheck(stage71_final, stage71_org, c("ADMLV_INFO", factors71), file.path(testOutDir, "stage_7_admlv1_diff.csv"))


# stage 7 admin 2
cat("checking stage7 admin 2 ...")
stage72 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000,
                       crop_per_person = sum(HWAH * HARVEST_AREA)/sum(POPULATION),
                       total_water = sum((PRCP + IRCM) * HARVEST_AREA),
                       crop_per_drop = sum(HWAH * HARVEST_AREA)/sum((PRCP + IRCM) * HARVEST_AREA),
                       crop_failure_area = sum(crop_failure_area),
                       HAREA_TOT = sum(HARVEST_AREA),
                       population = sum(POPULATION),
                       HWAM_AVE = sum(HWAM * HARVEST_AREA)/sum(HARVEST_AREA),
                       TOTAL_NITROGEN_APPLIED = sum(NICM * HARVEST_AREA)/1000
), by = factors72]
stage72[is.na(crop_per_drop),crop_per_drop:=0]

stage72_final <- finalize(stage72, factors72)
data.table::fwrite(stage72_final, file = file.path(testOutDir, "stage_7_admlv2.csv"))

stage72_org <- data.table::fread(file.path(analysisOutDir, "stage_7_admlv2.csv"))
diffCheck(stage72_final, stage72_org, c("ADMLV_INFO", factors72), file.path(testOutDir, "stage_7_admlv2_diff.csv"))


# stage 8 admin 0
cat("checking stage8 admin 0 ...")
stage80 <- stage70[, .(production = sum(production),
                       crop_per_person = sum(production)/unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors80][,mean(V1)]*1000,
                       total_water = sum(total_water),
                       crop_per_drop = sum(production)/sum(total_water)*1000,
                       crop_failure_area = sum(crop_failure_area),
                       HAREA_TOT = sum(HAREA_TOT),
                       population = unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors80][,mean(V1)],
                       HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                       TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors80]
stage80[is.na(crop_per_drop),crop_per_drop:=0]

stage80_final <- finalize(stage80, factors80)
data.table::fwrite(stage80_final, file = file.path(testOutDir, "stage_8_admlv0.csv"))

stage80_org <- data.table::fread(file.path(analysisOutDir, "stage_8_admlv0.csv"))
diffCheck(stage80_final, stage80_org, c("ADMLV_INFO", factors80), file.path(testOutDir, "stage_8_admlv0_diff.csv"))


# stage 8 admin 1
cat("checking stage8 admin 1 ...")
stage81 <- stage71[, .(production = sum(production),
                       crop_per_person = 0,
                       total_water = sum(total_water),
                       crop_per_drop = sum(production)/sum(total_water)*1000,
                       crop_failure_area = sum(crop_failure_area),
                       HAREA_TOT = sum(HAREA_TOT),
                       population = 0,
                       HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                       TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors81]
stage81[,population := unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors81][,V1]]
stage81[,crop_per_person := production/population*1000]
stage81[is.na(crop_per_drop),crop_per_drop:=0]

stage81_final <- finalize(stage81, factors81)
data.table::fwrite(stage81_final, file = file.path(testOutDir, "stage_8_admlv1.csv"))

stage81_org <- data.table::fread(file.path(analysisOutDir, "stage_8_admlv1.csv"))
diffCheck(stage81_final, stage81_org, c("ADMLV_INFO", factors81), file.path(testOutDir, "stage_8_admlv1_diff.csv"))


# stage 8 admin 2
cat("checking stage8 admin 2 ...")
stage82 <- stage72[, .(production = sum(production),
                       crop_per_person = 0,
                       total_water = sum(total_water),
                       crop_per_drop = sum(production)/sum(total_water)*1000,
                       crop_failure_area = sum(crop_failure_area),
                       HAREA_TOT = sum(HAREA_TOT),
                       population = 0,
                       HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                       TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors82]
stage82[,population := unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors82][,V1]]
stage82[,crop_per_person := production/population*1000]
stage82[is.na(crop_per_drop),crop_per_drop:=0]

stage82_final <- finalize(stage82, factors82)
data.table::fwrite(stage82_final, file = file.path(testOutDir, "stage_8_admlv2.csv"))

stage82_org <- data.table::fread(file.path(analysisOutDir, "stage_8_admlv2.csv"))
diffCheck(stage82_final, stage82_org, c("ADMLV_INFO", factors82), file.path(testOutDir, "stage_8_admlv2_diff.csv"))


# stage 9 admin 0
cat("checking stage9 admin 0 ...")
stage90 <- stage70[, .(production = mean(production),
                       crop_per_person = mean(crop_per_person),
                       total_water = mean(total_water),
                       crop_per_drop = mean(crop_per_drop),
                       crop_failure_area = mean(crop_failure_area),
                       HAREA_TOT = mean(HAREA_TOT),
                       population = mean(population),
                       HWAM_AVE = mean(HWAM_AVE),
                       TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors90]

stage90_final <- finalize(stage90, factors90)
data.table::fwrite(stage90_final, file = file.path(testOutDir, "stage_9_admlv0.csv"))

stage90_org <- data.table::fread(file.path(analysisOutDir, "stage_9_admlv0.csv"))
diffCheck(stage90_final, stage90_org, c("ADMLV_INFO", factors90), file.path(testOutDir, "stage_9_admlv0_diff.csv"))


# stage 9 admin 1
cat("checking stage9 admin 1 ...")
stage91 <- stage71[, .(production = mean(production),
                       crop_per_person = mean(crop_per_person),
                       total_water = mean(total_water),
                       crop_per_drop = mean(crop_per_drop),
                       crop_failure_area = mean(crop_failure_area),
                       HAREA_TOT = mean(HAREA_TOT),
                       population = mean(population),
                       HWAM_AVE = mean(HWAM_AVE),
                       TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors91]

stage91_final <- finalize(stage91, factors91)
data.table::fwrite(stage91_final, file = file.path(testOutDir, "stage_9_admlv1.csv"))

stage91_org <- data.table::fread(file.path(analysisOutDir, "stage_9_admlv1.csv"))
diffCheck(stage91_final, stage91_org, c("ADMLV_INFO", factors91), file.path(testOutDir, "stage_9_admlv1_diff.csv"))


# stage 9 admin 2
cat("checking stage9 admin 2 ...")
stage92 <- stage72[, .(production = mean(production),
                       crop_per_person = mean(crop_per_person),
                       total_water = mean(total_water),
                       crop_per_drop = mean(crop_per_drop),
                       crop_failure_area = mean(crop_failure_area),
                       HAREA_TOT = mean(HAREA_TOT),
                       population = mean(population),
                       HWAM_AVE = mean(HWAM_AVE),
                       TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors92]

stage92_final <- finalize(stage92, factors92)
data.table::fwrite(stage92_final, file = file.path(testOutDir, "stage_9_admlv2.csv"))

stage92_org <- data.table::fread(file.path(analysisOutDir, "stage_9_admlv2.csv"))
diffCheck(stage92_final, stage92_org, c("ADMLV_INFO", factors92), file.path(testOutDir, "stage_9_admlv2_diff.csv"))


# stage 10 admin 0
cat("checking stage10 admin 0 ...")
stage100 <- stage80[, .(production = sum(production),
                        crop_per_person = sum(production)/unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors100a][,mean(V1)]*1000,
                        total_water = sum(total_water),
                        crop_per_drop = sum(production)/sum(total_water)*1000,
                        crop_failure_area = sum(crop_failure_area),
                        HAREA_TOT = sum(HAREA_TOT),
                        population = unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors100a][,mean(V1)],
                        HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                        TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors100a]
stage100[is.na(crop_per_drop),crop_per_drop:=0]
stage100 <- stage100[, .(production = mean(production),
                        crop_per_person = mean(crop_per_person),
                        total_water = mean(total_water),
                        crop_per_drop = mean(crop_per_drop),
                        crop_failure_area = mean(crop_failure_area),
                        HAREA_TOT = mean(HAREA_TOT),
                        population = mean(population),
                        HWAM_AVE = mean(HWAM_AVE),
                        TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors100]


stage100_final <- finalize(stage100, factors100)
data.table::fwrite(stage100_final, file = file.path(testOutDir, "stage_10_admlv0.csv"))

stage100_org <- data.table::fread(file.path(analysisOutDir, "stage_10_admlv0.csv"))
diffCheck(stage100_final, stage100_org, c("ADMLV_INFO", factors100), file.path(testOutDir, "stage_10_admlv0_diff.csv"))


# stage 10 admin 1
cat("checking stage10 admin 1 ...")
stage101 <- stage81[, .(production = sum(production),
                        crop_per_person = 0,
                        total_water = sum(total_water),
                        crop_per_drop = sum(production)/sum(total_water)*1000,
                        crop_failure_area = sum(crop_failure_area),
                        HAREA_TOT = sum(HAREA_TOT),
                        population = 0,
                        HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                        TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors101a]
stage101[,population := unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors101a][,V1]]
stage101[,crop_per_person := production/population*1000]
stage101[is.na(crop_per_drop),crop_per_drop:=0]
stage101 <- stage101[, .(production = mean(production),
                        crop_per_person = mean(crop_per_person),
                        total_water = mean(total_water),
                        crop_per_drop = mean(crop_per_drop),
                        crop_failure_area = mean(crop_failure_area),
                        HAREA_TOT = mean(HAREA_TOT),
                        population = mean(population),
                        HWAM_AVE = mean(HWAM_AVE),
                        TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors101]

stage101_final <- finalize(stage101, factors101)
data.table::fwrite(stage101_final, file = file.path(testOutDir, "stage_10_admlv1.csv"))

stage101_org <- data.table::fread(file.path(analysisOutDir, "stage_10_admlv1.csv"))
diffCheck(stage101_final, stage101_org, c("ADMLV_INFO", factors101), file.path(testOutDir, "stage_10_admlv1_diff.csv"))


# stage 10 admin 2
cat("checking stage10 admin 2 ...")
stage102 <- stage82[, .(production = sum(production),
                        crop_per_person = 0,
                        total_water = sum(total_water),
                        crop_per_drop = sum(production)/sum(total_water)*1000,
                        crop_failure_area = sum(crop_failure_area),
                        HAREA_TOT = sum(HAREA_TOT),
                        population = 0,
                        HWAM_AVE = sum(HWAM_AVE * HAREA_TOT)/sum(HAREA_TOT),
                        TOTAL_NITROGEN_APPLIED = sum(TOTAL_NITROGEN_APPLIED)
), by = factors102a]
stage102[,population := unique(stage1a[,mget(populationVars)])[,sum(POPULATION),by=factors102a][,V1]]
stage102[,crop_per_person := production/population*1000]
stage102[is.na(crop_per_drop),crop_per_drop:=0]
stage102 <- stage102[, .(production = mean(production),
                        crop_per_person = mean(crop_per_person),
                        total_water = mean(total_water),
                        crop_per_drop = mean(crop_per_drop),
                        crop_failure_area = mean(crop_failure_area),
                        HAREA_TOT = mean(HAREA_TOT),
                        population = mean(population),
                        HWAM_AVE = mean(HWAM_AVE),
                        TOTAL_NITROGEN_APPLIED = mean(TOTAL_NITROGEN_APPLIED)
), by = factors102]

stage102_final <- finalize(stage102, factors102)
data.table::fwrite(stage102_final, file = file.path(testOutDir, "stage_10_admlv2.csv"))

stage102_org <- data.table::fread(file.path(analysisOutDir, "stage_10_admlv2.csv"))
diffCheck(stage102_final, stage102_org, c("ADMLV_INFO", factors102), file.path(testOutDir, "stage_10_admlv2_diff.csv"))


# stage 11 admin 0
if (file.exists(file.path(analysisOutDir, "stage_11_admlv0.csv"))) {
  cat("checking stage11 admin0 ...ignored\n")
} else {
  cat("checking stage11 admin0 ...skipped\n")
}


# stage 11 admin 1
if (file.exists(file.path(analysisOutDir, "stage_11_admlv1.csv"))) {
  cat("checking stage11 admin1 ...ignored\n")
} else {
  cat("checking stage11 admin1 ...skipped\n")
}


# stage 11 admin 2
if (file.exists(file.path(analysisOutDir, "stage_11_admlv2.csv"))) {
  cat("checking stage11 admin2 ...ignored\n")
} else {
  cat("checking stage11 admin2 ...skipped\n")
}

# stage 12


# stage 13
cat("checking stage13...")
stage13 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000), by = factors13]
stage13_final <- finalizeMonthly(stage13, factors13)
data.table::fwrite(stage13_final, file = file.path(testOutDir, "stage_13.csv"))

stage13_org <- data.table::fread(file.path(analysisOutDir, "stage_13.csv"))
diffCheck(stage13_final, stage13_org, factors13, file.path(testOutDir, "stage_13_diff.csv"))


# stage 14 admin 2
cat("checking stage14 admin2 ...")
stage142 <- stage1a[, .(production = sum(HWAH * HARVEST_AREA)/1000), by = factors142]
stage142_final <- finalizeMonthly(stage142, factors142)
data.table::fwrite(stage142_final, file = file.path(testOutDir, "stage_14_admlv2.csv"))

stage142_org <- data.table::fread(file.path(analysisOutDir, "stage_14_admlv2.csv"))
diffCheck(stage142_final, stage142_org, c("ADMLV_INFO", factors142), file.path(testOutDir, "stage_14_admin2_diff.csv"))


# stage 14 admin 1
cat("checking stage14 admin1 ...")
stage141 <- stage142[, .(production = sum(production)), by = factors141]
stage141_final <- finalizeMonthly(stage141, factors141)
data.table::fwrite(stage141_final, file = file.path(testOutDir, "stage_14_admlv1.csv"))

stage141_org <- data.table::fread(file.path(analysisOutDir, "stage_14_admlv1.csv"))
diffCheck(stage141_final, stage141_org, c("ADMLV_INFO", factors141), file.path(testOutDir, "stage_14_admin1_diff.csv"))


# stage 14 admin 0
cat("checking stage14 admin0 ...")
stage140 <- stage141[, .(production = sum(production)), by = factors140]
stage140_final <- finalizeMonthly(stage140, factors140)
data.table::fwrite(stage140_final, file = file.path(testOutDir, "stage_14_admlv0.csv"))

stage140_org <- data.table::fread(file.path(analysisOutDir, "stage_14_admlv0.csv"))
diffCheck(stage140_final, stage140_org, c("ADMLV_INFO", factors140), file.path(testOutDir, "stage_14_admin0_diff.csv"))

