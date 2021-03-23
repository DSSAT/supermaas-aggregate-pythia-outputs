#library(here)
library(argparser)

setwd(".")

data_cde_file <- "DATA_CDE.csv"
if (file.exists(data_cde_file)) {
  var_dic <- data.table::fread(data_cde_file)
} else {
  # const_ha_vars <- c("DWAP", "CWAM", "HWAM", "HWAH", "BWAH", "PWAM", "")
  # const_temp_vars <- c("TMAXA", "TMINA")
  # const_date_vars <- c("SDAT", "PDAT", "EDAT", "ADAT", "MDAT", "HDAT")
}

predefined_vars <- c("PRODUCTION")

p <- argparser::arg_parser("Generate Map with Aggregated Pythia outputs for World Modelers")
p <- argparser::add_argument(p, "input_csv", "Aggregated Pythia result CSV file for generating map")
p <- argparser::add_argument(p, "input_shape", "Shape file for generating map")
p <- argparser::add_argument(p, "output", "Path to the generated map file, the file name is optional")
p <- argparser::add_argument(p, "--variables", short = "-v", nargs = Inf, help = paste("Variable names for generating map: [", paste(c(predefined_vars, var_dic[total != "" | average != "", name]), collapse = ","), "]"))
argv <- argparser::parse_args(p)

# for test only
# argv <- argparser::parse_args(p, c("test\\data\\case5\\report5.csv", "test\\data\\case5\\ETH_Kelem_shp\\Kelem_Wellega_Oramia.shp", "test\\output", "-v", "PRODUCTION", "CWAM", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case5\\report5.csv", "test\\data\\case5\\ETH_Kelem_shp\\Kelem_Wellega_Oramia.shp", "test\\output"))
# argv <- argparser::parse_args(p, c("test\\data\\case7\\report6.csv", "test\\data\\case7\\gadm36_GHA_0.shp", "test\\output", "-v", "HWAH"))
# argv <- argparser::parse_args(p, c("test\\data\\case8\\agg_hist4p_mai_ir.csv", "test\\data\\case8\\05d_pt_land2_soil_grid_bnd.shp", "test\\output", "-v", "HWAH", "PRCP"))

library(ggplot2)
library(sf)
library(raster)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata) #for shapefiles, addition to gadm

suppressWarnings(in_csv <- normalizePath(argv$input_csv))
suppressWarnings(in_shape <- normalizePath(argv$input_shape))
suppressWarnings(out_file <- normalizePath(argv$output))

variables <- argv$variables
suppressWarnings(if (is.na(variables)) {
  variables <- predefined_vars;
})

base_file_name = tools::file_path_sans_ext(basename(out_file))
file_ext = tools::file_ext(out_file)
if (file_ext == "") {
  out_dir <- out_file
  file_ext <- "png"
} else {
  out_dir <- dirname(out_file)
}



if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

print("Loading files for map.")
shp_data<-st_read(in_shape) #read shapefile
crs(shp_data)
Ylddata<-read.csv(in_csv) #average yield data with lat and long

plot_yld_data <- st_as_sf(Ylddata, coords = c("lng", "lat"), crs =4326) #converting into spatial data
st_crs(plot_yld_data)
for (variable in variables) {
  if (variable == "PRODUCTION") {
    headers <- "production"
  } else {
    headers <- paste0(variable, c("_TOT", "_AVE"));
  }
  for (variable in headers) {
    if (variable %in% colnames(plot_yld_data)) {
      print(paste0("Processing map for ", variable))
      ggplot()+
        geom_sf(data=plot_yld_data, aes(color=get(variable)))+
        geom_sf(data=shp_data, size=0.25, alpha=0.5, fill="grey")+
        # borders()
        labs(x="Longitude", y="Latitude", color=variable)+
        scale_color_gradientn(colours = rainbow(7),
                              n.breaks=6
                              # ,labels=c(0,5, 10, 15, 20, 25)
        )+
        annotation_scale(location = "bl", width_hint = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", 
                               pad_x = unit(0.05, "in"), pad_y = unit(0.25, "in"),
                               style = north_arrow_fancy_orienteering)
      
      
      ggsave(
        filename = paste0(base_file_name, "_", variable, ".", file_ext),
        plot = last_plot(),
        path = out_dir
      )
    } else {
      print(paste0("Processing map for ", variable, " is skipped"))
    }
  }
}
print("Complete.")

