renv::restore(prompt=FALSE)

# Download and deploy GADM shape files
gadmDir <- "gadm_shapes"
if (!dir.exists(gadmDir)) {
  dir.create(path=gadmDir, recursive = T)
  ### download the GADM shape file with given URL
  gadmUrl = "https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_shp.zip"
  gadmZip = file.path(gadmDir, "gadm36_levels_shp.zip")
  download.file(url=gadmUrl, destfile=gadmZip)
  unzip(zipfile=gadmZip, exdir = gadmDir,
        files=grep(unzip(zipfile=gadmZip, list=TRUE)$Name, pattern="gadm36_*.*", value=TRUE, ignore.case = TRUE))
  file.remove(gadmZip)
}