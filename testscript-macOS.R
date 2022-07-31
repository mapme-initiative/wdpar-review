# Purpose: install and test package on MacOS
# Author: Johannes Schielein
# last edit: 2022-07-23

# Steps: Followed the instruction steps for MacOS installing external dependencies as
# described here: https://github.com/prioritizr/wdpar

# install.packages("wdpar", repos = "https://cran.rstudio.com/")

# ----- reproduce quickstart tutorial from https://github.com/prioritizr/wdpar -----
# load packages
library(wdpar)
library(dplyr)
library(ggmap)
library(mapview)

# download protected area data for Malta
mlt_raw_pa_data <- wdpa_fetch("Malta",
                              wait = TRUE,
                              download_dir = rappdirs::user_data_dir("wdpar"))

# clean Malta data
mlt_pa_data <- wdpa_clean(mlt_raw_pa_data)

# reproject data to longitude/latitude for plotting
mlt_pa_data <- st_transform(mlt_pa_data, 4326)

# download basemap imagery
bg <- get_stamenmap(unname(st_bbox(mlt_pa_data)),
                    zoom = 8,
                    maptype = "watercolor",
                    force = TRUE)

# make map
ggmap(bg) +
  geom_sf(aes(fill = IUCN_CAT), data = mlt_pa_data, inherit.aes = FALSE) +
  theme(axis.title = element_blank(), legend.position = "bottom")

# ----- interactive map to compare raw to processed data -----


mapView(mlt_pa_data) + mapView(mlt_raw_pa_data, col.regions = "red")

# ----- compare higher precision processing -----
# problem: Data with the default setting gets quite distorted on local level
# (see mapview before)

# clean Malta data with higher geomtry precision
mlt_pa_data_highprecision <- wdpa_clean(mlt_raw_pa_data,
                                        geometry_precision = 10000)


mapView(mlt_pa_data) +
  mapView(mlt_raw_pa_data, col.regions = "red") +
  mapView(mlt_pa_data_highprecision, col.regions = "green")


# clean Malta data with higher geomtry precision and keep overlaps
mlt_pa_data_highprecision_overlap <- wdpa_clean(mlt_raw_pa_data,
                                                geometry_precision = 10000,
                                                erase_overlaps = FALSE)


mapView(mlt_pa_data) +
  mapView(mlt_raw_pa_data, col.regions = "red") +
  mapView(mlt_pa_data_highprecision, col.regions = "green")+
  mapView(mlt_pa_data_highprecision_overlap, col.regions = "purple")


# ----- Testmedium data based on code from original documentation -----
starttime<-Sys.time()
# download protected area data for multiple of countries
## (i.e. Portugal, Spain, France)
raw_pa_data <-
  c("BRA", "COL", "PER") %>%
  lapply(wdpa_fetch, wait = TRUE,
         download_dir = rappdirs::user_data_dir("wdpar")) %>%
  bind_rows()

# clean protected area data (with procedure for erasing overlaps disabled)
full_pa_data <- wdpa_clean(raw_pa_data,
                           geometry_precision = 10000,
                           erase_overlaps = FALSE)

# stop time
stoptime<-Sys.time()

starttime-stoptime

nrow(full_pa_data)
mapview(full_pa_data)


# ----- Test large data (ODA recipient countries)-----
library(pbmcapply)
library(xlsx)

# get ISO3 codes for countries who are ODA recipients
# dir.create("input")
# dir.create("input/wdpa")
download.file(
  "https://www.theglobalfund.org/media/8341/core_historicaleligibility_database_en.xlsx",
  "input/oda_eligibility_en.xlsx"
)
oda_eligibility<-
  read.xlsx("input/oda_eligibility_en.xlsx",sheetIndex = 3,startRow = 2)

oda_eligibility_isocodes<-
  unique(oda_eligibility$ISO3)

oda_eligibility_isocodes

# Filter out countries where the iso3 code caused problems with download
oda_eligibility_isocodes<-oda_eligibility_isocodes[!oda_eligibility_isocodes%in%c("NRU","XKX","ZAN")]

# get data via isocodes
raw_pa_data <-
  oda_eligibility_isocodes %>%
  lapply(wdpa_fetch, wait = TRUE,
         download_dir = "input/wdpa/") %>%
  bind_rows()

# print nr. of PAs to process
print(paste("Nr. of PAs to be processed is",nrow(raw_pa_data)))

# collect start and stop time and process data using 8 cores

# clean protected area data (with procedure for erasing overlaps disabled)
f.clean<-
  function(file)
  {
    dat<- tryCatch(
      {wdpa_clean(file,
                   geometry_precision = 10000,
                   erase_overlaps = FALSE)},
      error = function(cond) {
        message(cond)
        return(NA)
      }
    )
    return(dat) 
  }

# run on multiple cores
starttime<-Sys.time()

full_pa_data <- 
  pbmclapply(1:nrow(raw_pa_data), function(i) {
    f.clean
  },mc.cores = 6)

# stop time
stoptime<-Sys.time()

stoptime-starttime

# reduce the data to valid observations and create one dataset
full_pa_data<-
  full_pa_data[!lapply(full_pa_data,class)=="try-error"]

full_pa_data<-
  lapply(full_pa_data,st_transform,crs="EPSG:4326")

lapply(full_pa_data,str,max.level=1)
full_pa_data_df<-
  do.call(rbind,full_pa_data[1:10000])

# export data
dir.create("output")
write_sf(raw_pa_data, 
         "output/wdpa_oda_recipients_RAW_V1.gpkg")

# ----- test large data_ tropical countries -----
library(sf)
wdpa_1<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_original_April2021/WDPA_Apr2021_Public/WDPA_Apr2021_Public.gdb/")

# Step 1: Filter PAs for tropics based ymin and ymax. 
# get min max from bounding box
wdpa_1$ymin<-NA
wdpa_1$ymax<-NA
for(i in 1:nrow(wdpa_1)){
  wdpa_1$ymin[i]<-st_bbox(wdpa_1[i,])$ymin
  wdpa_1$ymax[i]<-st_bbox(wdpa_1[i,])$ymax
}

# see how many PAs are in the tropics
table(wdpa_1$ymax<23.5&wdpa_1$ymin>-23.5) # 18066 PAs in the tropics !

wdpa_1$is_tropical<-wdpa_1$ymax<23.5&wdpa_1$ymin>-23.5
# filter
wdpa_tropics<-wdpa_1[which(wdpa_1$is_tropical==TRUE),]

table(wdpa_tropics$PARENT_ISO3)
# plot(wdpa_tropics,"PARENT_ISO3")
rm(wdpa_1)


# mapView(wdpa_tropics) # needs lots of ressources for rendering -> crashes session

# run on multiple cores
starttime<-Sys.time()

# tropics_pa_data <- 
#   pbmclapply(1:nrow(wdpa_tropics), function(i) {
#     f.clean(wdpa_tropics[i,])
#   },mc.cores = 6)

wdpa_tropics_clean<-
  wdpa_clean(wdpa_tropics,
             geometry_precision = 10000,
             erase_overlaps = FALSE)


# stop time
stoptime<-Sys.time()

stoptime-starttime

# get centroids
wdpa_tropics_clean_centroids<-
  st_centroid(wdpa_tropics_clean)

## create column for area coloring based on categories
wdpa_tropics_clean_centroids$GIS_AREA_cat<-
  cut(wdpa_tropics_clean_centroids$GIS_AREA,
      c(0,1000,5000,10000,20000,max(wdpa_tropics_clean_centroids$GIS_AREA)),
      c("< 1,000 sqkm","1,001-5,000 sqkm","5,001-10,000 sqkm","10,001-20,000 sqkm",paste("20,001-",max(wdpa_tropics_clean_centroids$GIS_AREA)," sqkm",sep="")))

mapView(wdpa_tropics_clean_centroids,zcol="GIS_AREA_cat")
?mapView
# eventually save workspace
save.image("output/workspace.Rdata")
load("output/workspace.Rdata")
