# Title: EM Large Sharks: Obj 1 - cleaning data ----
# The below documents the steps for downloading NORPAC data directly from AFSC
# inputting OBS special project data, merging with NORPAC haul data
# and making non-confidential location info
# Updated: Jul 29 2022
# Recent Author: Cindy Tribuzio

# TO DO ----
#1) automate pulling data from google sheet
#2) figure out why duplicates are added

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "lubridate","sp","sf", 
          "rnaturalearth", "rgdal", "rnaturalearthdata","spatialEco", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

datadir<-paste(getwd(),"/Obj1_obs_size_method/Data/Raw_Data/",sep="")
outdir<-paste(getwd(),"/Obj1_obs_size_method/Data/Cleaned_Data/",sep="")
shapedir <- paste(getwd(),"/Obj1_obs_size_method/Data/Shapefile/",sep="")

# Get NORPAC Data ----
# NORPAC haul data, queried from AKFIN by year
# skip this section if data have already been queried

dbname <- "afsc"
db <- read_csv('database.csv')
database_afsc=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_afsc=db %>% filter(database == dbname) %>% select(username)
password_afsc=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_afsc, pwd = password_afsc, believeNRows=FALSE))


NORPAC_haul <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_haul
                where     year >= 2018")) %>% 
  clean_names() %>% 
  mutate(retrieval_date = dmy(haul_date))

write_csv(NORPAC_haul, paste0(datadir, "confidential_NORPAChaul_18_22.csv"))
rm(NORPAC_haul)

# read in queried data
norpac <- read_csv(paste0(datadir, "confidential_NORPAChaul_18_22.csv")) %>% 
  select(cruise, permit, year, haul, haul_date, nmfs_area, latdd_start, londd_start) %>% 
  mutate(haul_date = floor_date(haul_date, "day"))


# OBS special project Data ----
obsSP <- read_csv(paste(datadir, "confidential_NPGOP_PSS_length.csv", sep = "")) %>% 
  mutate(Haul = as.numeric(gsub("/", "", Haul))) %>% 
  clean_names() %>% 
  rename("gear_desc" = "gear",
         "vessel_name" = "vessel",
         "sex_obs" = "sex",
         "haul_date" = "date") %>% 
  mutate(haul_date = mdy(haul_date))

# Merge haul data ----
round1match <- obsSP %>% 
  left_join(norpac, c("cruise", "permit", "year", "haul", "haul_date")) %>% 
  filter(!(is.na(nmfs_area))) #removes rows that didn't match

# Identify records that didn't merge
# sometimes cruise # is recorded for the wrong observer onboard
# try permit/haul/date
nomatch <- obsSP %>% 
  left_join(norpac, c("cruise", "permit", "year", "haul", "haul_date")) %>% 
  filter(is.na(nmfs_area)) %>% #removes rows that DID match
  select(c("cruise","permit","year","observer","vessel_name","gear_desc","haul_date","haul",
           "size_est","visual_cue","total_haul_pss","census","sex_obs","photo_taken",
           "wt","pcl","specimen_yn","specimen_num","notes","notes2" ))
round2match <-nomatch %>% 
  left_join(norpac, by = c("year", "permit", "haul", "haul_date"))%>% 
  filter(!(is.na(nmfs_area))) %>% 
  rename("cruise" = "cruise.y") %>% 
  select(!cruise.x)# add this to obsSP_haul

nomatch2 <- nomatch %>% 
  left_join(norpac, by = c("year","permit", "haul", "haul_date"))%>% 
  filter(is.na(nmfs_area)) 

# error check individuals as needed
write_csv(nomatch2, paste0(datadir, "confidential_non_matched_data.csv"))
#write_csv(norpac, paste0(datadir, "confidential_cleaned_norpac.csv"))

# combine merged objects
obsSP_conf <- round1match %>% 
  bind_rows(round2match) %>% 
  mutate(sampID = paste(62, "_", year, "_", cruise, "_", permit, "_", haul, "_", row_number(), sep=""))

# Spatial Joins for making non-confidential ----
AKncf_grid <- readOGR(dsn = paste(shapedir,"20kmhexagon_clip",sep=""),
                      layer = "20kmhexagon_clip")
# view grid
plot(AKncf_grid)

# Observer Data example ----
#datfile <- "CONFIDENTIAL_NORPAC_missing_hauls.csv"
#filedir <- paste(getwd(), "/data/catch/", datfile, sep = "")
#OBS_dat <- read_csv(filedir, skip = 6) %>% 
#  clean_names() %>% 
#  mutate(Fdepth_m = fishing_depth_fathoms*1.8288,
#         Bdepth_m = bottom_depth_fathoms*1.8288,
#         haulID = paste(species_code, "_", year, "_", cruise, "_", permit, "_", haul, "_", sample_type, sep="")) %>% #the haul ID is unique for each line, cleaned up hauljoin
#  select(!c(bottom_depth_fathoms, fishing_depth_fathoms, t_table, date_of_entry, #get rid of excess columns
#            received_from_norpac, loaded_to_repository, lat_dd_start, lon_dd_start))

  
# Create smaller set dataframe without extra cols
set_sm <- obsSP_conf %>% 
  select(lon = londd_start, 
         lat = latdd_start, 
         sampID) %>% 
  data.frame

# Make non-confidential ----
# Setting existing coordinate as lat-long system
OBSsp <- SpatialPointsDataFrame(coords = set_sm[, c(1, 2)], 
                                data = set_sm,
                                proj4string = CRS("+proj=longlat"))
OBSsp <- spTransform(OBSsp, CRS(proj4string(AKncf_grid)))

# merging layers
OBSsp2<-point.in.poly(OBSsp,AKncf_grid)

# extracting data
OBS_dat2<-as.data.frame(OBSsp2@data) %>% 
  select(c(sampID, Latitude, Longitude)) %>% 
  rename(ncf_lat = Latitude,
         ncf_lon = Longitude)

# Summary ----
# NOTE: there are haulIDs outside of AK waters, which returned NA for ncf lat/long

OBS_out <- left_join(obsSP_conf, OBS_dat2)

nrow(obsSP_conf) == nrow(OBS_out)

write_csv(OBS_out, paste(outdir, "confidential_NCF_Obj1_obsSPdat.csv", sep = ""))
