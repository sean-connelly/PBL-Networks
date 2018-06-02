
library(hrbrthemes);library(tidyverse)
library(stringr);library(lubridate)
library(zoo);library(scales)
library(sf);library(janitor)
library(viridis);library(leaflet)

options(scipen = 1000,stringsAsFactors = F)

setwd("E:/Data/PBL-Networks")

#=============
#Washington, DC
#=============

#Shapefile
dc <- st_read("Bicycle_Lanes.shp")

#Standardize
dc <- dc %>% 
  mutate(City = "Washington DC",
         Facility_Type = FACILITY,
         Facility_Class = NA_character_,
         Miles = BIKELANELE,
         Install_Year = ifelse(YEAR_INSTA==0,BIKELANE_Y,YEAR_INSTA)) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Type %in% c("Climbing Lane","Contraflow Bike Lane",
                              "Cycle Track","Existing Bike Lane"))

#Write local copy
st_write(dc,"./Shapefiles/Washington.shp")

#=============
#Seattle, WA
#=============

#Shapefile
seattle <- st_read("Existing_Bike_Facilities.shp")

#Standardize
seattle <- seattle %>% 
  mutate(City = "Seattle",
         Facility_Type = existing_f,
         Facility_Class = NA_character_,
         Miles = length_mil,
         Install_Year = year(date_compl)) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Type %in% c("In Street, Major Separation","Multi-use Trail",
                              "In Street, Minor Separation"))

#Write local copy
st_write(seattle,"./Shapefiles/Seattle.shp")

#=============
#New York, NY
#=============

#Shapefile
nyc <- st_read("geo_export_1f1902f2-5525-4be8-96e1-65cd9edee28a.shp")

#Standardize
nyc <- nyc %>% 
  mutate(City = "New York City",
         Facility_Type = coalesce(ft_facilit,tf_facilit),
         Facility_Class = paste0("Class ",allclasses),
         Miles = NA_real_,
         Install_Year = year(date_instd)) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Class %in% c("Class I","Class II"))

#Transform
nyc <- nyc %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

#Write local copy
st_write(nyc,"./Shapefiles/New_York_City.shp")

#=============
#Madison, WI
#=============

#Shapefile
madison <- st_read("Bike_Paths.shp")

#Standardize
madison <- madison %>% 
  filter(Status=="EX") %>% 
  mutate(City = "Madison",
         Facility_Type = NA_character_,
         Facility_Class = case_when(BFuncClass=="P" ~
                                      "Class I",
                                    BFuncClass=="S" ~
                                      "Class II",
                                    TRUE ~ NA_character_),
         Miles = NA_real_,
         Install_Year = as.numeric(Year)) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Class %in% c("Class I","Class II"))

#Write local copy
st_write(madison,"./Shapefiles/Madison.shp")

#=============
#San Francisco, CA
#=============

#Shapefile
sfca <- st_read("geo_export_32227f06-1806-42d8-8b58-0bd06a3742f2.shp")

#Standardize
sfca <- sfca %>% 
  mutate(City = "San Francisco",
         Facility_Type = symbology,
         Facility_Class = facility_t,
         "Miles" = length,
         Install_Year = coalesce(install_yr,year(date_creat))) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Class %in% c("CLASS I","CLASS II"))

#Transform CRS
sfca <- sfca %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

#Write local copy
st_write(sfca,"./Shapefiles/San_Francisco.shp")

#=============
#Portland, OR
#=============

#Shapefile
pdx <- st_read("Bicycle_Network.shp")

#Standardize
pdx <- pdx %>% 
  filter(Status=="ACTIVE") %>% 
  mutate(City = "Portland",
         Facility_Type = case_when(Facility=="ABL" ~
                                     "Advisory Bike Lane",
                                   Facility=="BBL" ~
                                     "Buffered Bike Lane",
                                   Facility=="BL" ~
                                     "Bike Lane",
                                   Facility=="ESR" ~
                                     "Enhanced Shared Roadway",
                                   Facility=="NG" ~
                                     "Neighborhood Greenway",
                                   Facility=="PBL" ~
                                     "Protected Bike Lane",
                                   Facility=="SIR" ~
                                     "Separated In Roadway",
                                   Facility=="TRL" ~
                                     "Trail",
                                   TRUE ~ NA_character_),
         Facility_Class = NA_character_,
         Miles = LengthMile,
         Install_Year = as.numeric(YearBuilt)) %>% 
  select(City,Facility_Type,Facility_Class,Miles,Install_Year,geometry) %>%
  filter(Facility_Type %in% c("Buffered Bike Lane","Bike Lane","Protected Bike Lane",
                              "Separated In Roadway","Trail"))

#Write local copy
st_write(pdx,"./Shapefiles/Portland.shp")

