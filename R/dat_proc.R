library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)

load(file = 'ignore/comid_statewide.Rdata')

prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

##
# csci scores
scrs <- read.csv('ignore/CSCI_raw.csv', header = T, stringsAsFactors = F) %>% 
  dplyr::select(SampleID, StationCode, New_Lat, New_Long, COMID, SampleDate, CSCI) %>% 
  rename(
    csci = CSCI, 
    lat = New_Lat, 
    long = New_Long
  ) %>% 
  mutate(
    SampleDate = dmy(SampleDate)
  )
  
save(scrs, file = 'data/scrs.RData', compress = 'xz')

##
# shed
shed <- readOGR('ignore/AmericanRiverWatershed_Subcatchments_v2.shp') %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf

save(shed, file = 'data/shed.RData', compress = 'xz')

##
# flowlines
spat <- readOGR('ignore/nhdflowline_RB5.shp') %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  st_intersection(shed) %>% 
  select(COMID)  

# simplify, join with all expectations
spat <- spat %>% 
  st_simplify(dTolerance = 0.003, preserveTopology = T) %>%
  left_join(comid, by = 'COMID') %>% 
  select(COMID, matches('^full0|^core0'))

save(spat, file = 'data/spat.RData', compress = 'xz')
  
