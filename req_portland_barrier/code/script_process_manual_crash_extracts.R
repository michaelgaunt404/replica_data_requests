#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(sf)
library(mapview)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
location = "req_portland_barrier/data/data_crash/odot_database_manual"

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts
project_locations_buffer = here::here(
  "req_portland_barrier/data"
  ,"project_locations_buffer.gpkg") %>%
  read_sf()

#prep===========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#make list of colnames
{
mult_2021 <- read.csv(
  "./req_portland_barrier/data/data_crash/odot_database_manual/multnomah21.txt"
  , header=FALSE)

list_colnames = c(1, 2, 3) %>%
  map(~{
    mult_2020 %>%
      filter(`Crash ID` == 1876326) %>%
      filter(`Record Type` == .x) %>%
      janitor::remove_empty("cols") %>%
      colnames() %>%
      sort()
  })

names(list_colnames) = c("crash", "vehicle", "person")
}

#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
files = list.files(location)
data_list = files[str_detect(files, ".txt")] %>%
  map(~{
    temp = read.csv(here::here(location, .x)
             , header=FALSE) %>%
      mutate(across(everything(), as.character))

  }) %>%
  reduce(bind_rows) %>%
  type_convert()

colnames(data_list) = headers

data_list = data_list %>%
  janitor::clean_names()

index_truck_inv_crashes = data_list %>%
  filter(vehicle_type_code %in% c(2, 4, 5, 11)) %>%
  pull(crash_id)

data_list_subset = data_list %>%
  filter(crash_id %in% index_truck_inv_crashes)

# write.csv(
#   data_list_subset
#   ,here::here(location, "data_list_subset.csv"))

data_list_subset = read.csv(
  here::here(location, "data_list_subset.csv"))

data_list_subset_pro = data_list_subset %>%
  # filter(record_type == 1) %>%
  select(crash_id, record_type, route_number, route_type
         ,collision_type, crash_severity
         ,vehicle_type_code
         ,total_vehicle_count, crash_year, highway_number, highway_suffix
         ,starts_with("crash_level_cause_1"), starts_with("collision_type"), starts_with("vehicle_movement_code")
         ,starts_with("vehicle_cause_1"), starts_with("vehicle_event_1")
         ,starts_with("latitude"), starts_with("longitude")) %>%
  mutate(longitude = str_glue("-{abs(longitude_degrees)+(longitude_minutes/60)+(longitude_seconds/3600)}")
         ,latitude = str_glue("{latitude_degrees+(latitude_minutes/60)+(latitude_seconds/3600)}")) %>%
  arrange(crash_id, record_type)

temp_comp = merge(
  data_list_subset_pro %>%
    filter(record_type == 2) %>%
    janitor::remove_empty("cols") %>%
    merge(index_vchl_cause, by.x = "vehicle_cause_1_code", by.y = "VHCL_CAUSE_1_CD", all.x = T) %>%
    merge(index_vchl_event, by.x = "vehicle_event_1_code", by.y = "VHCL_EVNT_CD", all.x = T) %>%
    merge(index_mvmnt, by.x = "vehicle_movement_code", by.y = "MVMNT_CD", all.x = T) %>%
    select(crash_id,vehicle_type_code,  ends_with("DESC"))
  ,data_list_subset_pro %>%
    filter(record_type == 1) %>%
    janitor::remove_empty("cols") %>%
    merge(index_crash_cause, by.x = "crash_level_cause_1_code", by.y = "CRASH_CAUSE_CD", all.x = T) %>%
    merge(index_collision_type, by.x = "collision_type", by.y = "COLLIS_TYP_CD", all.x = T) %>%
    select(!c(crash_level_cause_1_code, collision_type, crash_severity, record_type
              ,starts_with("longitude_"),starts_with("latitude_")))
  ,by = "crash_id") %>%
  mutate(label = str_glue("{crash_id} - {route_number} - {crash_year}
                          <br>Cause and collision type: {CRASH_CAUSE_SHORT_DESC} - {COLLIS_TYP_SHORT_DESC}
                          <br>Veh Code/MVMNT: {vehicle_type_code} - {MVMNT_SHORT_DESC}"))


temp_spatial = temp_comp %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>%
  st_filter(project_locations_buffer) %>%
  st_jitter(.00009)

temp_spatial %>%
  mapview::mapview()


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(crosstalk)

temp_spatial_sd = SharedData$new(temp_spatial)


bscols(
  widths = c(3,9)
  ,list(
    crosstalk::filter_select("vhcl_type", "Choose VHCL TYPE:",temp_spatial_sd,~vehicle_type_code)
    ,crosstalk::filter_select("vhcl_type", "Choose Specific Crash:",temp_spatial_sd,~crash_id)
  )
  ,leaflet(height = 600) %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet_default_tiles() %>%
    leaflet::addCircleMarkers(data = temp_spatial_sd
                              ,color = "black"
                              ,fillColor = "blue"
                              ,opacity = .8
                              ,fillOpacity  = .2
                              ,weight = 1
                              ,radius = 5
                              ,group = "Crash Locations"
                              # ,labelOptions = labelOptions(noHide = T, textsize = "15px")
                              ,label = temp_spatial$label %>%
                                purrr::map(htmltools::HTML)
                              ,popup = janitor::clean_names(temp_spatial) %>%
                                st_set_geometry(NULL) %>%
                                select(!label) %>%
                                leafpop::popupTable()
                              ,options = popupOptions(closeButton = FALSE)
                              )

)
























##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

