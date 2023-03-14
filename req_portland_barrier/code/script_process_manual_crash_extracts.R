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
         ,total_vehicle_count, crash_year, highway_number, highway_suffix
         ,starts_with("crash_level_cause_1"), starts_with("collision_type"), starts_with("vehicle_movement_code")
         ,starts_with("vehicle_cause_1"), starts_with("vehicle_event_1")
         ,starts_with("latitude"), starts_with("longitude")) %>%
  mutate(longitude = str_glue("-{abs(longitude_degrees)+(longitude_minutes/60)+(longitude_seconds/3600)}")
         ,latitude = str_glue("{latitude_degrees+(latitude_minutes/60)+(latitude_seconds/3600)}")) %>%
  arrange(crash_id, record_type)

data_list_subset_pro %>%
  filter(record_type == 1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>%
  st_filter(project_locations_buffer) %>%
  st_jitter(.00005) %>%
  select(!c(starts_with("latitude"), starts_with("longitude"))) %>%
  mapview::mapview()

data_list_subset_pro %>%
  filter(crash_id == 1768829) %>%
  select(!c(starts_with("latitude"), starts_with("longitude"))) %>%
  glimpse()


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

