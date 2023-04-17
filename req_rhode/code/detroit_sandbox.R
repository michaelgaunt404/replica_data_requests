#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Sandbox Script
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: This is a basic workflow that contains all the general steps
#-------- one will want to execute when acquiring data from Replica.
#--------
#-------- Reminder: This script is supposed to be barebones. ReplicaToolkitR
#-------- suggests that you limit computation and variable creation to
#-------- to scripts and/or `targets` and then load those variables into
#-------- Rmakrdown or other data products.
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(crosstalk)
library(data.table)
library(dplyr)
library(forcats)
library(gauntlet)
library(here)
library(leafem)
library(leaflet)
library(leaflet.extras2)
library(log4r)
library(magrittr)
library(mapview)
library(purrr)
library(reactable)
library(readr)
library(sf)
library(sfhotspot)
library(SpatialKDE)
library(stringr)
library(tigris)
library(tidyr)
library(wellknown)

##inspect processed data=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section contains functions that make it easy to inspect the data made above

generators = here::here("req_detroit/data/TopFreightGeneratorsV3") %>%
  read_sf() %>%
  mutate(Name = str_remove(Name, "\n"))

generators_sf = replica_trip_origin_links %>%
  st_join(generators) %>%
  st_drop_geometry() %>%
  filter(!is.na(Name)) %>%
  group_by(Name, vehicle_type) %>%
  summarise(count_veh = sum(count)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(count_ttl = sum(count_veh)) %>%
  merge(generators, .,
        by = "Name") %>%
  select(!id)

generators_sf_hd = generators_sf %>%  filter(vehicle_type == "HEAVY_COMMERCIAL")
generators_sf_md = generators_sf %>%  filter(vehicle_type != "HEAVY_COMMERCIAL")
generators_sf_ttl = generators_sf %>%  select(!count_veh) %>%  unique()

replica_trip_origin_links_sa = replica_trip_origin_links %>%
  st_filter(acquired_sa_polys)

temp_map = mapview(generators_sf_ttl, zcol = "count_ttl", layer.name = "Generators - Combined Duty") +
  mapview(generators_sf_hd, zcol = "count_veh", layer.name = "Generators - Heavy Duty") +
  mapview(generators_sf_md, zcol = "count_veh", layer.name = "Generators - Medium Duty") +
  mapview(generators_sf_ttl, zcol = "count_ttl", layer.name = "Generators - Combined Duty") +
  mapview(generators, layer.name = "Identified Freight Gen.", label = "name") +
  mapview(acquired_sa_polys, layer.name = "Study Area Blockgroups") +
  mapview(replica_trip_origin_links_sa, zcol = "count", layer.name = "Raw Freight Origin Links"
                     ,cex = "count", alpha = "count", alpha.regions = "count")

temp_map@map = temp_map@map %>%
  leaflet::hideGroup(
    c("Raw Freight Origin Links", "Identified Freight Gen.", "Study Area Blockgroups")
  )


temp_map@map %>%
  htmlwidgets::saveWidget(here::here("req_detroit/docs", "detroit_gen_v3_map.html"))











