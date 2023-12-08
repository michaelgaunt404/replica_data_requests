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

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts


req_folder = "req_texas_border_crossings"

specfic_folder = "replica-texas_border-12_01_23_ciuadad_sf_street_bridge_north"
specfic_folder = "replica-texas_border-12_01_23_ciuadad_good_neighbor_north"
specfic_folder = "replica-texas_border-12_01_23-loredo_world_trade_bridge_north"
specfic_folder = "replica-texas_border-12_01_23-brownsville_vetrens_bridge_north"
file = "replica-texas_border-12_01_23-network_link_layer.shp"

data_sf = here::here(
  req_folder, "data", specfic_folder
  ,file
) %>%
  read_sf() %>%
  st_transform(4326)

data_sf %>%
  mutate(lwd = gauntletMap::rescale_to(trip_count, 10) ) %>%
  mapview(zcol = "trip_count", lwd = "lwd")


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

specfic_folder =
  c("replica-texas_border-12_01_23_ciudad_good_neighbor_north"
    ,"replica-texas_border-12_01_23_ciudad_sf_street_bridge_north"
    ,"replica-texas_border-12_01_23-brownsville_vetrens_bridge_north"
    ,"replica-texas_border-12_01_23-ciudad_zaragoza_bridge_north"
    ,"replica-texas_border-12_01_23-ciudad_zaragoza_bridge_south"
    ,"replica-texas_border-12_01_23-laredo_solidartiy_int_bridge_north"
    ,"replica-texas_border-12_01_23-laredo_world_trade_bridge_north")

data_sf_pro = specfic_folder %>%
  map(~{
    data_sf = here::here(
      req_folder, "data", .x
      ,file
    ) %>%
      read_sf() %>%
      st_transform(4326) %>%
      mutate(location = gsub(".*23_", "\\1", .x) %>%
               gsub(".*23-", "\\1", .) %>%
               str_remove("_north")
             ,direction = "north")}) %>%
      reduce(bind_rows) %>%
  mutate(location_pro = str_replace(location, "_", " - ") %>%
           gauntlet::strg_pretty_char())

write_sf(
  data_sf_pro
  ,here::here(req_folder, "data", "extracted_texas_freight_port_of_entry_20231201.shp")
)


map = data_sf_pro %>%
  group_by(location_1 = location) %>%
  group_map(~{
    # print(.x)
    index = unique(.x$location)

    # print(index)
    #
    layer_name = index %>%
      str_replace("_", " - ") %>%
      gauntlet::strg_pretty_char()

    map_1 = .x %>%
      mutate(lwd = gauntletMap::rescale_to(trip_count, 10) ) %>%
      mapview(zcol = "trip_count", layer.name= layer_name,lwd = "lwd")

    return(map_1)
  }) %>%
  reduce(`+`)

index = sort(unique(data_sf_pro$location)) %>%
  str_replace("_", " - ") %>%
  gauntlet::strg_pretty_char()

map = map@map %>%
  leaflet::hideGroup(index)

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================








































