#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script is a genreal purpose script to make polys with mapedit
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
library(mapedit)
library(mapview)
library(sf)
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
location = "req_portland_barrier/data"

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#make network poly==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_point_2 = mapedit::drawFeatures() %>%
  st_transform(4326)

network_point_1 = network_point_1 %>%  mapedit::editFeatures() %>%
  st_transform(4326)

mapview(network_point_3) + mapview(network_point_1) + mapview(network_point_2)

temp = read_sf(here::here(location, "network_poly.gpkg"))

 write_sf(network_poly, here::here(location, "network_poly.gpkg"))

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================


 counties = tigris::counties(state = "OR") %>%  st_transform(4326)


 counties_sel = counties %>%  mapedit::selectFeatures()

 roads_1 = tigris::roads(state = 41
                         ,county = 005) %>%  st_transform(4326)

 roads_2 = tigris::roads(state = 41
                         ,county = 047) %>%  st_transform(4326)

 roads = bind_rows(
   roads_1
   ,roads_2
 )


 roads = counties_sel$COUNTYFP %>%
   map(~{
     tigris::roads(state = 41
                   ,county = .x) %>%
       st_transform(4326)
   }) %>%
   reduce(bind_rows)

 roads %>%
   filter(str_detect(FULLNAME, "Barlow|99")) %>%
   mapview()






 project_locations_points = bind_rows(
   network_point_3
   ,network_point_1
   ,network_point_2)


 project_locations_buffer = project_locations_points %>%
   quick_buffer(radius = 1000)


 project_locations_buffer_union = project_locations_buffer %>%
   st_union()

mapview( project_locations_buffer_union)



write_sf(network_poly, here::here(location, "network_poly.gpkg"))
write_sf(project_locations_buffer_union, here::here(location, "project_locations_buffer_union.gpkg"))
write_sf(project_locations_buffer, here::here(location, "project_locations_buffer.gpkg"))
write_sf(network_poly, here::here(location, "network_poly.gpkg"))



roads_sel = roads %>%
  st_filter(project_locations_buffer) %>%
  filter(str_detect(FULLNAME, "- 84|State Hwy 99E|- 5|US Hwy 30|State Hwy 211|State Hwy 224|Jordan|N Lombard St|Toll House Park|State Frontage Rd|S Molalla|E Historic Columbia River Hwy|Crown Point Hwy|Barlow Rd|NW Cornelius Pass Rd|SE Milwaukie Ave|Pacific Hwy") |
           LINEARID %in% c(1104257985936, 1104257985936, 1102155320882, 1102223149165, 1102223149165
                           ,110686011838, 110501879631)) %>%
  filter(!str_detect(FULLNAME, "Bus"))

roads_sel_buf = roads_sel %>%
  quick_buffer(radius = 20)
