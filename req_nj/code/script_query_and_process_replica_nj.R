#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script contains a basic replicaToolkitR workflow.
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

library(replicaToolkitR)

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#none currently here - place your own source code here as you see fit

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging

# query_network_trip_using_bbox(
#   bb_network_layer = 'req_nj/data/nj_extent.gpkg'
#   ,bb_sa_layer = 'req_nj/data/nj_sm_studyarea.gpkg'
#   ,network_table = "replica-customer.north_atlantic.north_atlantic_2021_Q4_network_segments"
#   ,trip_table = "replica-customer.north_atlantic.north_atlantic_2021_Q4_thursday_trip"
#   ,customer_name = "replica-customer"
#   ,file_destination = "req_nj/data"
#   ,max_record = Inf
#   ,mode_type = c('COMMERCIAL')
#   ,query_links = c("highway", "corridor", "motorway", "motorway_link", "trunk",
#                    "trunk_link", "primary", "primary_link")
# )

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#should be the same as *file_destination* input above
location = 'req_nj/data'

#this is the folder that the data is written to
#it is a sub-folder to *file_destination* and will have a data_[datetime] format
# folder = 'data_20230222_130855'
# folder = 'data_20230315_103827'
# folder = 'data_20230315_111722' #without motorway_links
folder = 'data_20230315_114230' #mega large download

#process acquired data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable


##create gis layers=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
acquired_sa_polys = get_tigris_polys_from_replica_index(
  location = location
  ,folder = folder
  ,states = "OR"
  ,auto_save = T
)

make_network_link_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)

make_network_centroid_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)

##aggregate data================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make_trip_origin_point_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )
#
# aggregated_network_links = aggregate_network_links(
#   location = location
#   ,folder = folder
#   ,auto_save = T
#   ,agg_count_object = table_agg_by_link_subset_limited
#   ,network_object = replica_queried_network_links
# )


#VIZ============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##load data=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# poi_list = fread(header = T, here(location, "poi_list.csv"))
#
# acquired_sa_polys = read_sf(
#   here(location, folder ,"acquired_sa_polys.gpkg"))
#
# replica_trip_origin_destination = fread(
#   here(location, folder, "replica_trip_origin_destination.csv"))
#
# table_agg_by_link_subset_limited  = fread(
#   here(location, folder, "table_agg_by_link_subset_limited.csv"))
#
# replica_queried_network_links = read_sf(
#   here(location, folder ,"replica_queried_network_links.gpkg"))
#
# aggregated_network_links = aggregate_network_links(
#   location = location
#   ,folder = folder
#   ,auto_save = T
#   ,agg_count_object = table_agg_by_link_subset_limited
#   ,network_object = replica_queried_network_links
# )
#
# aggregated_network_links = read_rds(
#   here(location, folder ,"aggregated_network_links.rds"))

###data write-out=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export_agg_links_to_spatial(
  data = aggregated_network_links
  ,location = location
  ,folder = folder
)

#
# replica_queried_network_cntds = read_sf(
#   here(location, folder ,"replica_queried_network_cntds.gpkg"))
#
# aggregated_network_cntds = aggregate_network_links(
#   location = location
#   ,folder = folder
#   ,auto_save = F
#   ,agg_count_object = table_agg_by_link_subset_limited
#   ,network_object = replica_queried_network_cntds
# )


##inspect processed data=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section contains functions that make it easy to inspect the data made above
inspect_queried_network(
  location = location
  ,folder = folder
)







tmp = mapedit::drawFeatures() %>%
  st_transform(4326)









##OD stuff from origin links====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poly_origin_veh = acquired_sa_polys %>%
  merge(replica_trip_origin_destination %>%
          group_by(origin_poly, vehicle_type) %>%
          summarise(count = sum(count)) %>%
          ungroup()
        ,by.x = "GEOID10", by.y = "origin_poly")

poly_origin = acquired_sa_polys %>%
  merge(replica_trip_origin_destination %>%
          group_by(origin_poly) %>%
          summarise(count = sum(count)) %>%
          ungroup()
        ,by.x = "GEOID10", by.y = "origin_poly")

od_map = c("HEAVY_COMMERCIAL", "MEDIUM_COMMERCIAL") %>%
  map(~{
    poly_origin_veh %>%
      filter(vehicle_type == .x) %>%
      mutate(area_mi2 = round(ALAND10/2589988, 2)
             ,count_density = round(count/area_mi2, 2)) %>%
      mapview(zcol = "count_density"
              ,color = "black"
              ,layer.name  = str_glue("{str_replace(.x, '_COMMERCIAL', ' Duty')} (trips per mi2)")
              ,homebutton = F)
  }) %>%
  reduce(`+`)

od_agg_map = poly_origin %>%
  mutate(area_mi2 = round(ALAND10/2589988, 2)
         ,count_density = round(count/area_mi2, 2)) %>%
  mapview(zcol = "count"
          ,color = "black"
          ,layer.name  = str_glue("All (trips per mi2)")
          ,homebutton = F)

od_comb = (od_agg_map + od_map) %>%
  .@map %>%
  leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                       ,"MEDIUM Duty (trips per mi2)"))

htmlwidgets::saveWidget(temp_od
                        ,here(location, folder,  "viz", "freight_origins_map.html"))

export_agg_links(
  data = list(
    "origin_poly_trips" = poly_origin
    ,"origin_poly_trips_by_veh" = temp
  )
  ,location = location
  ,folder = folder
)

yolo = aggregated_network_links  %>%
map(~{
  .x %>%
    st_filter(tmp)
})



##static link maps==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static_vis_anlt = viz_static_ntwrk_map_anlt(
  spatial_agg_object = yolo
  ,return_leaflet = T
) %>%
  leaflet::hideGroup(c("HEAVY_COMMERCIAL", "MEDIUM_COMMERCIAL"))

static_vis_anltpt = viz_static_ntwrk_map_anltpt(
  spatial_agg_object = aggregated_network_links
  ,return_leaflet = F
)

# static_vis_anlto = viz_static_ntwrk_map_anlto_grp(
#   spatial_agg_object = aggregated_network_links
#   ,poi_list = poi_list
# )
#

static_vis_anlt_1 = static_vis_anlt %>%
  leaflet::hideGroup(c("HEAVY_COMMERCIAL", "MEDIUM_COMMERCIAL"))


list(
  list(static_vis_anlt_1#, static_vis_anltpt, static_vis_anlto
       )
  ,list("static_vis_anlt"#, "static_vis_anltpt", "static_vis_anlto"
        )
) %>%
  pmap(~{
    htmlwidgets::saveWidget(.x
                            ,here(location, folder, "viz", str_glue("{.y}.html")))
  })

##dynamic HTML widget maps======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make_network_map_anlt(
#   network_cntrd_object = aggregated_network_cntds
#   ,poi_list = poi_list
#   ,origin_polys = acquired_sa_polys
# )
#
# make_network_map_anltpt(
#   network_cntrd_object = aggregated_network_cntds
#   ,poi_list = poi_list
#   ,origin_polys = acquired_sa_polys
# )
#
# make_network_map_anlto(
#   network_cntrd_object = aggregated_network_cntds
#   ,poi_list = poi_list
#   ,origin_polys = acquired_sa_polys
# )


mapview(yolo)
