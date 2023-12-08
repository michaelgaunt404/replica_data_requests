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
library(gauntletMap)

library(replicaToolkitR)








# replicaToolkitR::query_network_trips_using_bbox
#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#none currently here - place your own source code here as you see fit

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging

query_network_trip_using_bbox(
  bb_network_layer = 'data/req_dev/req_florida_tampa/data/network_poly_20231107.gpkg',
  bb_sa_layer = 'data/req_dev/req_florida_tampa/data/network_poly_20231107.gpkg',
  query_links = c("highway", "corridor", "motorway", "motorway_link", "trunk",
                  "trunk_link", "primary", "primary_link"),
  mode_type = c("COMMERCIAL"),
  data_set_location = "south_atlantic",
  data_set_period = "2023_Q2",
  data_set_day = "thursday",
  customer_name = "replica-customer",
  prefix_origin = "start",
  prefix_dest = "end",
  file_destination = "data/req_dev/req_florida_tampa/data",
  query_network = T,
  max_record = Inf
)

library(gauntletMap)


#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#should be the same as *file_destination* input above
location = 'data/req_dev/req_florida_tampa/data'

#this is the folder that the data is written to
#it is a sub-folder to *file_destination* and will have a data_[datetime] format
folder = 'data_20231110_102657'

#process acquired data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable


##create gis layers=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_tigris_polys_from_replica_index_2(
  location = location
  ,folder = folder
  # ,states = "TX"
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
# NOTE:MG_20231110 - I think this is obsolete as it looks like it does first link string operation rather than origins
make_trip_origin_point_layer_2(
  location = location
  ,folder = folder
  ,auto_save = T
)

#VIZ============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##load data=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#currently no POI_list - for tampa request
poi_list = fread(
  header = T, skip = 0, here(location, "poi_list.csv")) %>%
  janitor::remove_empty("rows") %>%
  mutate(id = parse_number(id))

replica_queried_network_links = read_sf(
  here(location, folder ,"replica_queried_network_links.gpkg"))

replica_queried_network_cntds = read_sf(
  here(location, folder ,"replica_queried_network_cntds.gpkg"))

#this is artifact of previous analysis - I think its because link names get truncated but cant remmeber if its aproblem or nto
# replica_queried_network_links_trunc = read_sf(
#   here(location, folder ,"replica_queried_network_links_trunc.gpkg"))

acquired_sa_polys = read_sf(here(location,folder,"acquired_sa_polys.gpkg"))
#
# replica_trip_origin_links = read_sf(
#   here(location, folder, "replica_trip_origin_links.gpkg"))

table_agg_by_link_subset_limited = fread(
  here(location, folder, "table_agg_by_link_subset_limited.csv"))

aggregate_network_links(
  location = location
  ,folder = folder
  ,auto_save = T
  ,agg_count_object = table_agg_by_link_subset_limited
  ,network_object = replica_queried_network_links
)

aggregated_network_links = read_rds(
  here(location, folder, "aggregated_network_links.rds"))

aggregated_network_cntds = aggregate_network_links(
  location = location
  ,folder = folder
  ,auto_save = T
  ,agg_count_object = table_agg_by_link_subset_limited
  ,network_object = replica_queried_network_cntds
)

aggregated_network_cntds %>%
  read_rds(
    here(location, folder ,"aggregated_network_links.rds")
  )

replica_trip_origin_links = read_sf(
  here(location, folder ,"replica_trip_origin_links.gpkg"))

replica_trip_origin_destination = fread(
  here(location, folder, "replica_trip_origin_destination.csv"))

###data write-out=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export_agg_links_to_spatial(
  data = aggregated_network_links
  ,location = location
  ,folder = folder
)

function(data, location, folder){
  # data = aggregated_network_links

  index_names = names(data)

  full_location = here::here(location, folder, "spatial_output")

  if (!exists(full_location)){
    dir.create(full_location)
  }

  message(str_glue("Aggregated link RDS object elements will be saved individually at the following location:\n{paste0('./', location, '/', folder, '/', 'spatial_output')}"))

  index_names %>%
    purrr::map(~{
      write_sf(
        data[[.x]]
        ,here::here(location, folder, "spatial_output", stringr::str_glue("{.x}.gpkg"))
      )

    })
}


##inspect processed data=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section contains functions that make it easy to inspect the data made above
inspect_queried_network(
  location = location
  ,folder = folder
)

##OD stuff from origin links====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp = acquired_sa_polys %>%
  merge(replica_trip_origin_destination %>%
          group_by(origin_poly, vehicle_type) %>%
          summarise(count = sum(count)) %>%
          ungroup()
        ,by.x = "GEOID10", by.y = "origin_poly")

temp_agg = acquired_sa_polys %>%
  merge(replica_trip_origin_destination %>%
          group_by(origin_poly) %>%
          summarise(count = sum(count)) %>%
          ungroup()
        ,by.x = "GEOID10", by.y = "origin_poly")

od_map = c("HEAVY_COMMERCIAL", "MEDIUM_COMMERCIAL") %>%
  map(function(x) {
    temp_object = temp %>%
      filter(vehicle_type == x) %>%
      filter(ALAND10 > 0) %>%
      mutate(area_mi2 = round(ALAND10/2589988, 5)
             ,count_density = round(count/area_mi2, 1)) %>%
      mapview(zcol = "count_density"
              ,color = "black"
              ,layer.name  = str_glue("{str_replace(x, '_COMMERCIAL', ' Duty')} (trips per mi2)")
              ,homebutton = F)
  }) %>%
  reduce(`+`)

od_agg_map = temp_agg %>%
  filter(ALAND10 > 0) %>%
  mutate(area_mi2 = round(ALAND10/2589988, 2)
         ,count_density = round(count/area_mi2, 2)) %>%
  mapview(zcol = "count"
          ,color = "black"
          ,layer.name  = str_glue("All (trips per mi2)")
          ,homebutton = F)

temp_od = (od_agg_map + od_map) %>%
  .@map %>%
  leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                       ,"MEDIUM Duty (trips per mi2)"))

htmlwidgets::saveWidget(temp_od
                        ,here(location, folder,  "viz", "freight_origins_map.html"))

export_agg_links(
  data = list(
    "origin_poly_trips" = temp_agg
    ,"origin_poly_trips_by_veh" = temp
  )
  ,location = location
  ,folder = folder
)


##static link maps==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static_vis_anlt = viz_static_ntwrk_map_anlt(
  spatial_agg_object = aggregated_network_links
  ,return_leaflet = T
)

static_vis_anltpt = viz_static_ntwrk_map_anltpt(
  spatial_agg_object = aggregated_network_links
  ,return_leaflet = T
)

static_vis_anlto = viz_static_ntwrk_map_anlto_grp(
  spatial_agg_object = aggregated_network_links
  ,poi_list = poi_list
  ,return_leaflet = T
)


#NOTE::
#need to all be leaflet objects
#need to make vix folder before you write out
list(
  list(static_vis_anlt, static_vis_anltpt, static_vis_anlto
  )
  ,list("static_vis_anlt", "static_vis_anltpt", "static_vis_anlto"
  )
) %>%
  pmap(~{
    htmlwidgets::saveWidget(.x
                            ,here(location, folder, "viz", str_glue("{.y}.html")))
  })

# htmlwidgets::saveWidget(static_vis_anlto@map
#                         ,here(location, folder, "viz", str_glue("static_vis_anlto.html")))


##dynamic HTML widget maps======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynamic_map_anlt = make_network_map_anlt(
  network_cntrd_object = aggregated_network_cntds
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)

dynamic_map_anltpt = make_network_map_anltpt(
  network_cntrd_object = aggregated_network_cntds
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)

dynamic_map_anlto = make_network_map_anlto(
  network_cntrd_object = aggregated_network_cntds
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)


list(
  c(dynamic_map_anlt, dynamic_map_anltpt, dynamic_map_anlto
  )
  ,c("dynamic_map_anlt", "dynamic_map_anltpt", "dynamic_map_anlto"
  )
) %>%
  pmap(~{
    htmlwidgets::saveWidget(.x, here(location, folder, "viz", "{.y}.html"))
  })


list(
  list(dynamic_map_anlt, dynamic_map_anltpt, dynamic_map_anlto)
  ,list("dynamic_map_anlt", "dynamic_map_anltpt", "dynamic_map_anlto")
) %>%
  pmap(~{
    print(head(.x))
  })


htmlwidgets::saveWidget(dynamic_map_anlt, here(location, folder, "viz", "dynamic_map_anlt.html"))


replicaToolkitR::check_origin_counts(
  location = location
  ,folder = folder
)





