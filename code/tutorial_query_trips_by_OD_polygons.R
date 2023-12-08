#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script is intended on how to use the query_trips_by_OD_polygons() function
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

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#should be the same as *file_destination* input above

#this is the folder that the data is written to
#it is a sub-folder to *file_destination* and will have a data_[datetime] format

#query data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable


#need a study are to filter network links by
study_area = mapedit::drawFeatures() %>%
  st_transform(4326) %>%
  mutate(Name  = "Generic_study_area")
network_table = "replica-customer.great_lakes.great_lakes_2021_Q4_network_segments"
trip_table = "replica-customer.great_lakes.great_lakes_2021_Q4_thursday_trip"
mode_type = c('COMMERCIAL')
generators = here::here("req_detroit/data/TopFreightGeneratorsV3") %>%
  read_sf() %>%
  mutate(Name = str_remove(Name, "\n"))
customer_name = "replica-customer"
mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
index_od = c('origin_lng', 'origin_lat') #input to specify the way origin is spelled - think
poly_gen_att = generators
study_area = study_area

#this is the actual example of how to use the function
#NOTE: the output again will be two strings that link to the BQ tables that are made from this
outgoing_objects = query_trips_by_OD_polygons(
  index_od = c('origin_lng', 'origin_lat')
  ,poly_gen_att = study_area
  ,study_area = poly_gen_att
)

outgoing_objects_1 = query_trips_by_OD_polygons(
  index_od = c('origin_lng', 'origin_lat')
  ,poly_gen_att = generators
  ,study_area = poly_gen_att
)


#process the data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#note: the function has not been made to automate download yet
#----- still need this section here to acquire and then process

max_record = Inf

objects_od = list(
  outgoing_objects
  # ,incoming_objects
) %>%
  map(~.x[["table_od_poly"]] %>%
        bigrquery::bq_table_download(n_max = max_record)) %>%
  reduce(bind_rows)

objects_od %>%
  st_as_sf(coords = c("point_lng", "point_lat"), crs = 4326) %>%
  st_filter(study_area) %>%
  filter(vehicle_type  != "MEDIUM_COMMERCIAL") %>%
  mapview(cex = "count")

objects_net = list(
  outgoing_objects
  # ,incoming_objects
) %>%
  map(~.x[["table_ord_trip_links_sf"]] %>%
        bigrquery::bq_table_download(n_max = max_record)
  )


#bespoke process for actual project=============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#note: im including this here for an example
#----- everything below here was for an actual project


yolo_1 = bq_table_download(table_od_poly)
yolo_1_sf = st_as_sf(yolo_1, coords = c("point_lng", "point_lat"), crs = 4326)
yolo_1_sf %>% filter(vehicle_type != "MEDIUM_COMMERCIAL") %>% mapview(zcol = "count", cex = "count")

yolo_2 = bq_table_download(outgoing_objects_1$table_od_poly)
yolo_2_sf = st_as_sf(yolo_2, coords = c("point_lng", "point_lat"), crs = 4326)
((yolo_2_sf %>% filter(vehicle_type == "MEDIUM_COMMERCIAL") %>% mapview(zcol = "count", cex = "count")) + mapview(generators)) +
  (yolo_1_sf %>% filter(vehicle_type == "MEDIUM_COMMERCIAL") %>% mapview(zcol = "count", cex = "count", layer.name = "yolo_1"))



yolo = bq_table_download(table_ord_trip_links_sf_pro)

(#objects_net[[1]]
  yolo %>%
    filter(vehicle_type != "MEDIUM_COMMERCIAL") %>%
  # select(network_link_ids_unnested, highway, geometry) %>%
  unique() %>%
  # sample_frac(.5) %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  mapview(zcol = "count", lwd = "count")
  ) + mapview(study_area)


# objects_od %>%
#   group_by(vehicle_type, type) %>%
#   summarise(count = sum(count))




objects_od =
  bind_rows(
    here::here("req_detroit/data/custom_poly_effort_20230417", "manual_outgoing_od.csv") %>%  fread()
    ,here::here("req_detroit/data/custom_poly_effort_20230417", "manual_incoming_od.csv") %>%  fread()

  )

# objects_network_outgoing =
#   # bind_rows(
#     here::here("req_detroit/data/custom_poly_effort_20230417", "manual_outgoing_network.csv") %>%  fread()
#   # )

# objects_od %>%
#   filter(poly_name == "Fresh Pak")
#
# objects_od %>%
#   filter(type == "outgoing") %>%
#   # filter(vehicle_type == "MEDIUM_COMMERCIAL") %>%
  # st_as_sf(coords = c('point_lng', "point_lat"), crs = 4326) %>%
  # st_join(generators) %>%
  # st_drop_geometry() %>%
  # group_by(Name) %>%
  # summarise(count = sum(count)) %>%
  # merge(generators, ., by = "Name") %>%
#   mapview(zcol = "count")
#
#   mapview()

file = "req_detroit/data/TopFreightGeneratorsV3/manual_outgoing.csv"
col = "poly_name"

process_manual_custom_cluster = function(data, file = NULL, col, study_area_filter = NULL){

  if (!is.null(file)){
  temp_data = here::here(file) %>%
    fread()
  } else {
    temp_data = data
  }

  temp_data[[col]] = parse_character(temp_data[[col]])

  data_anlto_sf = temp_data %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  data_anlt_sf = temp_data %>%
    group_by(mode, vehicle_type, streetName, network_link_ids_unnested, highway, geometry) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  data_anl_sf = temp_data %>%
    group_by(streetName, network_link_ids_unnested, highway, geometry) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  list_agg_networks = list(
    data_anlto_sf
    ,data_anlt_sf
    ,data_anl_sf
  )

  if (!is.null(study_area_filter)){
    list_agg_networks = map(list_agg_networks, ~st_filter(.x, study_area_filter))
  }

  return(list_agg_networks)
}

object_outgoing = process_manual_custom_cluster(
  file = "req_detroit/data/custom_poly_effort_20230417/manual_outgoing_network.csv"
  ,col = "poly_name"
  ,study_area_filter = study_area
)

object_incoming = process_manual_custom_cluster(
  file = "req_detroit/data/custom_poly_effort_20230417/manual_incoming_network.csv"
  ,col = "poly_name"
  ,study_area_filter = study_area
)

objects_od_ttl = objects_od  %>%
  st_as_sf(coords = c('point_lng', "point_lat"), crs = 4326) %>%
  st_join(generators) %>%
  st_drop_geometry() %>%
  group_by(poly_name, type) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  merge(generators, ., by.x = "Name",  by.y = "poly_name")  %>%
  select(!starts_with("id"))

write_sf(
  objects_od_ttl
  ,here::here("req_detroit/data/custom_poly_effort_20230417/spatial/polys_ttl_incoming_outgoing.shp")
)

objects_od_ttl_out = objects_od_ttl %>%  filter(type == "outgoing")
objects_od_ttl_in = objects_od_ttl %>%  filter(type != "outgoing")

objects_od_ttl = objects_od  %>%
  st_as_sf(coords = c('point_lng', "point_lat"), crs = 4326) %>%
  st_join(generators) %>%
  st_drop_geometry() %>%
  group_by(poly_name, type, vehicle_type ) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  merge(generators, ., by.x = "Name",  by.y = "poly_name")  %>%
  select(!starts_with("id"))

write_sf(
  objects_od_ttl
  ,here::here("req_detroit/data/custom_poly_effort_20230417/spatial/polys_type_incoming_outgoing.shp")
)

objects_od_md_out = objects_od_ttl %>%  filter(type == "outgoing", vehicle_type == "MEDIUM_COMMERCIAL")
objects_od_md_in = objects_od_ttl %>%  filter(type != "outgoing", vehicle_type == "MEDIUM_COMMERCIAL")
objects_od_hv_out = objects_od_ttl %>%  filter(type == "outgoing", vehicle_type != "MEDIUM_COMMERCIAL")
objects_od_hv_in = objects_od_ttl %>%  filter(type != "outgoing", vehicle_type != "MEDIUM_COMMERCIAL")





# map_1 = mapview(object_incoming[[3]], zcol = "count", lwd = "count", layer.name = "Incoming Freight") +
#   mapview(object_outgoing[[3]], zcol = "count", lwd = "count", layer.name = "Outgoing Freight") +
#   mapview(objects_od_ttl_out, zcol = "count", layer.name = "Freight Poly Counts - Outgoing") +
#   mapview(objects_od_ttl_in, zcol = "count", layer.name = "Freight Poly Counts - Incoming")
#
# map_1@map %>%
#   leaflet::hideGroup(c("Incoming Freight", "Outgoing Freight")) %>%
# htmlwidgets::saveWidget(here::here("req_detroit/data/custom_poly_effort_20230417", "anl_map.html"))
#
# map_2_incoming =
#   mapview(object_incoming[[2]][object_incoming[[2]]$vehicle_type == "HEAVY_COMMERCIAL",]
#           ,lwd = "count", zcol = "count", layer.name = "Incoming - Heavy Duty") +
#   mapview(object_incoming[[2]][object_incoming[[2]]$vehicle_type == "MEDIUM_COMMERCIAL",]
#           ,lwd = "count", zcol = "count", layer.name = "Incoming - Medium Duty") +
#   mapview(objects_od_md_in, zcol = "count", layer.name = "Freight Polys - Medium") +
#   mapview(objects_od_hv_in, zcol = "count", layer.name = "Freight Polys - Heavy")
#
#
# map_2_outgoing = mapview(object_outgoing[[2]][object_outgoing[[2]]$vehicle_type == "HEAVY_COMMERCIAL",]
#                 ,lwd = "count", zcol = "count", layer.name = "Outgoing - Heavy Duty") +
#   mapview(object_outgoing[[2]][object_outgoing[[2]]$vehicle_type == "MEDIUM_COMMERCIAL",]
#           ,lwd = "count", zcol = "count", layer.name = "Outgoing - Medium Duty") +
#   mapview(objects_od_md_out, zcol = "count", layer.name = "Freight Polys - Medium") +
#   mapview(objects_od_hv_out, zcol = "count", layer.name = "Freight Polys - Heavy")
#
# map_2_incoming@map %>%
#   leaflet::hideGroup(c("Incoming - Heavy Duty", "Incoming - Medium Duty")) %>%
#   htmlwidgets::saveWidget(here::here("req_detroit/data/custom_poly_effort_20230417", "anlt_incoming_map.html"))
#
# map_2_outgoing@map %>%
  leaflet::hideGroup(c("Outgoing - Heavy Duty","Outgoing - Medium Duty")) %>%
  htmlwidgets::saveWidget(here::here("req_detroit/data/custom_poly_effort_20230417", "anlt_outgoing_map.html"))




object = object_outgoing[[1]] %>%
  arrange(poly_name, vehicle_type) %>%
  mutate(poly_name = str_replace_all(poly_name, "/", "-"))

index_od = unique(object %>%  select(poly_name, vehicle_type) %>%  st_drop_geometry()) %>%
  mutate(layer_name = str_glue("{poly_name} - {vehicle_type}"))

incoming_maps = list(
  index_od$poly_name
  ,index_od$vehicle_type
  ,index_od$layer_name
) %>%
  pmap(function(x, y, z){
    object %>%
      filter(poly_name == x
             ,vehicle_type == y) %>%
      mapview(zcol = "count", lwd = "count", layer.name = str_glue("{z}"))

  }) %>%
  reduce(`+`)

incoming_maps@map %>%
leaflet::hideGroup(index_od$layer_name) %>%
  htmlwidgets::saveWidget(here::here("req_detroit/data/custom_poly_effort_20230417", "anlto_outgoing_map.html"))

object_outgoing[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_outgoing.shp"))
object_incoming[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_incoming.shp"))

object_outgoing[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_outgoing.shp"))
object_incoming[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_incoming.shp"))

object_outgoing[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_outgoing.shp"))
object_incoming[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_incoming.shp"))


















