library(crosstalk)
library(data.table)
library(dplyr)
library(tidyverse)
library(wellknown)
library(mapedit)
library(mapview)
library(sf)




##inputs========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
mode_type = c('PRIVATE_AUTO')
customer_name = "replica-customer"

##actual_function===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
jitter = .003

#put a message here that prompts user that they will select a study area

study_area = mapedit::drawFeatures() %>%  st_transform(4326)

study_area_wkt = wellknown::sf_convert(study_area)

table_network = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{study_area_wkt}')
,geometry) as flag_contains
from `{network_table}`
)
where flag_contains = TRUE"))

table_network_count = bigrquery::bq_table_nrow(table_network)

#put a prompt here tells the user how big the table is and if they want to proceed with downloading the data Y or N input that is case agnostic

table_network_data = bigrquery::bq_table_download(table_network) %>%
  arrange(stableEdgeId )

table_network_data_sf =  table_network_data %>%
  st_as_sf(wkt = "geometry", crs = 4326)

table_network_data_sf_jit = table_network_data_sf %>%
  st_jitter(factor = jitter)

#put a message here that the user will now see the map they downloaded but the data has been jittered by a factor of jitter

mapview(table_network_data_sf_jit)

#put a prompt here if they wish to provide a different jitter or if they are happy with it

link_selections = list(
  intersection = c("int_15th_to_onramp205"
                   ,"int_15th_to_99E"
                   ,"into_99E_14th_main"
                   ,"int_main_14th_99E")
  ,index = c(2, 2, 3, 3)
) %>%
  pmap(~{
    print(str_glue("Current intersection selection: {.x}"))

    tmp_list = list()
    for (i in 1:.y){
      print(str_glue("Select: seq_{i}"))
      tmp_list[[str_glue("seq_{i}")]] = table_network_data_sf_jit %>%
        mapedit::selectFeatures() %>%
        pull(stableEdgeId)
      Sys.sleep(2)
    }

    tmp_list_named = setNames(list(tmp_list), .x)

    return(tmp_list_named)
  })


flatten_link_list(link_selections)

link_selections_index = link_selections %>%
  flatten() %>%
  flatten() %>%
  flatten() %>%
  unlist() %>%
  reduce(c) %>%
  sort() %>%  unique()



link_selections_index_pro = paste0("'", link_selections_index, "'", collapse = ", ")

table = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from
(select * except(network_link_ids)
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS link_ord
from `{trip_table}`, unnest(network_link_ids) as network_links
where 1=1
--and activity_id in ('17812870136189189137')
and mode in ('PRIVATE_AUTO')
)
where 1 = 1
and network_links in ('11056177580356907149', '17719514884527908600');")
)
bigrquery::bq_table_nrow(table)


#


11056177580356907149
11056177580356907149

flatten_list(link_selections %>%
               flatten())

my_list(flatten_list)

# Function to flatten list and extract names and values
flatten_list <- function(lst, parent_name = "") {
  # If input is not a list, return a data frame with the single value
  if (!is.list(lst)) {
    return(data.frame(name = parent_name, value = lst))
  }
  # If input is a list, recursively flatten its elements and combine into a data frame
  else {
    child_dfs <- lapply(names(lst), function(name) {
      child_name <- if (parent_name == "") name else paste0(parent_name, ".", name)
      flatten_list(lst[[name]], child_name)
    })
    do.call(rbind, child_dfs)
  }
}

# Example list with nested elements
my_list <- list(
  a = 1,
  b = list(
    c = 2,
    d = list(
      e = 3,
      f = 4
    )
  )
)

# Flatten list and create data frame
my_df <- flatten_list(my_list)

# View data frame
my_df






















































replicaToolkitR::query_network_trip_using_bbox()



mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
study_area_wkt = wellknown::sf_convert(study_area)
generators_wkt = wellknown::sf_convert(st_union(generators))


index = c('origin_lng', 'origin_lat')
type = "outgoing"

index = c('destination_lng', 'destination_lat')
type = "incoming"



generators = generators %>%  head(2)
x = generators$Name[1]
y = wellknown::sf_convert(generators)[1]


make_replica_query = function(index, type){
  index_pro = paste0(index, collapse = ", ")

  table_trip_custom_poly_list =
    list(
      generators$Name
      ,wellknown::sf_convert(generators)
    ) %>%
    pmap(~{

      print(str_glue('Processing:  {.x}'))
      tmp_tbale = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select *, '{.x}' as poly_name from (
select *,
ST_CONTAINS(
ST_GEOGFROMTEXT('{.y}')
,ST_GeogPoint({index_pro})) as flag_contains
from `{trip_table}`
where mode in ({mode_type_pro})
)
where flag_contains = TRUE"))

  print(str_glue('There are {bigrquery::bq_table_nrow(tmp_tbale)} rows in subset...'))

  return(tmp_tbale)
    })

table_trip_custom_poly_list_comb = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue('{paste0("select * from `", unlist(map(table_trip_custom_poly_list, replica_temp_tbl_name)), "`",  collapse = " union all ")};'))

table_od_poly = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select poly_name, mode, vehicle_type
  ,{index[1]} as point_lng, {index[2]} as point_lat
  ,count(*) as count
  ,'{type}' as type
from `{replica_temp_tbl_name(table_trip_custom_poly_list_comb)}`
group by poly_name, mode, vehicle_type, {index_pro}"))

table_ord_trip_links = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select poly_name, mode, vehicle_type, network_link_ids_unnested, count(*) as count from
         (
        select activity_id, mode, vehicle_type, network_link_ids_unnested, poly_name
    from `{replica_temp_tbl_name(table_trip_custom_poly_list_comb)}`
,unnest(network_link_ids) as network_link_ids_unnested)
group by poly_name, mode, vehicle_type, network_link_ids_unnested;"))

table_ord_trip_links_sf = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select *
  from `{replica_temp_tbl_name(table_ord_trip_links)}` table_left
  left join `{network_table}` table_right
  on table_left.network_link_ids_unnested = table_right.stableEdgeId;"))

table_ord_trip_links_sf_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{study_area_wkt}')
,geometry) as flag_contains
from `{replica_temp_tbl_name(table_ord_trip_links_sf)}`
where mode in ({mode_type_pro})
)
where flag_contains = TRUE"))

return(
  list(
    table_od_poly = table_od_poly
    ,table_ord_trip_links_sf = table_ord_trip_links_sf
    ,table_ord_trip_links_sf_pro = table_ord_trip_links_sf_pro
  )
)

}

bigrquery::bq_table_nrow(table_ord_trip_links_sf)
bigrquery::bq_table_nrow(table_ord_trip_links_sf_pro)

outgoing_objects = make_replica_query(
  index = c('origin_lng', 'origin_lat')
  ,type = "outgoing"
)

incoming_objects = make_replica_query(
  index = c('destination_lng', 'destination_lat')
  ,type = "incoming"
)

max_record = Inf

objects_od = list(
  outgoing_objects, incoming_objects
) %>%
  map(~.x[["table_od_poly"]] %>%
        bigrquery::bq_table_download(n_max = max_record)) %>%
  reduce(bind_rows)

# objects_net = list(
#   outgoing_objects, incoming_objects
# ) %>%
#   map(~.x[["table_ord_trip_links_sf"]]) %>%
#         # bigrquery::bq_table_nrow())
#         bigrquery::bq_table_download(n_max = max_record))
#
# objects_od %>%
#   group_by(vehicle_type, type) %>%
#   summarise(count = sum(count))




# objects_od =
#   bind_rows(
#     here::here("req_rhode/data", "manual_outgoing_od.csv") %>%  fread()
#     ,here::here("req_rhode/data", "manual_incoming_od.csv") %>%  fread()
#
#   )

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

# file = "req_detroit/data/TopFreightGeneratorsV3/manual_outgoing.csv"
# col = "poly_name"

process_manual_custom_cluster = function(data, file = NULL, col, study_area_filter = NULL){

  if (!is.null(file)){
  temp_data = here::here(file) %>%
    fread()
  } else {
    temp_data = data
  }

  temp_data[[col]] = parse_character(temp_data[[col]])
  temp_data = temp_data %>%
    filter(!is.na(startLat))
    # mutate(id = row_number()
    #        ,id_floor = id %>%
    #          floor_divide(500))

  # test = temp_data %>%
  #   group_by(id) %>%
  #   group_map(~.x %>%
  #               # print()
  #               st_as_sf(wkt = "geometry", crs = 4326)
  #             )

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

#load_networks
{
  object_outgoing = process_manual_custom_cluster(
    file = "req_rhode/data/manual_outgoing_network.csv"
    ,col = "poly_name"
    ,study_area_filter = study_area
  )

  object_incoming = process_manual_custom_cluster(
    file = "req_rhode/data/manual_incoming_network.csv"
    ,col = "poly_name"
    ,study_area_filter = study_area
  )

  object_outgoing[[1]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_bypoly_outgoing.shp"))
  object_incoming[[1]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_bypoly_incoming.shp"))

  object_outgoing[[2]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_type_outgoing.shp"))
  object_incoming[[2]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_type_incoming.shp"))

  object_outgoing[[3]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_ttl_outgoing.shp"))
  object_incoming[[3]] %>%  write_sf(here::here("req_rhode/data/spatial/network_links_ttl_incoming.shp"))


}

#load_ods
{
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
  ,here::here("req_rhode/data/spatial/polys_ttl_incoming_outgoing.shp")
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
  ,here::here("req_rhode/data/spatial/polys_type_incoming_outgoing.shp")
)

objects_od_md_out = objects_od_ttl %>%  filter(type == "outgoing", vehicle_type == "MEDIUM_COMMERCIAL")
objects_od_md_in = objects_od_ttl %>%  filter(type != "outgoing", vehicle_type == "MEDIUM_COMMERCIAL")
objects_od_hv_out = objects_od_ttl %>%  filter(type == "outgoing", vehicle_type != "MEDIUM_COMMERCIAL")
objects_od_hv_in = objects_od_ttl %>%  filter(type != "outgoing", vehicle_type != "MEDIUM_COMMERCIAL")
}




map_1 = mapview(object_incoming[[3]], zcol = "count", lwd = "count", layer.name = "Incoming Freight") +
  mapview(object_outgoing[[3]], zcol = "count", lwd = "count", layer.name = "Outgoing Freight") +
  mapview(objects_od_ttl_out, zcol = "count", layer.name = "Freight Poly Counts - Outgoing") +
  mapview(objects_od_ttl_in, zcol = "count", layer.name = "Freight Poly Counts - Incoming")

map_1@map %>%
  leaflet::hideGroup(c("Incoming Freight", "Outgoing Freight")) %>%
  htmlwidgets::saveWidget(here::here("req_rhode/data", "anl_map.html"))

map_2_incoming =
  mapview(object_incoming[[2]][object_incoming[[2]]$vehicle_type == "HEAVY_COMMERCIAL",]
          ,lwd = "count", zcol = "count", layer.name = "Incoming - Heavy Duty") +
  mapview(object_incoming[[2]][object_incoming[[2]]$vehicle_type == "MEDIUM_COMMERCIAL",]
          ,lwd = "count", zcol = "count", layer.name = "Incoming - Medium Duty") +
  mapview(objects_od_md_in, zcol = "count", layer.name = "Freight Polys - Medium") +
  mapview(objects_od_hv_in, zcol = "count", layer.name = "Freight Polys - Heavy")


map_2_outgoing = mapview(object_outgoing[[2]][object_outgoing[[2]]$vehicle_type == "HEAVY_COMMERCIAL",]
                ,lwd = "count", zcol = "count", layer.name = "Outgoing - Heavy Duty") +
  mapview(object_outgoing[[2]][object_outgoing[[2]]$vehicle_type == "MEDIUM_COMMERCIAL",]
          ,lwd = "count", zcol = "count", layer.name = "Outgoing - Medium Duty") +
  mapview(objects_od_md_out, zcol = "count", layer.name = "Freight Polys - Medium") +
  mapview(objects_od_hv_out, zcol = "count", layer.name = "Freight Polys - Heavy")

map_2_incoming@map %>%
  leaflet::hideGroup(c("Incoming - Heavy Duty", "Incoming - Medium Duty")) %>%
  htmlwidgets::saveWidget(here::here("req_rhode/data", "anlt_incoming_map.html"))

map_2_outgoing@map %>%
  leaflet::hideGroup(c("Outgoing - Heavy Duty","Outgoing - Medium Duty")) %>%
  htmlwidgets::saveWidget(here::here("req_rhode/data", "anlt_outgoing_map.html"))




object = object_outgoing[[1]] %>%
  arrange(poly_name, vehicle_type) %>%
  mutate(poly_name = str_replace_all(poly_name, "/", "-"))

index = unique(object %>%  select(poly_name, vehicle_type) %>%  st_drop_geometry()) %>%
  mutate(layer_name = str_glue("{poly_name} - {vehicle_type}"))

temp_map = list(
  index$poly_name
  ,index$vehicle_type
  ,index$layer_name
) %>%
  pmap(function(x, y, z){
    object %>%
      filter(poly_name == x
             ,vehicle_type == y) %>%
      mapview(zcol = "count", lwd = "count", layer.name = str_glue("{z}"))

  }) %>%
  reduce(`+`)

temp_map@map %>%
leaflet::hideGroup(index$layer_name) %>%
  htmlwidgets::saveWidget(here::here("req_rhode/data", "anlto_outgoing_map.html"))

# object_outgoing[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_outgoing.shp"))
# object_incoming[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_incoming.shp"))
#
# object_outgoing[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_outgoing.shp"))
# object_incoming[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_incoming.shp"))
#
# object_outgoing[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_outgoing.shp"))
# object_incoming[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_incoming.shp"))


















