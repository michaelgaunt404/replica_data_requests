


study_area = mapedit::drawFeatures() %>%  st_transform(4326)





network_table = "replica-customer.great_lakes.great_lakes_2021_Q4_network_segments"
trip_table = "replica-customer.great_lakes.great_lakes_2021_Q4_thursday_trip"
mode_type = c('COMMERCIAL')
generators = here::here("req_detroit/data/TopFreightGeneratorsV3") %>%
  read_sf() %>%
  mutate(Name = str_remove(Name, "\n"))
customer_name = "replica-customer"


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
    ,table_ord_trip_links_sf = table_ord_trip_links_sf_pro
  )
)

}

# bigrquery::bq_table_nrow(table_ord_trip_links_sf)
# bigrquery::bq_table_nrow(table_ord_trip_links_sf_pro)
#
# outgoing_objects = make_replica_query(
#   index = c('origin_lng', 'origin_lat')
#   ,type = "outgoing"
# )
#
# incoming_objects = make_replica_query(
#   index = c('destination_lng', 'destination_lat')
#   ,type = "incoming"
# )
#
# max_record = Inf

# objects_od = list(
#   outgoing_objects, incoming_objects
# ) %>%
#   map(~.x[["table_od_poly"]] %>%
#         bigrquery::bq_table_download(n_max = max_record)) %>%
#   reduce(bind_rows)
#
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

index = unique(object %>%  select(poly_name, vehicle_type) %>%  st_drop_geometry()) %>%
  mutate(layer_name = str_glue("{poly_name} - {vehicle_type}"))

incoming_maps = list(
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

incoming_maps@map %>%
leaflet::hideGroup(index$layer_name) %>%
  htmlwidgets::saveWidget(here::here("req_detroit/data/custom_poly_effort_20230417", "anlto_outgoing_map.html"))

object_outgoing[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_outgoing.shp"))
object_incoming[[1]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_bypoly_incoming.shp"))

object_outgoing[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_outgoing.shp"))
object_incoming[[2]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_type_incoming.shp"))

object_outgoing[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_outgoing.shp"))
object_incoming[[3]] %>%  write_sf(here::here("req_detroit/data/custom_poly_effort_20230417/spatial/network_links_ttl_incoming.shp"))


















