network_table = "replica-customer.southwest.southwest_2021_Q4_network_segments"
trip_table = "replica-customer.southwest.southwest_2021_Q4_thursday_trip"
mode_type = c('COMMERCIAL')
generators = here::here("req_detroit/data/TopFreightGeneratorsV3") %>%
  read_sf() %>%
  mutate(Name = str_remove(Name, "\n"))
customer_name = "replica-customer"


location = 'req_austin/data'
folder = "data_20230602_000000"

study_area = here::here(location, "study_area_poly_20230418.gpkg") %>%
  read_sf() %>%
  mutate(Name = "study_area")

outgoing_objects = make_replica_query(
  index_od = c('origin_lng', 'origin_lat')
  ,poly_gen_att = study_area
  ,study_area = study_area
)

max_record = Inf

objects_od = list(
  outgoing_objects
  # ,incoming_objects
) %>%
  map(~.x[["table_od_poly"]] %>%
        bigrquery::bq_table_download(n_max = Inf)) %>%
  reduce(bind_rows)

objects_od %>%
  filter(vehicle_type != "MEDIUM_COMMERCIAL") %>%
  st_as_sf(coords = c("point_lng", "point_lat"), crs = 4326) %>%
  st_filter(study_area) %>%
  mapview(cex = "count", zcol = "count")

objects_net = list(
  outgoing_objects
  # ,incoming_objects
) %>%
  map_df(~.x[["table_ord_trip_links_sf"]] %>%
        bigrquery::bq_table_download(n_max = max_record
                                     ,page_size = 1000)
  )

(objects_net  %>%
    filter(vehicle_type != "MEDIUM_COMMERCIAL") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    mapview(zcol = "count", lwd = "count")
) + mapview(study_area)


#run query for both incoming and outgoing
list_incoming_outgoing = list(
  c('origin_lng', 'origin_lat')
  ,c('destination_lng', 'destination_lat')
) %>%
  map(~{
    make_replica_query(
      index_od = .x
      ,poly_gen_att = study_area
      ,study_area = study_area
    )
  })

data_od = list_incoming_outgoing %>%
  map_df(~.x[["table_od_poly"]] %>%
           bigrquery::bq_table_download(n_max = Inf)) %>%
  group_by(poly_name, mode, point_lng, point_lat) %>%
  mutate(ttl_location = sum(count)) %>%
  group_by(poly_name, mode, point_lng, point_lat, vehicle_type) %>%
  mutate(ttl_location_veh = sum(count)) %>%
  group_by(poly_name, mode, point_lng, point_lat, type) %>%
  mutate(ttl_location_io = sum(count)) %>%
  ungroup() %>%
  group_by(poly_name, mode, vehicle_type, type) %>%
  mutate(count_pr = gauntlet::dgt2(percent_rank(count))
         ,count_nrm_mmax = gauntlet::dgt2(normalize_min_max(count))
         ,radius = gauntlet::rescale_to(count, 20)) %>%
  ungroup() %>%
  mutate(across(c(vehicle_type, type), ~gsub("_.*", "\\1", .x) %>%
                  str_to_title())
         ,label = str_glue("{vehicle_type} - {type}
                           <br>Count: {count}
                           <br>Count (Min-Max norm.): {100*count_nrm_mmax}%
                           <br>Count (% rank): {100*count_pr}%")
         ,radius_all = gauntlet::rescale_to(count, 20))








# write.csv(data_od, here::here(location, folder, "gererator_attractor_locations.csv"))
# write_sf(data_od_sf, here::here(location, folder, "data", "gererator_attractor_locations.gpkg"))
# data_od_sf = read_sf(here::here(location, folder, "data", "gererator_attractor_locations.gpkg"))


# data_od_sf = data_od %>%
#   st_as_sf(coords = c("point_lng", "point_lat"), crs = 4326)

roads = mapedit::drawFeatures() %>%
  st_transform(4326)

# roads = roads %>%

data_od_sf = data_od_sf %>%
  st_filter(quick_buffer(roads, radius = 500))

pal_count = colorNumeric("viridis", data_od_sf$count)

data_od_sf_sd = SharedData$new(data_od_sf)

tmp_widget = bscols(
  widths = c(3, 9)
  ,list(
    crosstalk::filter_checkbox("net_sd_vehicle_type", "Choose Vehicle Type:"
                                ,data_od_sf_sd, ~vehicle_type, inline = T)
    ,crosstalk::filter_checkbox("net_type", "Choose Travel Type:"
                                ,data_od_sf_sd, ~type, inline = T)
    ,htmltools::HTML("Network Link Volume Filters") %>%  htmltools::strong()
    ,htmltools::hr()
    ,crosstalk::filter_slider("net_sd_count", "Freight Count (counts):"
                              ,data_od_sf_sd, ~count)
    ,crosstalk::filter_slider("net_count_nrm_mmax", "Freight Count (Min-Max Norm):"
                              ,data_od_sf_sd, ~count_nrm_mmax)
    ,crosstalk::filter_slider("net_count_nrm_prank", "Freight Count (%Rank):"
                              ,data_od_sf_sd, ~count_pr)
  )
  ,leaflet(height = 700) %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    # addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles(providers$Esri, group = "Esri") %>%
    leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Img") %>%
    leaflet::addProviderTiles(providers$CartoDB, group = "CartoDB") %>%

    addPolygons(data = study_area
                ,color = "black", opacity = .8, fillColor = "blue"
                ,fillOpacity  = .2,weight = 1, group = "Study Area") %>%
    leaflet::addCircleMarkers(data = data_od_sf_sd
                              ,fillColor = ~pal_count(data_od_sf$count)
                              ,radius = 5*log10(data_od_sf$count+1)
                              ,color = "black"
                              ,opacity = .8
                              ,fillOpacity  = .5
                              ,weight = 1
                              # ,radius = 5
                              ,group = "Freight Origins and Destinations"
                              ,label = data_od_sf$label %>%
                                purrr::map(htmltools::HTML)
    ) %>%
    leaflet::addLayersControl(
      .
      ,baseGroups = c("OSM (default)", "Esri", "Esri World Img", "CartoDB")
      ,overlayGroups = c("Study Area", "Freight Origins and Destinations")
      ,options = layersControlOptions(collapsed = T, sortLayers = F)) %>%
    hideGroup(c("Freight Origins and Destinations")) %>%
    leafem::addMouseCoordinates() %>%
    leaflet::addLegend(
      position = "bottomright"
      ,title = "Freight Origins and Destinations"
      ,group = "Freight Origins and Destinations"
      ,pal = pal_count
      ,opacity = 0.7
      ,values = data_od_sf$count)
)

tmp_widget

htmlwidgets::saveWidget(
  tmp_widget
  ,file = here::here("gen_att_fullmap_interactive_reduced.html"), selfcontained = T)





folder_temp = "manual_extract_austin_20230810"
write_sf(data_od_sf, here::here(location, folder_temp, "gererator_attractor_locations.gpkg"))
write.csv(data_od_sf, here::here(location, folder_temp, "gererator_attractor_locations.csv"))
write_sf(roads, here::here(location, folder_temp, "road_buffer.gpkg"))



















mapviewOptions(homebutton = F)

leaflet_extract_layer_names(tmp_map@map)

tmp_map = data_od_sf %>%
  mutate(layer_id = str_glue("{vehicle_type} - {type}")
         ,layer_id_1 = layer_id)  %>%
  group_by(layer_id_1) %>%
  group_map(~{
    tmp_layer_name = unique(.x$layer_id)

    mapview(.x
            ,cex = "count"
            ,zcol = "count"
            ,layer.name = str_glue("{tmp_layer_name} - Freight Count")
            # ,col.region = pal(.x$count))
    )
  }) %>%
      reduce(`+`)

tmp_map_comb = data_od_sf %>%
  select(vehicle_type, ttl_location_veh, geometry) %>%
  unique() %>%
  mutate(vehicle_type_1 = vehicle_type) %>%
  group_by(vehicle_type_1) %>%
  group_map(~{
    tmp_layer_name = unique(.x$vehicle_type) %>% gsub("_.*", "\\1", .) %>% str_to_title()
  mapview(.x
          ,cex = "ttl_location_veh"
          ,zcol = "ttl_location_veh"
          ,layer.name = str_glue("Comb Incoming Outgoing - {tmp_layer_name}"))
  }) %>%
  reduce(`+`)

 tmp_map_full =  mapview(study_area, layer.name = "Study Area") +
    tmp_map +
    tmp_map_comb

tmp_map_full@map %>%
   leaflet::hideGroup(leaflet_extract_layer_names(.,layers_to_keep = "Study Area")) %>%
   htmlwidgets::saveWidget(., here::here(location, folder, "viz", "gen_att_fullmap.html"))

leaflet_extract_layer_names <- function(map_object, layers_to_keep = character()) {
  map_object$x$calls %>%
    map(~ tryCatch(.x$args[[1]]$group, error = function(e) NA)) %>%
    unlist() %>%
    .[!is.na(.)] %>%
    setdiff(layers_to_keep)
}


library(d3scatter)

shared_iris <- SharedData$new(iris)
hw <- bscols(
  d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width="100%", height=300),
  d3scatter(shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width="100%", height=300)
)
htmltools::save_html(hw, "result.html")


