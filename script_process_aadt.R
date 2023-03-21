

project_locations_cntrd = project_locations_buffer %>%
  st_centroid() %>%
  mapview()

counties = tigris::counties(state = "OR")
counties_sel = counties %>%
  mapedit::selectFeatures()

truck_aadt_159 = arcpullr::get_spatial_layer(
  "https://gis.odot.state.or.us/arcgis1006/rest/services/transgis/catalog/MapServer/159"
)

truck_aadt_160 = arcpullr::get_spatial_layer(
  "https://gis.odot.state.or.us/arcgis1006/rest/services/transgis/catalog/MapServer/160"
)

(
truck_aadt_160 %>%
  st_filter(counties_sel) %>%
  mutate(lwd = gauntlet::rescale_to(TRK_AADT, 16)) %>%
  mapview(zcol = "TRK_AADT", lwd = "lwd")
) + mapview(project_locations_cntrd, layer.name = "Project Locations")

