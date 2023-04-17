









poi_list = fread(
  header = T, skip = 0, here(location, "poi_list.csv")) %>%
  janitor::remove_empty("rows")

replica_queried_network_links = read_sf(
  here(location, folder ,"replica_queried_network_links.gpkg"))

aggregated_network_links = read_rds(
  here(location, folder, "aggregated_network_links.rds"))

acquired_sa_polys = read_sf(here(location,folder,"acquired_sa_polys.gpkg"))

replica_trip_origin_links = read_sf(
  here(location, folder, "replica_trip_origin_links.gpkg"))

table_agg_by_link_subset_limited = fread(
  here(location, folder, "table_agg_by_link_subset_limited.csv"))



data_temp$agg_link_vehicle_type_origin %>%
  filter(origin_poly %in% c('261635240001', '261635249001')) %>%
  group_by(vehicle_type, origin_poly) %>%
  nest() %>%
  mutate(data = map(data, ~arrange(.x, desc(count)) %>%  head() )) %>%
  unnest(cols = c(data)) %>%
  select(vehicle_type, origin_poly, streetName, count) %>%
  ungroup() %>%
  View()








