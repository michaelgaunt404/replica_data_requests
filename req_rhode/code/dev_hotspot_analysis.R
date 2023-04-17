

net_1 = here::here(
  location
  ,"study_area_20230227"
  ,"StudyArea.shp") %>%
  read_sf()



net_1 %>%
  st_union() %>%
  mapview()


net_pro = net_1 %>%
  st_union()

net_pro %>%
  quick_buffer(radius = 200) %>%
  mapview()


write_sf(
  net_pro %>%
    quick_buffer(radius = 200)
  ,here::here(
    location
    ,"network_poly_20230227.gpkg"))



net = here::here(
  location
  ,"network_poly.gpkg") %>%
  read_sf()



mapview(net) + mapview(net_1)






aggregated_network_links$agg_link %>%
  mutate(test = rescale_to(count, 16)) %>%
  mapview(zcol = "count", lwd = "test")



table_agg_by_link_subset_limited %>%  glimpse()
  filter(network_link_ids_unnested == "10011404240167071974")
count(network_link_ids_unnested )


table_agg_by_link_subset_limited_pro = table_agg_by_link_subset_limited %>%
  mutate(network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", "")) %>%
  group_by(mode, vehicle_type, origin_poly, flag_sa_origin, network_link_ids_unnested) %>%
  summarise(count = sum(count)) %>%
  ungroup()

yolo = table_agg_by_link_subset_limited_pro %>%
  merge(replica_queried_network_links, .
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested")



yolo %>%
  mutate(lwd = rescale_to(count, 16)) %>%
  mapview(zcol = "count", lwd = "lwd")




















library(here)
# library(tidyverse)
library(dplyr)
library(stringr)
library(magrittr)
library(purrr)

library(leaflet)
library(leaflet.extras2)
library(mapview)
library(sf)

library(SpatialKDE)
library(sfhotspot)

library(replicaToolkitR)
library(gauntlet) #https://github.com/michaelgaunt404/gauntlet








replica_trip_origin_links = read_sf(
  here(location, folder ,"replica_trip_origin_links.gpkg"))

replica_trip_origin_links_fltrd = replica_trip_origin_links %>%
  st_filter(acquired_sa_polys) %>%
  st_transform(crs = 32618)

grid_sm = create_grid_hexagonal(
  replica_trip_origin_links_fltrd
  ,cell_size = 200)

grid_sm_water_removed = tigris::erase_water(grid_sm) %>%
  st_cast("POLYGON")

hotspot_object = list(
  list("MEDIUM_COMMERCIAL", "HEAVY_COMMERCIAL")
  ,list(grid_sm, grid_sm)
) %>%
  pmap(~{
    temp_hotspot = replica_trip_origin_links_fltrd %>%
      filter(vehicle_type == .x) %>%
      hotspot_kde(
        data = .
        ,grid = .y
        ,weights = count
        ,bandwidth = 800) %>%
      mutate(kde_norm = gauntlet::normalize_min_max(kde)) %>%
      mutate(label = str_glue("KDE (Min-Max Norm): {dgt2(kde_norm)}<br>KDE (raw): {dgt2(kde)}<br>TTL Trips: {sum}")) %>%
      st_transform(crs = 4326)
  })

names(hotspot_object) = c("KDE - Med. Duty", "KDE - Heavy Duty")


(hotspot_object[[2]]%>%
  mapview(zcol = "kde")) + mapview(generators)


generators = here(location, "SW_Freight_Generators_Origins_Review.csv") %>%
  read.csv() %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  mutate(count = 1)

maps_kde = names(hotspot_object) %>%
  map(~{
    hotspot_object[[.x]] %>%
        mapview(zcol = "kde", layer.name = .x, homebutton = F)}) %>%
  reduce(`+`)

map_generators = (mapview(generators, layer.name = "Freight Generators", label = "name", homebutton = F) + maps_kde) %>%
  .@map %>%
  leaflet::hideGroup(c("KDE - Med. Duty", "KDE - Heavy Duty"))


map_generators %>%
  htmlwidgets::saveWidget(
    here(location, folder,  "output/viz", "map_freight_generators.html"))





