

turn_counts_3 = data.table::data.table(
  mode = c("PRIVATE_AUTO","PRIVATE_AUTO",
           "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
           "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
           "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
           "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO"),
  link_1 = c(8424031259136198656,13555745704114337792,
             17061392881397542912,761096520003630464,2795330410658213376,
             12876848107368568832,761096520003630464,761096520003630464,
             17061392881397542912,761096520003630464,17061392881397542912,
             12876848107368568832,12876848107368568832,
             13681064813963915264,10043866682152163328,8017584530681808896,
             8424031259136198656),
  link_2 = c(10043866682152163328,13749034025215238144,
             13681064813963915264,NA,8296332553237771264,
             18211329695608406016,13681064813963915264,17534896915849416704,
             10043866682152163328,18211329695608406016,17534896915849416704,NA,
             17534896915849416704,NA,NA,14524488763208370176,
             13681064813963915264),
  count = c(11L,20241L,2251L,2L,107L,298L,201L,
            220L,1352L,1067L,57L,32L,1L,128L,1L,14985L,1L)
) %>%
  mutate(across(starts_with("link_"), ~.x %>%
                  as.character %>%
                  str_trunc(14, "right", "")))


sum(turn_counts_3$count)
sum(turn_counts_2$count)
sum(turn_counts$count)

bq_table_upload()


single_car = data.table::data.table(
                             activity_id = c(4608095374466946048,
                                             4608095374466946048,
                                             4608095374466946048,4608095374466946048,
                                             4608095374466946048,4608095374466946048),
                                    mode = c("PRIVATE_AUTO",
                                             "PRIVATE_AUTO","PRIVATE_AUTO",
                                             "PRIVATE_AUTO","PRIVATE_AUTO",
                                             "PRIVATE_AUTO"),
               vehicle_type = c(NA, NA, NA, NA, NA, NA),
                             origin_bgrp = c(410050242004,
                                             410050242004,410050242004,410050242004,
                                             410050242004,410050242004),
                        destination_bgrp = c(410050224005,
                                             410050224005,410050224005,410050224005,
                                             410050224005,410050224005),
               network_link_ids_unnested = c(761096520003630464,
                                             17534896915849416704,
                                             2795330410658213376,17828194480971601920,
                                             2907131394716651520,10856843281024086016),
                      index = c(254L, 255L, 256L, 257L, 258L, 259L),
                  row_index = c(1L, 2L, 3L, 4L, 5L, 6L),
          intersection_name = c(778L, 778L, 784L, 784L, 784L, 784L)
             ) %>%
  mutate(across(starts_with("network_link_ids_unnested"), ~.x %>%
                  as.character %>%
                  str_trunc(14, "right", "")))


trajectory_single_car = big_network %>%
  merge(single_car,
        by.x = "stableEdgeId", by.y = "network_link_ids_unnested")

mapview(trajectory_single_car, zcol = "row_index")


temp = single_car %>%
  mutate(link_id = str_glue("{link_1}_{link_2}")) %>%
  group_by(link_id) %>%
  nest() %>%
  mutate(connected_links = map(
    data
    ,~{
      # .x[["link_1"]]
      intersection %>%
        filter(stableEdgeId %in% c(.x[["link_1"]], .x[["link_2"]])) %>%
        st_union() %>%
        wellknown::sf_convert()
    }))

temp %>%
  ungroup() %>%
  # .[1, 3] %>%
  unnest(cols = c("connected_links", 'data') ) %>%
  st_as_sf(wkt = "connected_links", crs = 4326) %>%
  mutate(lwd_count = gauntlet::rescale_to(count, 16)) %>%
  # st_jitter(.00001) %>%
  filter(link_2 == "17534896915849") %>%
  mapview(zcol = "count", lwd = "lwd_count")







big_network = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from
                     ({paste(str_glue('(select * from {network_links})'), collapse = ' union all ')});")
) %>%
  bigrquery::bq_table_download() %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  mutate(stableEdgeId = as.character(stableEdgeId) %>%
           str_trunc(14, "right", ""))

sf::write_sf(big_network, here::here("req_portland_barrier/data/turn_counts", "big_network.shp"))

big_network = sf::read_sf(here::here("req_portland_barrier/data/turn_counts", "big_network.shp"))

agg_turn_counts = data.table::fread(
  here::here("req_portland_barrier/data/turn_counts", "agg_turn_counts.csv")
) %>%
  mutate(across(c("first_link", "last_link"), ~.x %>%
                  as.character %>%
                  str_trunc(14, "right", "")))

first_last_link_pair_list = data.table::fread(
  here::here("req_portland_barrier/data/turn_counts", "first_last_link_pair_list.csv")
) %>%
  mutate(across(c("first_link", "last_link", "network_link_ids_unnested"), ~.x %>%
                  as.character %>%
                  str_trunc(14, "right", "")))

temp = first_last_link_pair_list %>%
  group_by(first_link, last_link) %>%
  nest() %>%
  mutate(connected_links = map(
    data
    ,~{
      big_network %>%
        filter(stableEdgeId %in% c(.x[["network_link_ids_unnested"]])) %>%
        st_union() %>%
        wellknown::sf_convert()
    })) %>%
  ungroup() %>%
  unnest(cols = c("connected_links") ) %>%
  select(!data) %>%
  st_as_sf(wkt = "connected_links", crs = 4326) %>%
  merge(agg_turn_counts
        ,by = c("first_link", "last_link")) %>%
  mutate(lwd_count = gauntlet::rescale_to(count, 16) %>%
           dgt2()) %>%
  merge(
    big_network %>%
      st_drop_geometry() %>%
      select(stableEdgeId,first_link_name =  streetName, first_link_type = highway)
    ,by.x = "first_link", by.y = "stableEdgeId"
  ) %>%
  merge(
    big_network %>%
      st_drop_geometry() %>%
      select(stableEdgeId,last_link_name =  streetName, last_link_type = highway)
    ,by.x = "last_link", by.y = "stableEdgeId"
  ) %>%
  arrange(first_link_name, last_link_name) %>%
  select(starts_with("first")
         ,starts_with("last")
         ,everything()) %>%
  mutate(link_index = str_glue("{first_link_name} - {first_link}"))

combined_map = temp$link_index %>%
  unique() %>%
  sort() %>%
  map(~{
    temp %>%
      filter(link_index == .x) %>%
      mapview(zcol = "count"
              ,lwd = "lwd_count"
              ,layer.name = .x
              ,homebutton = F)
  }) %>%
  reduce(`+`)

combined_map_leaflet = combined_map@map %>%
  leaflet::hideGroup(
    c(
      "14th Street - 76109652000363", "Main Street - 12876848107368"
      ,"Main Street - 17061392881397", "McLoughlin Boulevard - 13555745704114"
      ,"McLoughlin Boulevard - 80175845306818"
    )
  )

combined_map_leaflet

htmlwidgets::saveWidget(
  combined_map_leaflet
  ,file = here::here("req_portland_barrier/data/turn_counts", "oregon_city_turn_counts.html")
)















