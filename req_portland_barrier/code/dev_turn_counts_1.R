
yolo = bq_table_download("replica-customer._d48ded622e745d9120443120b79c81a0aee797b2.anonev_ozHKl0xWCynGNweZLdwTVfdZW3mBB5M_Ywzlvz1dFZQ")

turn_counts = data.table(
  stringsAsFactors = FALSE,
              mode = c("PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                       "PRIVATE_AUTO","PRIVATE_AUTO"),
            link_1 = c(13681064813963915264,
                       13681064813963915264,8424031259136198656,17534896915849416704,
                       18211329695608406016,10043866682152163328,
                       8424031259136198656,17061392881397542912,13681064813963915264,
                       17534896915849416704,761096520003630464,10043866682152163328,
                       12876848107368568832,12876848107368568832,
                       17061392881397542912,18211329695608406016,18211329695608406016,
                       12876848107368568832,761096520003630464,
                       8424031259136198656,10043866682152163328,761096520003630464,
                       17061392881397542912,13681064813963915264,761096520003630464) %>%
    as.character(),
            link_2 = c(17061392881397542912,NA,
                       18211329695608406016,761096520003630464,8424031259136198656,
                       NA,13681064813963915264,13681064813963915264,
                       8424031259136198656,17061392881397542912,17534896915849416704,
                       8424031259136198656,NA,17534896915849416704,
                       10043866682152163328,12876848107368568832,761096520003630464,
                       18211329695608406016,13681064813963915264,
                       10043866682152163328,17061392881397542912,18211329695608406016,
                       17534896915849416704,761096520003630464,NA) %>%
    as.character(),
             count = c(533L,128L,1965L,57L,657L,
                       1L,636L,1718L,208L,18L,163L,596L,32L,1L,1028L,
                       65L,246L,233L,156L,1702L,324L,821L,39L,45L,2L)
) %>%
  mutate(across(starts_with("link_"), ~str_trunc(.x, 14, "right", "")))


turn_counts_2 = data.table::data.table(
        mode = c("PRIVATE_AUTO","PRIVATE_AUTO",
                 "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                 "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                 "PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO","PRIVATE_AUTO",
                 "PRIVATE_AUTO"),
      link_1 = c(8424031259136198656,761096520003630464,
                 8424031259136198656,17061392881397542912,12876848107368568832,
                 12876848107368568832,17061392881397542912,
                 10043866682152163328,761096520003630464,17061392881397542912,761096520003630464,
                 761096520003630464,12876848107368568832,8424031259136198656,
                 13681064813963915264),
      link_2 = c(13681064813963915264,NA,
                 10043866682152163328,13681064813963915264,NA,17534896915849416704,
                 10043866682152163328,NA,18211329695608406016,17534896915849416704,
                 17534896915849416704,13681064813963915264,18211329695608406016,
                 18211329695608406016,NA),
       count = c(844L,2L,2298L,2251L,32L,1L,1352L,1L,
                 1067L,57L,220L,201L,298L,2622L,128L)
) %>%
  mutate(across(starts_with("link_"), ~.x %>%
                  as.character %>%
                  str_trunc(14, "right", "")))

intersection = yolo %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  mutate(stableEdgeId = as.character(stableEdgeId) %>%
           str_trunc(14, "right", ""))

intersection %>%
  st_jitter(.00001) %>%
  mapview()

temp = turn_counts_2 %>%
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


temp %>%
  ungroup() %>%
  # .[1, 3] %>%
  unnest(cols = c("connected_links", 'data') ) %>%
  st_as_sf(wkt = "connected_links", crs = 4326) %>%
  mutate(lwd_count = gauntlet::rescale_to(count, 16)) %>%
  # st_jitter(.00001) %>%
  filter(link_1 == "17534896915849") %>%
  mapview(zcol = "count", lwd = "lwd_count")


temp %>%
  ungroup() %>%
  # .[1, 3] %>%
  unnest(cols = c("connected_links", 'data') ) %>%
  st_as_sf(wkt = "connected_links", crs = 4326) %>%
  mutate(lwd_count = gauntlet::rescale_to(count, 16)) %>%
  # st_jitter(.00001) %>%
  filter(link_1 == "84240312591361") %>%
  mapview(zcol = "count", lwd = "lwd_count")


intersection %>%
  filter(stableEdgeId == "13681064813963915264")




bq_job_meta("replica-customer.US.bquxjob_420838ad_186a9258796")


bq_job()


bolo = bq_table_download("replica-customer._d48ded622e745d9120443120b79c81a0aee797b2.anonbd050b46b467c5d852a73a9bb48687356313e983f9cd10db3bcad1694b4844dd")


bolo %>%
  arrange(activity_id, index) %>%
  group_by(activity_id) %>%
  mutate(test = lead(index)-index) %>%
  ungroup() %>%
  count(test)




















