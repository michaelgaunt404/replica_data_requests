

replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
  data.table::fread()
replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)

replica_queried_network %>%
  filter(str_detect(streetName, "New York Avenue Northeast")) %>%
  mapview(zcol = "lanes")



temp = aggregated_network_links$agg_link %>%
  filter(str_detect(streetName, "Anacostia F|Minnesota Ave"))

(temp %>%
  st_centroid() %>%
    st_jitter(.00001) %>%
  mapview(zcol = "count")) + mapview(temp, zcol = "count", lwd = 10)

gauntlet::st_true_midpoint()

temp = aggregated_network_links$agg_link %>%
  filter(str_detect(streetName, "Anacostia F"))

dc_roads = tigris::primary_secondary_roads(state = 11) %>%
  st_transform(4326)

guide = dc_roads %>%
  filter(FULLNAME == "I- 295") %>%
  head(1)

guide %>%
  mapview(lwd = 10)


(temp %>%
    st_centroid() %>%
    st_jitter(.00001) %>%
    mapview(zcol = "count")) + mapview(temp, zcol = "count") + mapview(guide, lwd = 10)



guide %>%
  st_line_sample(ls, sample = 1)
  st_cast("POINT") %>%
  mapview()


  test = guide %>%
    st_transform(crs = 32618) %>%
    split_lines(., id = "LINEARID", max_length = 500) %>%  st_transform(4326)

  test %>%
    mapview(lwd = 12)

  (test %>%  quick_buffer(with = 32618, radius = 50) %>%
    mapview()) + (temp %>%
                    st_centroid() %>%
                    # st_jitter(.00001) %>%
                    mapview(zcol = "count"))


  link = test
  data = temp %>%
    st_centroid()

  function(link, data){
    link_buff = link %>%
      st_transform(crs = 32618) %>%
      st_buffer(dist = 50, endCapStyle = "FLAT") %>%
      st_transform(crs = 4326)

    link_buff


  }











































  split_lines <- function(input_lines, max_length, id = "ID") {
    geom_column <- attr(input_lines, "sf_column")

    input_crs <- sf::st_crs(input_lines)

    input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

    attr(input_lines[["geom_len"]], "units") <- NULL
    input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

    too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

    rm(input_lines) # just to control memory usage in case this is big.

    too_long <- mutate(too_long,
                       pieces = ceiling(geom_len / max_length),
                       piece_len = (geom_len / pieces),
                       fID = 1:nrow(too_long))

    split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]

    split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
      select(-geom_len, -pieces) %>%
      group_by(fID) %>%
      mutate(ideal_len = cumsum(piece_len)) %>%
      ungroup()

    coords <- data.frame(sf::st_coordinates(too_long[[geom_column]]))
    rm(too_long)

    coords <- rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))

    split_nodes <- group_by(coords, fID) %>%
      # First calculate cumulative length by feature.
      mutate(len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
      mutate(len = ifelse(is.na(len), 0, len)) %>%
      mutate(len = cumsum(len)) %>%
      # Now join nodes to split points -- this generates all combinations.
      left_join(select(split_points, fID, ideal_len, split_fID), by = "fID") %>%
      # Calculate the difference between node-wise distance and split-point distance.
      mutate(diff_len = abs(len - ideal_len)) %>%
      # regroup by the new split features.
      group_by(split_fID) %>%
      # filter out na then grab the min distance
      filter(!is.na(diff_len) & diff_len == min(diff_len)) %>%
      ungroup() %>%
      # Grab the start node for each geometry -- the end node of the geometry before it.
      mutate(start_nID = lag(nID),
             # need to move the start node one for new features.
             new_feature = fID - lag(fID, default = -1),
             start_nID = ifelse(new_feature == 1, start_nID + 1, start_nID)) %>%
      # Clean up the mess
      select(fID, split_fID, start_nID, stop_nID = nID, -diff_len, -ideal_len, -len, -X, -Y)

    split_nodes$start_nID[1] <- 1

    split_points <- left_join(split_points, select(split_nodes, split_fID, start_nID, stop_nID), by = "split_fID")

    new_line <- function(start_stop, coords) {
      sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
    }

    split_lines <- apply(as.matrix(split_points[c("start_nID", "stop_nID")]),
                         MARGIN = 1, FUN = new_line, coords = coords)

    split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

    return(split_lines)
  }

















