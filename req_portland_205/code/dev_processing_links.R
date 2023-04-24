
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
mapviewOptions(homebutton = F)


robust_prompt_used = function(prompt) {
  message(str_glue("Do you want to {prompt}? Y/N"))
  response <- toupper(readline())

  if (response == "Y") {
    message(str_glue('You elected to {prompt}...'))
    return(response == "Y")
  } else if (response == "N") {
    message(str_glue('You did not elect to {prompt}...'))
    return(response == "Y")
  } else {
    message("Invalid response. Please enter Y or N.")
    robust_prompt_used(prompt = prompt)
  }
}

prompt_jitter_factor <- function() {
  while(TRUE) {
    jitter <- as.numeric(readline(prompt = "Provide jitter value: "))
    if(!is.na(jitter) && is.numeric(jitter) && jitter >= 0.00001 && jitter <= 0.01) {
      return(jitter)
    }
    else {
      cat("Invalid input. Jitter value must be a number between 0.00001 and 0.01.\n")
    }
  }
}


flatten_named_list <- function(lst, parent_name = "") {
  # If input is not a list, return a data frame with the single value
  if (!is.list(lst)) {
    return(data.frame(name = parent_name, value = lst))
  }
  # If input is a list, recursively flatten its elements and combine into a data frame
  else {
    child_dfs <- lapply(names(lst), function(name) {
      child_name <- if (parent_name == "") name else paste0(parent_name, ".", name)
      flatten_named_list(lst[[name]], child_name)
    })
    do.call(rbind, child_dfs)
  }
}

index_network_name_strip = c("Street", "Road", "Boulevard") %>%
  paste0(collapse = "|")


merge_cols <- function(data) {
  tmp_colnames = colnames(select(data, starts_with("seq_")))

  for (colname in tmp_colnames){
    data = merge(data
                 ,table_network_data_simp
                 ,by.x = colname, by.y = "stableEdgeId") %>%
      rename("{colname}_streetName" := "streetName")
  }

  tmp_colnames = colnames(select(data, ends_with("streetName")))

  tmp_func = paste0("{", tmp_colnames, "}", collapse = "_") %>%
    paste0("str_glue('", ., "')")

  tmp_func_fl = paste0("{", tmp_colnames[c(1,length(tmp_colnames))], "}", collapse = "_") %>%
    paste0("str_glue('", ., "')")

  data = data %>%
    mutate(mvmnt_desc = !!rlang::parse_expr(tmp_func)
           ,mvmnt_desc_fl = !!rlang::parse_expr(tmp_func_fl))

  return(data)
}


bind_cols_prefix = function(data, prefix = "seq_"){

  tmp_colnames = colnames(select(data, starts_with(prefix))) %>%
    gsub("_link.*", "\\1", .) %>%
    unique() %>%
    sort()

  tmp_object = tmp_colnames %>%
    map_df(~{
      tmp = data %>%
        select(mode, starts_with("mvmnt_desc"), starts_with(.x), count)  %>%
        mutate(order = parse_number(.x))

      colnames(tmp) = colnames(tmp) %>%
        gsub(".*(streetName)", "\\1", .) %>%
        gsub(".*(link_id)", "\\1", .)

      return(tmp)
    })

  return(tmp_object)
}


view_replica_network = function(network_table
                                ,customer_name){
  message("Please draw a study area that will be used to query Replica's roadway network...")
  message("Draw it as small and parsimonious as possible")
  message("You can draw mulitple, discrete objects if you wish")

  study_area = mapedit::drawFeatures() %>% st_transform(4326)
  study_area_wkt = wellknown::sf_convert(st_union(study_area))

  table_network = bigrquery::bq_project_query(
    customer_name,
    stringr::str_glue("select * from (
                      select *,
                      ST_INTERSECTS(
                      ST_GEOGFROMTEXT('{study_area_wkt}')
                      ,geometry) as flag_contains
                      from `{network_table}`
                      )
                      where flag_contains = TRUE"))

  table_network_count = bigrquery::bq_table_nrow(table_network)

  message(str_glue("{table_network_count} links were returned... \nwould you like to continue and download or abort and try again"))
  check_continue = robust_prompt_used("continue")
  stopifnot("Aborted" = check_continue)

  table_network_data = bigrquery::bq_table_download(table_network) %>%
    arrange(stableEdgeId)

  return(table_network_data)
}

mvmnt_df = data.frame(
  mvmnt_desc = c("int_15th_to_onramp205"
                 ,"int_15th_to_99E"
                 ,"into_99E_14th_main"
                 ,"int_main_14th_99E")
  ,ttl_seq = c(2, 2, 3, 3)
)

make_space_2 = function(with = "+", n = 50, c = "", last = T){
  if (last){
    paste0(rep(with, n), collapse = c) %>% paste0(., "\n")
  } else {
    paste0(rep(with, n), collapse = c) %>% paste0("\n", .)
  }
}

# network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
# trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
# mode_type = c('PRIVATE_AUTO')
# customer_name = "replica-customer"
# jitter_factor = 0.003



#' Download and visualize a map of the selected study area
#'
#' @param network_table Name of the network table in the bigquery project
#' @param trip_table Name of the trip table in the bigquery project
#' @param mode_type Character vector specifying the mode types to include in the analysis
#' @param customer_name Name of the bigquery project
#' @param jitter_factor A factor that determines how much the network data is jittered to improve visualization
#' @param mvmnt_df A dataframe containing movements to get data for
#' @return A mapview object showing the network data in the selected study area
#'
#' @examples
download_and_visualize_map("replica-customer.northwest.northwest_2021_Q4_network_segments",
                           "replica-customer.northwest.northwest_2021_Q4_thursday_trip",
                           c('PRIVATE_AUTO'),
                           "replica-customer",
                           0.003, mvmnt_df
)
download_and_visualize_map <- function(network_table, trip_table, mode_type, customer_name, jitter_factor) {
  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

  table_network_data = view_replica_network(
    network_table = network_table
    ,customer_name = customer_name)

  table_network_data_sf =  table_network_data %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  message(str_glue("The network you queried will be displayed with a {jitter_factor} jitter factor..."))

  table_network_data_sf_jit = table_network_data_sf %>%
    st_jitter(factor = jitter_factor)

  mapview(table_network_data_sf_jit, zcol = "flags", burst = T)

  rejitter <- TRUE
  while (rejitter) {
    jitter_decision = robust_prompt_used("change the jitter factor and reijitter")

    if (tolower(jitter_decision) == "t") {
      jitter_factor = prompt_jitter_factor()
      table_network_data_sf_jit = table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      mapview(table_network_data_sf_jit)
    }
  }


  #sec: get and process links
  {
    link_selections = list(
      mvmnt_df$mvmnt_desc
      ,mvmnt_df$ttl_seq
    ) %>%
      pmap(~{
        print(str_glue("{make_space_2()}Select links for: {.x}"))
        tmp_list = list()

        for (i in 1:.y){
          print(str_glue("Links for {i} movement..."))
          tmp_list[[str_glue("seq_{i}")]] = table_network_data_sf_jit %>%
            mapedit::selectFeatures() %>%
            pull(stableEdgeId)
          Sys.sleep(2)
        }

        tmp_list_named = setNames(list(tmp_list), .x)

        return(tmp_list_named)
      })

    #manual functions
    {
      #manual save out and read in
      # link_selections %>%  saveRDS(here::here("req_portland_205/data", 'link_selections.rds'))
      # link_selections = readRDS(here::here("req_portland_205/data", 'link_selections.rds'))
      }

    link_selections_df = link_selections %>%
      flatten() %>%
      flatten_named_list() %>%
      tidyr::separate(col = "name", into = c("intersection", "sequence"), sep = "\\.")

    link_selections_index_pro = paste0("'", sort(unique(link_selections_df$value)), "'", collapse = ", ")


    table_network_data_sf %>%
      filter(stableEdgeId %in% unique(link_selections_df$value)) %>%
      st_as_sf(wkt = "geometry", crs = 4326) %>%
      mapview()

    review_map = unique(link_selections_df$intersection) %>%
      map(~{
        link_selections_df %>%
          filter(intersection == .x) %>%
          merge(table_network_data_sf, .
                ,by.x = "stableEdgeId", by.y = "value") %>%
          mapview(zcol = "sequence", layer.name = .x
                  # ,label = "label"
                  ,color = hcl.colors(5, palette = "viridis")
          )
      }) %>%
      reduce(`+`)

    review_map

    #need to have a check here if we want to reselect one of them by name

  }

  #sec: table query
  {
    #should be messages about what is going in

    table = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue("select * from
(select * except(network_link_ids)
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS link_ord
from `{trip_table}`, unnest(network_link_ids) as network_links
where 1=1
and mode in ('PRIVATE_AUTO')
)
where 1 = 1
and network_links in ({link_selections_index_pro});"))

bigrquery::bq_table_nrow(table)

table_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select
activity_id,mode, network_links,vehicle_type, link_ord
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS seq_ord
,count(*)
    OVER (PARTITION BY activity_id) AS act_link_count
from {replica_temp_tbl_name(table)}
order by activity_id, link_ord;"))

turning_links = bigrquery::bq_table_download(table_pro
                                             ,page_size = 1000,
                                             quiet = F)
  }

  #sec: perform QC and processing operation
  {
  processed_mvmnt_links = unique(link_selections_df$intersection) %>%
    map_df(~{
      link_sub = link_selections_df %>%
        filter(intersection == x)

      index_max_seq = max(parse_number(link_sub$sequence))

      tl_sub = tl %>%
        filter(network_links %in% link_sub$value) %>%
        arrange(activity_id, seq_ord ) %>%
        group_by(activity_id) %>%
        mutate(seq_ord_rltv = row_number()) %>%
        ungroup() %>%
        group_by(activity_id) %>%
        filter(n() == index_max_seq) %>%
        ungroup()

      index_order_checks = c(1:index_max_seq) %>%
        c(1:2) %>%
        map(~{
          tl_sub %>%
            filter(seq_ord_rltv == .x) %>%
            filter(network_links %in% (link_sub %>%
                                         filter(parse_number(sequence) == .x) %>%
                                         pull(value))) %>%
            pull(activity_id)
        })

      index_activity = index_order_checks[[1]]

      for (i in 1:(index_max_seq-1)){
        index_activity = intersect(index_activity, index_order_checks[[i+1]])
      }

      tl_sub_pro = tl_sub %>%
        filter(activity_id %in% index_activity) %>%
        group_by(activity_id) %>%
        mutate(seq_ord = str_glue("seq_{row_number()}_link_id"))

      tmp_tl = tl_sub_pro %>%
        arrange(activity_id, seq_ord) %>%
        select(activity_id, mode, network_links, seq_ord)

      index_seq = unique(tmp_tl$seq_ord) %>%
        sort()

      tmp_tl_agg = tmp_tl %>%
        pivot_wider(names_from = "seq_ord", values_from = "network_links") %>%
        mutate(count = 1) %>%
        group_by(mode, across(starts_with("seq_"))) %>%
        summarise(count = sum(count)) %>%
        ungroup()

      tmp_tl_agg_comb_sf = tmp_tl_agg %>%
        merge_cols() %>%
        bind_cols_prefix() %>%
        merge(table_network_data %>%
                select(stableEdgeId, geometry), .
              ,by.x = "stableEdgeId", by.y = "link_id") %>%
        mutate(label = str_glue("<b>{mvmnt_desc_fl}</b><br>Full Link List: {mvmnt_desc}<br>Count: {count}<br>Street Name: {streetName}<br>Link Order: {order}")
               ,order = as.factor(order)) %>%
        mutate(glag_grp = str_glue("{mvmnt_desc}_{count}")) %>%
        st_as_sf(wkt = "geometry", crs = 4326) %>%
        arrange(mvmnt_desc, count, order)
    }, .progress = "Perfroming intersection quality checks")
  }

  return(processed_mvmnt_links)
}








map = unique(yolo_2$glag_grp) %>%
  map(~{
    yolo_2 %>%
      filter(glag_grp  == .x) %>%
      mutate(order = as.factor(order)) %>%
      mapview(zcol = "order", layer.name = .x
              ,label = "label"
              ,color = hcl.colors(5, palette = "viridis")
      )
  }) %>%
  reduce(`+`)






# items = turning_links %>%
#   filter(network_links == "8017584530681808495"
#          ,seq_ord == 1) %>%  pull(activity_id)
#
# item_1 = turning_links %>%
#   filter(network_links == "8017584530681808495"
#          ,seq_ord == 1) %>%  pull(activity_id)
#
# item_2 = turning_links %>%
#   filter((network_links == "16808743366416082416" |
#             network_links == "2795330410658213329")
#          ,seq_ord == 2) %>%  pull(activity_id)
#
# intersect(items, item_2)
#
# here::here("req_portland_205/data", 'turning_links_20230419.csv') %>%
#   write_csv(turning_links, .)








tl = here::here("req_portland_205/data", 'turning_links_20230419.csv') %>%
  fread() %>%
  mutate(seq_ord = str_glue("seq_{seq_ord}_link_id"))

# table_network_data %>%  saveRDS(here::here("req_portland_205/data", 'table_network_data.rds'))

# table_network_data = readRDS(here::here("req_portland_205/data", 'table_network_data.rds'))

table_network_data_simp = table_network_data %>%
  select(stableEdgeId, streetName) %>%
  mutate(streetName = replace_na(streetName, "NoName") %>%
           str_remove_all(index_network_name_strip) %>%
           str_trim())

table_network_data %>%
  filter(stableEdgeId == "8017584530681808495")

link_sub %>%
  filter(value == "8017584530681808495")

tl %>%
  filter(network_links == "8017584530681808495")

tl_sub %>%
  filter(network_links == "8017584530681808495") %>%
  count(network_links, act_link_count, seq_ord_rltv)

tl_sub %>%
  filter(network_links == "8017584530681808495") %>%
  filter(act_link_count == 2)

tl_sub %>%
  filter(activity_id == "12043381266702287857" )

tl_sub %>%
  ungroup() %>%
  filter(network_links == "8017584530681808495"
         ,seq_ord_rltv == 1
         ,act_link_count == 3) %>%
  pull(activity_id)
  arrange(activity_id, seq_ord_rltv) %>%
  head(1000) %>%  View()


x = unique(link_selections_df$intersection)[3]


tmp_tl_agg_comb_sf %>%
  mutate(order = as.factor(order)) %>%
  st_jitter(.0001) %>%
  mapview(zcol = "order"
          ,label = "label"
          ,color = hcl.colors(5, palette = "viridis"))


yolo_2 = yolo %>%
  mutate(glag_grp = str_glue("{mvmnt_desc}_{count}"))

map = unique(yolo_2$glag_grp) %>%
  map(~{
    yolo_2 %>%
      filter(glag_grp  == .x) %>%
      mutate(order = as.factor(order)) %>%
      mapview(zcol = "order", layer.name = .x
              ,label = "label"
              ,color = hcl.colors(5, palette = "viridis")
      )
  }) %>%
  reduce(`+`)

map@map %>%  htmlwidgets::saveWidget(here::here("req_portland_205/data", 'oregon_city_turn_counts_2023.html'))

mapview(tmp_tl_agg_comb_sf
        ,color = hcl.colors(5, palette = "viridis")
        ,zcol = "order")




















# unique(tl$act_link_count) %>%
combined_link_mvmnts = c(4, 5) %>%
  map_df(~{
  tmp_tl = tl %>%
    filter(act_link_count == .x) %>%
    arrange(activity_id, seq_ord) %>%
    select(activity_id, mode, network_links, seq_ord)

  index_seq = unique(tmp_tl$seq_ord) %>%
    sort()

  tmp_tl_agg = tmp_tl %>%
    pivot_wider(names_from = "seq_ord", values_from = "network_links") %>%
    mutate(count = 1) %>%
    group_by(mode, across(starts_with("seq_"))) %>%
    summarise(count = sum(count)) %>%
    ungroup()

  tmp_tl_agg_comb_sf = tmp_tl_agg %>%
    merge_cols() %>%
    bind_cols_prefix() %>%
    merge(table_network_data %>%
            select(stableEdgeId, geometry), .
          ,by.x = "stableEdgeId", by.y = "link_id") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    arrange(mvmnt_desc, order)

})


unique(combined_link_mvmnts$mvmnt_desc ) %>%
  map(~{
    combined_link_mvmnts %>%
      filter(mvmnt_desc  == .x) %>%
      mutate(order = as.factor(order)) %>%
      mapview(zcol = "order", layer.name = .x)
  }) %>%
  reduce(`+`)













x <- "MULTILINESTRING ((-122.6009 45.36302, -122.6015 45.3633), (-122.6015 45.36344, -122.6018 45.36309, -122.6022 45.36267))"
multilinestring(x)


check = tmp_tl_agg_comb %>%
  mutate(test = str_glue("({seq_1_geometry}, {seq_2_geometry})") %>%
           str_remove_all("LINESTRING") %>%
           paste0("MULTILINESTRING", .)) %>%
  st_as_sf(wkt = test, crs = 4326)


















