


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















#' Download and visualize a map of the selected study area
#'
#' @param network_table Name of the network table in the bigquery project
#' @param trip_table Name of the trip table in the bigquery project
#' @param mode_type Character vector specifying the mode types to include in the analysis
#' @param customer_name Name of the bigquery project
#' @param jitter_factor A factor that determines how much the network data is jittered to improve visualization
#' @return A mapview object showing the network data in the selected study area
#'
#' @examples
download_and_visualize_map("replica-customer.northwest.northwest_2021_Q4_network_segments",
                           "replica-customer.northwest.northwest_2021_Q4_thursday_trip",
                           c('PRIVATE_AUTO'),
                           "replica-customer",
                           0.003)
download_and_visualize_map <- function(network_table, trip_table, mode_type, customer_name, jitter_factor) {
  jitter_factor = .003
  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

  message("Please draw a study area that will be used to query Replica's roadway network...")
  message("Draw it as small and parsimonious as possible")
  message("You can draw mulitple, discrete objects if you wish")

  study_area = mapedit::drawFeatures() %>% st_transform(4326)

  study_area_wkt = wellknown::sf_convert(st_union(study_area))

  # Query the network table for data in the selected study area
  table_network = bigrquery::bq_project_query(
    customer_name,
    stringr::str_glue("select * from (
                      select *,
                      ST_INTERSECTS(
                      ST_GEOGFROMTEXT('{study_area_wkt}')
                      ,geometry) as flag_contains
                      from `{network_table}`
                      )
                      where flag_contains = TRUE")
  )

  table_network_count = bigrquery::bq_table_nrow(table_network)

  message(str_glue("{table_network_count} links were returned... \nwould you like to continue and download or abort and try again"))
  check_continue = robust_prompt_used("continue")
  stopifnot("Aborted" = check_continue)

  table_network_data = bigrquery::bq_table_download(table_network) %>%
    arrange(stableEdgeId)

  table_network_data_sf =  table_network_data %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  message(str_glue("The network you queried will be displayed with a {jitter_factor} jitter factor..."))

  table_network_data_sf_jit = table_network_data_sf %>%
    st_jitter(factor = jitter_factor)

  mapview(table_network_data_sf_jit)

  rejitter <- TRUE
  while (rejitter) {
    jitter_decision <- readline(prompt="Do you want to rejitter the data? (T/F)")
    if (tolower(jitter_decision) == "t") {
      jitter_factor = prompt_jitter_factor()
      table_network_data_sf_jit = table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      mapview(table_network_data_sf_jit)
    }
  }


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

  link_selections %>%  saveRDS(here::here("req_portland_205/data", 'link_selections.rds'))

  link_selections = readRDS(here::here("req_portland_205/data", 'link_selections.rds'))


  link_selections_df = link_selections %>%
    flatten() %>%
    flatten_named_list() %>%
    tidyr::separate(col = "name", into = c("intersection", "sequence"), sep = "\\.")


  link_selections_index_pro = paste0("'", unique(link_selections_df$value), "'", collapse = ", ")

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
and network_links in ({link_selections_index_pro});")
  )

table_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select
activity_id,mode, network_links,vehicle_type, link_ord
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS seq_ord
,count(*)
    OVER (PARTITION BY activity_id) AS act_link_count
from {replica_temp_tbl_name(table)}
order by activity_id, link_ord;")
)



}

table_network_data_simp = table_network_data %>%
  select(stableEdgeId, streetName) %>%
  mutate(streetName = replace_na(streetName, "NO_NAME"))


first = table_network_data %>%
  select(stableEdgeId, highway, streetName, geometry) %>%
  rename_with(~str_glue("seq_1_{.x}"))

second = table_network_data %>%
  select(stableEdgeId, highway, streetName, geometry) %>%
  rename_with(~str_glue("seq_2_{.x}"))

tl = here::here("req_portland_205/data", 'turning_links_20230419.csv') %>%
  fread() %>%
  mutate(seq_ord = str_glue("seq_{seq_ord}"))

mapviewOptions(homebutton = F)

##for two links
{
tmp_tl = tl %>%
  filter(act_link_count == 2) %>%
  arrange(activity_id, seq_ord) %>%
  select(activity_id, mode, network_links, seq_ord)

tmp_tl_agg = tmp_tl %>%
  pivot_wider(names_from = "seq_ord", values_from = "network_links") %>%
  mutate(count = 1) %>%
  group_by(mode, seq_1, seq_2) %>%
  summarise(count = sum(count)) %>%
  ungroup()

tmp_tl_agg_lng_agg = tmp_tl_agg %>%
  pivot_longer(cols = c("seq_1", "seq_2")) %>%
  merge(table_network_data %>%
          select(stableEdgeId, highway, streetName, geometry), .
        ,by.x = "stableEdgeId", by.y = "value") %>%
  pivot_wider(cols = c("seq_1", "seq_2"))
  st_as_sf(wkt = "geometry", crs = 4326)

  tmp_tl_agg_comb = tmp_tl_agg %>%
    merge(., first, by.x = "seq_1", by.y = "seq_1_stableEdgeId") %>%
    merge(., second, by.x = "seq_2", by.y = "seq_2_stableEdgeId") %>%
    mutate(link = str_glue("{seq_1_streetName}_to_{seq_2_streetName}")) %>%
    select(seq_2, seq_1, link,count ) %>%
    pivot_longer(cols = c("seq_1", "seq_2")) %>%
    merge(table_network_data %>%
            select(stableEdgeId, highway, streetName, geometry), .
          ,by.x = "stableEdgeId", by.y = "value") %>%
    st_as_sf(wkt = "geometry", crs = 4326)


unique(tmp_tl_agg_comb$link) %>%
  map(~{
    tmp_tl_agg_comb %>%
      filter(link == .x) %>%
      mapview(zcol = "name", layer.name = .x)
  }) %>%
  reduce(`+`)
}



##for two links
{
  tmp_tl = tl %>%
    filter(act_link_count == 3) %>%
    arrange(activity_id, seq_ord) %>%
    select(activity_id, mode, network_links, seq_ord)

  index_seq = unique(tmp_tl$seq_ord)

  tmp_tl_agg = tmp_tl %>%
    pivot_wider(names_from = "seq_ord", values_from = "network_links") %>%
    mutate(count = 1) %>%
    group_by(mode, across(starts_with("seq_"))) %>%
    summarise(count = sum(count)) %>%
    ungroup()


  merge_cols <- function(data) {

    tmp_colnames = colnames(select(data, starts_with("seq_")))

    for (colname in tmp_colnames){
      data = merge(data
                   ,table_network_data_simp
                   ,by.x = colname, by.y = "stableEdgeId"
                   ,suffixes = c("", paste0("_", colname)))
        # rename("{colname}_streetName" = "streetName")
      colnames(data) <- gsub(paste0("_", colname, "$"), paste0("_streetName"), colnames(data))

      # merge(., table_network_data_simp, by.x = "seq_1", by.y = "stableEdgeId") %>%
      #   rename("seq_1_streetName" = "streetName")
    }
    return(data)
  }

  tmp_tl_agg_comb = tmp_tl_agg %>%
    merge_cols()
    merge(., table_network_data_simp, by.x = "seq_1", by.y = "stableEdgeId") %>%
      rename("seq_1_streetName" = "streetName") %>%
      merge(., table_network_data_simp, by.x = "seq_2", by.y = "stableEdgeId") %>%
      rename("seq_2_streetName" = "streetName") %>%
      merge(., table_network_data_simp, by.x = "seq_3", by.y = "stableEdgeId") %>%
      rename("seq_3_streetName" = "streetName") %>%
      mutate(link = str_glue("{seq_1_streetName}_to_{seq_2_streetName}_to_{seq_3_streetName}")) %>%
    select(link,count, starts_with("seq_")) %>%
    pivot_longer(cols = starts_with("seq_")) %>%
    merge(table_network_data %>%
            select(stableEdgeId, highway, streetName, geometry), .
          ,by.x = "stableEdgeId", by.y = "value") %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  unique(tmp_tl_agg_comb$link) %>%
    map(~{
      tmp_tl_agg_comb %>%
        filter(link == .x) %>%
        mapview(zcol = "name", layer.name = .x)
    }) %>%
    reduce(`+`)
}
















x <- "MULTILINESTRING ((-122.6009 45.36302, -122.6015 45.3633), (-122.6015 45.36344, -122.6018 45.36309, -122.6022 45.36267))"
multilinestring(x)


check = tmp_tl_agg_comb %>%
  mutate(test = str_glue("({seq_1_geometry}, {seq_2_geometry})") %>%
           str_remove_all("LINESTRING") %>%
           paste0("MULTILINESTRING", .)) %>%
  st_as_sf(wkt = test, crs = 4326)


















