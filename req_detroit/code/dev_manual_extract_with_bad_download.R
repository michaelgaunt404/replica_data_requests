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

table_network = 'replica-customer._d48ded622e745d9120443120b79c81a0aee797b2.anonaffddc8485c4bf894d9ce951ddf67212071e981c4b2a4f949cdf8ae34b9033e0'
table_agg_by_link_subset_limited = "replica-customer._d48ded622e745d9120443120b79c81a0aee797b2.anon82a807693d85f7a9bbb32e59f832dcbd8f114e7c7d62ecc26fa7ee24d8f74d16"
folder = here::here("req_detroit/data/data_20230419_095952")
max_record = 20000

bigrquery::bq_table_nrow(table_network)
bigrquery::bq_table_nrow(table_agg_by_link_subset_limited)


here(folder, "replica_queried_network.csv") %>%
  write.csv(
    bigrquery::bq_table_download(
      table_network
      ,n_max = max_record)
    , file = ., row.names = F)

here(folder, "table_agg_by_link_subset_limited.csv") %>%
  write.csv(
    bigrquery::bq_table_download(
      table_agg_by_link_subset_limited
      ,n_max = max_record)
    , file = ., row.names = F)

# here(folder, "replica_sa_poly_index.csv") %>%
#   write.csv(
#     bigrquery::bq_table_download(
#       table_sa_poly_index
#       ,n_max = max_record)
#     , file = ., row.names = F)

# here(folder, "replica_trip_origin_links.csv") %>%
#   write.csv(
#     bigrquery::bq_table_download(
#       table_trip_first_link_pro
#       ,n_max = max_record)
#     , file = ., row.names = F)

# here(folder, "replica_trip_origin_destination.csv") %>%
#   write.csv(
#     bigrquery::bq_table_download(
#       table_simple_origin_destination
#       ,n_max = max_record)
#     , file = ., row.names = F)






table_network_aug = bigrquery::bq_project_query(
  customer_name
  ,str_glue("select *
,CAST(stableEdgeId
 AS STRING) as stableEdgeId_string
 from `{table_network}`")
)



tmp = here::here('req_detroit/data/data_20230419_095952', "replica_queried_network.csv") %>%
  fread()























































function(location, folder, auto_save = F, network_object = NULL){

  if (is.null(network_object)){
    message("Spatial links were made using CSV from file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
      data.table::fread()
    replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)
  } else {
    message("Spatial links were made using supplied tabular network object...")
    replica_queried_network = sf::st_as_sf(network_object, wkt = "geometry", crs = 4326)
  }

  stopifnot("Network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network))) == 1))
  stopifnot("ERROR: Nerwork object must be LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network))[[1]] %in% c("LINESTRING")))

  replica_queried_network = replica_queried_network %>%
    mutate(stableEdgeId_trunc = str_trunc(stableEdgeId , 14, "right", "")) %>%
    select(stableEdgeId, stableEdgeId_trunc, streetName, distance, osmid, highway, flag_contains)


  if (auto_save) {
    sf::write_sf(replica_queried_network, here::here(location, folder, "replica_queried_network_links_trunc.gpkg"))
  }

  return(replica_queried_network)
}




function(location, folder, auto_save = F
         ,agg_count_object = NULL
         ,network_object = NULL){
  #TODO:make this compatible with other polygon types
  #TODO:review different aggregations - they don't make the most sense

  {
    # location = "data/req_dev"
    # folder = "data_20230207_161636"
    # agg_count_object = NULL
    # auto_save = F
    # data('replica_queried_network_cntds')
    # network_object = replica_queried_network_cntds
  }

  if (is.null(agg_count_object)){
    message("Aggregations will be made using file and location...")
    network_links = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
      data.table::fread() %>%
      mutate(#flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
        # ,
        network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
  } else {
    message("Aggregations will be made using supplied network object...")
    network_links = agg_count_object %>%
      mutate(#flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
        # ,
        network_link_ids_unnested_trunc = str_trunc(network_link_ids_unnested, 14, "right", ""))
  }

  stopifnot("You need to supply an object to merge tabular data with, either network centroids or polylines...." = !is.null(network_object))
  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(network_object))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(network_object))[[1]] %in% c("POINT", "LINESTRING")))

  #pre-process data
  {
    if (unique(st_geometry_type(network_object))[[1]] == "POINT"){
      index_extract = "POINT"
    } else {
      index_extract = "LINESTRING"
    }
  }


  #perform aggregations
  {
    message(stringr::str_glue("{make_space()}\nStarting aggreagtion by link and study area destination flag...."))

    #ANL
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link...."))

      agg_link = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested')
          ,grp_p = c()
          ,col = count, rnd = 2) %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(count))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(count)))

      agg_link_mrg = merge(
        network_object, agg_link
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_mrg_pro = agg_link_mrg %>%
        filter(!is.na(count)) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {count} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)"))

      message("Aggregation complete....")
    }

    #ANLT
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link and vehicle type...."))

      agg_link_vehicle_type = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'vehicle_type')
          ,grp_p = c('network_link_ids_unnested')
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(vehicle_type) %>%
        mutate(count_nrm_prank = dgt2(percent_rank(count))
               ,count_nrm_mmax = dgt2(normalize_min_max(count)))

      agg_link_vehicle_type_mrg = merge(
        network_object, agg_link_vehicle_type
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_vehicle_type_mrg_pro = agg_link_vehicle_type_mrg %>%
        filter(!is.na(vehicle_type)) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
          <hr>
          Metrics Adj for Veh. Type: {vehicle_type}
          <br>Link Volume: {count}
          <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))

      message("Aggregation complete....")
    }

    #ANLTT
    {
      agg_link_flag = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'flag_trip_type')
          ,grp_p = c('network_link_ids_unnested')
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(flag_trip_type) %>%
        mutate(count_nrm_prank = gauntlet::dgt2(percent_rank(count))
               ,count_nrm_mmax = gauntlet::dgt2(normalize_min_max(count))) %>%
        ungroup()

      agg_link_flag_mrg = merge(
        network_object, agg_link_flag
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_flag_mrg_pro = agg_link_flag_mrg %>%
        filter(!is.na(flag_trip_type )) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
          <hr>
          Metrics Adj for Trip Type: {flag_trip_type}
          <br>Link Volume: {count}
          <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))

      message("Aggregation complete....")
    }

    #ANLTO
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link, vehicle type, and originating poly....{gauntlet::make_space('-')}"))
      message("INFO:\nBy default external-external and external-internal trips are removed before aggregation")
      message("This is because origin based visualizations and calcultions display locations WITHIN/INTERNAL TO the user provided study area...")
      message(str_glue("Prefiltering makes processing faster and limits size of data object{gauntlet::make_space('-')}"))
      #NOTE: total link volume calculated here will be less than previous
      #--this is because it only counts int-int, and int-ext trips


      agg_link_vehicle_type_origin = network_links %>%
        filter(!is.na(vehicle_type)) %>%
        filter(origin_poly != "out of study area") %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'vehicle_type', "origin_poly")
          ,grp_p = c('network_link_ids_unnested', "origin_poly")
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(origin_poly, vehicle_type) %>%
        mutate(count_nrm_prank = dgt2(percent_rank(count))
               ,count_nrm_mmax = dgt2(normalize_min_max(count))) %>%
        ungroup()

      agg_link_vehicle_type_origin_mrg = merge(
        network_object, agg_link_vehicle_type_origin
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      nrow(filter(agg_link_flag_mrg, is.na(flag_trip_type)))
      (nrow(filter(agg_link_flag_mrg, is.na(stableEdgeId))) == 0)

      agg_link_vehicle_type_origin_mrg_pro = agg_link_vehicle_type_origin_mrg %>%
        filter(!is.na(origin_poly)) %>%
        mutate(label = str_glue(
          "Origin: {origin_poly}
          <br>Link name: {streetName} ({highway})
          <br>Link Volume: {ttl_count_link} (int-int/ext trips)
          <hr>
          Metrics Adj for Origin and Vehicle Type: {vehicle_type}
          <br>Link Volume: {count} - {100*count_nrm_mmax}% (Min-Max norm.)"))

      message("Aggregation complete....")}
  }

  #save out
  {
    list_objects = list(agg_link = agg_link_mrg_pro
                        # ,agg_link_flag = agg_link_flag_mrg_pro
                        ,agg_link_vehicle_type = agg_link_vehicle_type_mrg_pro
                        # ,agg_link_vehicle_type_origin = agg_link_vehicle_type_origin_mrg_pro
    )

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "aggregated_network_links.rds")
      message(str_glue("It is saved at this location:\n{file}"))
      saveRDS(list_objects, file)
    } else {
      message("You did not elect to automatically save the returned list object!")
    }
  }

  return(list_objects)

}


agg_link_mrg_pro %>%
  write_sf(here::here(location, folder, "anl_20230419.shp"))

agg_link_vehicle_type_mrg_pro %>%
  write_sf(here::here(location, folder, "anlt_20230419.shp"))


































