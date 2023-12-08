function(
    bb_network_layer
    ,bb_sa_layer
    ,query_links = c('highway','corridor','road'
                     ,'motorway','motorway_link'
                     ,'trunk','trunk_link'
                     ,'primary','primary_link'
                     ,'secondary','secondary_link'
                     ,'tertiary','tertiary_link')
    ,mode_type = c('PASSENGER_CAR','PRIVATE_AUTO'
                   ,'COMMERCIAL','CARPOOL'
                   ,'WALKING','BIKING','PUBLIC_TRANSIT'
                   ,'ON_DEMAND_AUTO','OTHER_TRAVEL_MODE')
    ,customer_name
    ,trip_table
    ,network_table
    ,file_destination
    ,query_network = T
    ,max_record = 1000){

  #commented out inputs
  {
    # bb_network_layer = 'data/req_dev/study_area_network.shp'
    # bb_sa_layer = 'data/req_dev/study_area_network.shp'
    # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
    # trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
    # customer_name = "replica-customer"
    # file_destination = "data/req_dev"
    # max_record = Inf
    # mode_type = c('COMMERCIAL')
    # # ,mode_type = c('ON_DEMAND_AUTO')
    # query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk")
  }

  #init_error_logging_and_setup
  {
    query_start = gauntlet::strg_clean_datetime()
    folder = here::here(file_destination, stringr::str_glue("data_{query_start}"))
    stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
    dir.create(folder)
    log_file = here::here(file_destination, stringr::str_glue("data_{query_start}/log_file.txt"))
    logger = log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
    log4r::info(logger, "Query started")
    message(stringr::str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))
    mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

    log4r::info(logger,
                stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_sa_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{make_space('-', n = 10)}\n{paste0(stringr::str_glue('{sort(query_links)}'),collapse = '\n')}{make_space('-', n = 10)}"))

    if (!any(is.na(query_links))){
      message("No NAs detected in links input.... Good")
      links_pro = paste0("'", query_links, "'", collapse = ", ")
      links_where_statement = stringr::str_glue("where highway in ({links_pro})")
    } else {
      message("NAs detected, all highway links will be queried.")
      warn(logger, "NAs detected, all highway links will be queried.")
      links_where_statement = ""
    }

    message("Initial set up complete....")
  }

  #load_process_boundary_object
  {
    message("Loading and processing boundary objects")
    list_wkt_objects = list(bb_network_layer
                            ,bb_sa_layer) %>%
      map(~{

        temp_object = sf::read_sf(here::here(.x))

        if (nrow(temp_object) != 1){
          log4r::fatal(logger, "More than one polygon in a supplied boundary object detected")
          stopifnot("One of the bounding layers you provided has more than one polygon, please fix this...." = F)
        }

        if (st_crs(temp_object)$input == "EPSG:4326"){
          gauntlet::log_and_warn('CRS for one of the bounding layers you provided is was not set to EPSG:4326, it was converted for you....', logger)
          temp_object = sf::st_transform(temp_object, 4326)
        }
        gauntlet::log_and_info("Bounding box checks pass.... GOOD", logger)
        temp_wkt = wellknown::sf_convert(temp_object)
      })
  }

  ##query Google----
  {
    gauntlet::log_and_info("Starting Google query now", logger)

    ###CREATE: network_table and get count----
    #gets links only for certain area defined by user
    #defines list of roads that we want to visualize
    #--could have a problem where a trip in study area is not on roads
    #--but if list is sufficiently large then I think its okay
    {
      message(stringr::str_glue("{make_space()}\nCreating network table now...."))

      table_network = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{list_wkt_objects[[1]]}')
,geometry) as flag_contains
from `{network_table}`
where highway in ({links_pro})
)
where flag_contains = TRUE")
      )

temp_query = stringr::str_glue("SELECT highway, count(*) as count
                      from {replica_temp_tbl_name(table_network)}
                      group by highway
                      order by count;")

table_network_link_count = bigrquery::bq_project_query(
  customer_name
  ,temp_query
)

project_dataset = stringr::str_glue("{table_network_link_count$project}.{table_network_link_count$dataset}")

log4r::info(logger, stringr::str_glue("Replica project and dataset are: {project_dataset}"))

highway_counts = bigrquery::bq_table_download(table_network_link_count)

if ((nrow(highway_counts) == 0)){
  mes_fatal = "Number of returned links was zero\n...change bounding box\n...stopping...."
  log4r::fatal(logger, mes_fatal)
  stopifnot("Number of returned links was zero\n...change bounding box\n...stopping...." = F)
} else {
  str_glue("{make_space()}\nNon-empty data returned, Good\nQuery continued...\nIn total {sum(highway_counts$count)} links\n{str_glue('-{highway_counts$count} ({100*gauntlet::dgt2(highway_counts$count/sum({highway_counts$count}))})% {highway_counts$highway}') %>%
                paste0(collapse = '\n')}") %>%
    gauntlet::log_and_info(., logger)

  index_empty_highways = query_links[-which(query_links %in% highway_counts$highway)]

  if (length(index_empty_highways) == 0){
    gauntlet::log_and_info(stringr::str_glue("All requested link types returned with some number of links\nnone empty.... GOOD"),logger)
  } else {
    gauntlet::log_and_warn(
      str_glue("{make_space()}\nThe following link types were requested but not found in bounding box:\n{paste0(index_empty_highways, collapse = '\n')}"),logger)
  }
}

log4r::info(logger,stringr::str_glue("table_network: {replica_temp_tbl_name(table_network)}"))

check_network_links_TF = F
while (check_network_links_TF == F) {
  check_network_links  = readline("Would you like to continue the function exectuion? (Y/N) ")
  check_network_links_TF = (check_network_links %in% c("y", "n", "Y", "N"))
  if (!check_network_links_TF){
    message("Not a valid input... try again")
  }
}

if (check_network_links %in% c("n", "N")) {
  log4r::warn(logger, "You have elected to terminate function run..." )
  stopifnot("You have elected to terminate function run..." = F)
}


    }

    ###CREATE: poly index and get count----
    #this will only work if they want block groups
    #screw it- itll work for now
    #--ACTUALLY NOT - just defining study area
    #real problem is in aggregation lower down
    #you could define which layer you want to pull from brps or ztaz or whatever
    #----> would need another input but this is a persistent problem
    {
      message(stringr::str_glue("{make_space()}\nCreating study area subset now...."))

      table_sa_poly_index = bigrquery::bq_project_query(
        "replica-customer"
        ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{list_wkt_objects[[2]]}')
,surface_point) as flag_contains
from `Geos.bgrp`
)
where flag_contains = TRUE")
      )

log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
    }

    ###CREATE: trip subset occurring on select links----
    #returns only trips that have a network link in subset of links
    #performs big cross=reference/filtering operation
    {
      message(stringr::str_glue("{make_space()}\nFiltering trips that only use queried network now...."))

      table_trip_network_match = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select distinct activity_id
from (
select distinct activity_id, network_link_ids
from
(select *
from `{trip_table}`
where mode in ({mode_type_pro})
), unnest(network_link_ids) as network_link_ids
)
where
1 = 1
and network_link_ids in (select stableEdgeId from {replica_temp_tbl_name(table_network)});"))

log4r::info(logger,stringr::str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
    }

    ###CREATE: thruzone trips----
    #returns all trips that go through zone
    #all other sections aggregate this in some way
    {
      message(stringr::str_glue("{make_space()}\nCreating trips through zone table now...."))

      table_trips_from_spec_polys = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select * from `{trip_table}`
        where 1=1
        AND mode in ({mode_type_pro})
        AND ((origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)})) OR
                           (destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)})))"))

      table_trips_thru_zone = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select *
,case
when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then origin_bgrp
else 'out of study area'
END as origin_poly
,case
when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then destination_bgrp
else 'out of study area'
END as destination_poly
,case
when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
else 'external'
END as flag_sa_origin
,case
when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
else 'external'
END as flag_sa_destination
from `{trip_table}`
        where 1=1
        AND mode in ({mode_type_pro})
        AND ((origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)})) OR
                           (destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)})))"))

      # log4r::info(logger,stringr::str_glue("table_trips_from_spec_polys: {replica_temp_tbl_name(table_trips_from_spec_polys)}"))

      # message(stringr::str_glue("{make_space()}\nInitial queries complete, starting aggregation queries now...."))
    }

    #od_data
    {
      message(stringr::str_glue("{make_space()}\nOrigin and Destination aggreations commencing...."))

      # table_simple_origin_destination = bigrquery::bq_project_query(
      #   customer_name
      #   ,stringr::str_glue("select mode
      # ,vehicle_type
      # ,origin_poly, flag_sa_origin
      # ,destination_poly, flag_sa_destination
      # ,count(*) as count
      # from {replica_temp_tbl_name(table_trips_thru_zone)}
      # group by mode, vehicle_type
      #      ,origin_poly, flag_sa_origin
      # ,destination_poly, flag_sa_destination;"))

      # log4r::info(logger,stringr::str_glue("table_simple_origin_destination: {replica_temp_tbl_name(table_simple_origin_destination)}"))

      table_od = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select
    activity_id, mode, vehicle_type
    ,origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng
    ,destination_bgrp, destination_poly, flag_sa_destination, destination_lat, destination_lng
    from {replica_temp_tbl_name(table_trips_thru_zone)};"))

      table_od = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select
    mode, vehicle_type
    ,origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng, count(*) as count
    from {replica_temp_tbl_name(table_trips_thru_zone)}
                           group by mode, vehicle_type,origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng;"))

      library(bigrquery)

      tmp = bq_table_download(table_od)
      tmp %>%
        # count(vehicle_type, origin_lat, origin_lng) %>%
        filter(vehicle_type != "HEAVY_COMMERCIAL") %>%
        st_as_sf(coords = c("origin_lng", "origin_lat"), crs = 4326) %>%
        st_filter(study_area) %>%
        mapview(cex = "count")


      table_ordered_trip_links = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select
    activity_id, mode, vehicle_type
    ,origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng
    ,destination_bgrp, destination_poly, flag_sa_destination, destination_lat, destination_lng
    ,network_link_ids_unnested
    ,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_from_spec_polys)}
                          ,unnest(network_link_ids) as network_link_ids_unnested;"))

      log4r::info(logger,stringr::str_glue("table_ordered_trip_links: {replica_temp_tbl_name(table_ordered_trip_links)}"))

      table_trip_first_link = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue("select
  mode, vehicle_type
  ,origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng, network_link_ids_unnested
  , count(*) as count
  from {replica_temp_tbl_name(table_ordered_trip_links)}
  where 1 = 1
  and index = 1
  group by mode, vehicle_type, origin_bgrp, origin_poly, flag_sa_origin, origin_lat, origin_lng, network_link_ids_unnested;"))
#
#       table_trip_first_link_pro = bigrquery::bq_project_query(
#         customer_name
#         ,stringr::str_glue("select
# table_left.*
# ,table_right.*
# from {replica_temp_tbl_name(table_trip_first_link)} table_left
# left join (select stableEdgeId,startLat,startLon,endLat,endLon
# from {network_table}
# where stableEdgeId in (select network_link_ids_unnested from {replica_temp_tbl_name(table_trip_first_link)})) table_right
# on (table_left.network_link_ids_unnested = table_right.stableEdgeId);"))

      # log4r::info(logger,stringr::str_glue("table_trip_first_link_pro: {replica_temp_tbl_name(table_trip_first_link_pro)}"))

      message(stringr::str_glue("Origin and Destination aggreations complete....{make_space()}"))
    }

    #network_links
    {
      message(stringr::str_glue("{make_space()}\nLink aggreations commencing...."))

      table_agg_by_link = bigrquery::bq_project_query(
        customer_name
        ,  str_glue("select
mode, vehicle_type
,origin_poly, flag_sa_origin
,flag_sa_destination
,network_link_ids_unnested
,count(*) as count
from {replica_temp_tbl_name(table_ordered_trip_links)}
group by
mode, vehicle_type
,origin_poly, flag_sa_origin
,flag_sa_destination
,network_link_ids_unnested"))

      log4r::info(logger,stringr::str_glue("table_agg_by_link: {replica_temp_tbl_name(table_agg_by_link)}"))

      #subset network count
      {
        table_agg_by_link_subset = bigrquery::bq_project_query(
          customer_name
          , stringr::str_glue("select *
from {replica_temp_tbl_name(table_agg_by_link)}
where network_link_ids_unnested in
         (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})"))

        table_agg_by_link_sum_subset = bigrquery::bq_project_query(
          customer_name
          ,qs_table_agg_by_link_sum(table_agg_by_link_subset))

        summary_table_link_counts = bigrquery::bq_table_download(table_agg_by_link_sum_subset) %>%
          mutate(flag_link = forcats::fct_relevel(flag_link,
                                                  c("1 count", "2 count", "3 count"
                                                    ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
          arrange(flag_link) %>%
          mutate(percent = 100*gauntlet::dgt3(count/sum(count))
                 ,count_cum = cumsum(count)
                 ,percent_cum = 100*gauntlet::dgt3(count_cum/sum(count))) %>%
          arrange(desc(flag_link)) %>%
          mutate(count_rm = cumsum(count)
                 ,percent_rm = cumsum(percent))

        #message here to reduce the size of the network!
        gauntlet::log_and_info(
          str_glue("{make_space()}\nUser supplied inputs resulted in {gauntlet::strg_pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{make_space('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{make_space('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal MAXIMUM number of records is ~500,000{gauntlet::make_space('-')}")
          ,logger)
        message(stringr::str_glue("If your selection has resulted in too many records, you can............
         1) Decrease the study area layer resulting in less originating polys
         2) Decrease the size of the network layer supplied to the function
         3) Reduce the number of link types queired by the function by changing query_links input
         4) (BEST OPTION) Increase trip volume limit"))

        check_threshold_TF = F
        while (check_threshold_TF == F) {
          check_threshold  = readline("Would you like to increase link volume limit to minimize data download? (Y/N)? ")
          check_threshold_TF = (check_threshold %in% c("y", "n", "Y", "N", "zz_backdoor"))
          if (!check_threshold_TF){
            message("Not a valid input... try again")
          }
        }

        if (check_threshold %in% c("n", "N")) {
          log4r::info(logger, "You did not elect to increase link volume threshold, minimum of 5 trips per link will be used..." )

          table_agg_by_link_subset_limited = bigrquery::bq_project_query(
            customer_name
            ,stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}
                    where count >= 5"))

          log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
        } else if (check_threshold == "zz_backdoor") {
          message(stringr::str_glue("Backdoor access granted. No link count threshold will be applied..."))
          log4r::warn(logger, "Backdoor access granted. No link count threshold will be applied..." )

          table_agg_by_link_subset_limited = bigrquery::bq_project_query(
            customer_name,
            stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}")
          )

          log4r::info(logger, stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
        } else {

          check_threshold_count_TF = F
          while (check_threshold_count_TF == F) {
            check_threshold_count = readline("Please pick new threshold (integer): ")
            check_threshold_count = as.numeric(check_threshold_count)
            check_threshold_count_TF = (check_threshold_count >= 5)

            if (!check_threshold_count_TF){
              message("Not a valid input, value must be 5 or greater... try again")
            } else {
              message(stringr::str_glue("Thank you, a threshold of {check_threshold_count} will be used..."))
              log4r::warn(logger, "You elected to increase link volume threshold, you supplied a threshold of {check_threshold_count} to be applied" )
            }
          }

          table_agg_by_link_subset_limited = bigrquery::bq_project_query(
            customer_name
            ,stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}
                    where count >= {check_threshold_count}"))

          log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
        }
      }

    }

    log4r::info(logger,stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))
    log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))

    message(stringr::str_glue("Link aggreations complete....{make_space()}"))

  }

  #data_download
  {
    message("Starting data download now.....")

    check_network_dl_TF = F
    while (check_network_dl_TF == F) {
      check_network_dl  = readline(stringr::str_glue("Would you like to download the network links ({summary_table_link_counts[[3, 6]]} records) at this time? (Y/N) "))
      check_network_dl_TF = (check_network_links %in% c("y", "n", "Y", "N"))
      if (!check_network_dl_TF){
        message("Not a valid input... try again")
      }
    }

    if (check_network_dl %in% c("y", "Y")){
      here(folder, "replica_queried_network.csv") %>%
        write.csv(
          bigrquery::bq_table_download("replica-customer._d48ded622e745d9120443120b79c81a0aee797b2.anon1fd13c4557ab75023b1c0fbca1842c49110b8f8564a73f39b6840da268e3246a", n_max = max_record, page_size = 1000)
          , file = ., row.names = F)
    } else {
      gauntlet::log_and_warn("User did not elect to download the network at function runtime.")
    }

    here(folder, "replica_sa_poly_index.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_sa_poly_index, n_max = max_record, page_size = 1000)
        , file = ., row.names = F)

    here(folder, "replica_trip_origin_links.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_trip_first_link, n_max = max_record, page_size = 1000)
        , file = ., row.names = F)

    #this one is good
    here(folder, "table_agg_by_link_subset_limited.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_agg_by_link_subset_limited, n_max = max_record, page_size = 1000)
        , file = ., row.names = F)

    # here(folder, "replica_trip_origin_destination.csv") %>%
    #   write.csv(
    #     bigrquery::bq_table_download(table_simple_origin_destination, n_max = max_record, page_size = 1000)
    #     , file = ., row.names = F)

    message("All data downloaded.....")
  }

  #perform checks
  {
    message("Perfroming data checks now.....")

    link_merge_check = check_links_download(
      location = file_destination
      ,folder = stringr::str_glue("data_{query_start}")
    )

    here(folder, "replica_network_links_without_trip_volumes.csv") %>%
      write.csv(link_merge_check
                ,file = ., row.names = F)

  }

}









get_tigris_polys_from_replica_index_2 = function(
    location, folder, auto_save = F, network_object = NULL, year = 2010){

  # location = "data/req_dev"
  # folder = "data_20230111_150315"
  # auto_save = F
  # network_object = NULL
  # states = "WA"
  # year = 2010

  #load_replica_poly_index
  if (is.null(network_object)){
    message("Study area index loaded using file and location...")
    replica_queried_network = here::here(location, folder, "replica_sa_poly_index.csv") %>%
      data.table::fread() %>%
      select(raw_id)
  } else {
    message("Study area index made using suplied network object...")
    replica_queried_network = network_object %>%
      select(raw_id)
  }

  index_replica_queried_network = replica_queried_network %>%
    mutate(states = raw_id %>%
             str_trunc(2, "right", ellipsis = "") %>%
             as.integer()
           ,counties = raw_id %>%
             str_trunc(5, "right", ellipsis = "") %>%
             str_trunc(3, "left", ellipsis = "") %>%
             as.integer()) %>%
    select(states, counties) %>%
    unique()

  block_groups = list(index_replica_queried_network$states
       ,index_replica_queried_network$counties) %>%
    pmap_df(~{

      tigris::block_groups(state = .x
                           ,county = .y
                           ,year = year
                           ,refresh = TRUE)
    }, progress = T) %>%
    sf::st_transform(4326)

  block_groups_sub = block_groups %>%
    mutate(flag_blkgrps = 1) %>%
    merge(replica_queried_network %>%
            mutate(flag_index = 1
                   ,raw_id = as.character(raw_id))
          , by.x = "GEOID10", by.y = "raw_id", all = T) %>%
    mutate(across(c(flag_blkgrps, flag_index),~tidyr::replace_na(.x, 0))) %>%
    mutate(flag_poly_merge = case_when(
      (flag_blkgrps == 1 & flag_index == 0)~"Tirgris but not Replica"
      ,(flag_blkgrps == 1 & flag_index == 1)~"Both Tirgris and Replica"
      ,(flag_blkgrps == 0 & flag_index == 1)~"Replica but not Tigris"
      ,T~"Error please check.."
    ))

  check_match = block_groups_sub %>%
    sf::st_drop_geometry() %>%
    filter(flag_index == 1) %>%
    count(flag_blkgrps, flag_index) %>%
    mutate(percent = paste0(100*(n/sum(n)), "%")) %>%
    capture.output() %>%
    paste0(collapse = "\n")

  stringr::str_glue("{make_space('-')}\n1 - 1 combinations indicate good matches") %>%  message()
  stringr::str_glue("Anything else indicates bad poly matches") %>%  message()
  stringr::str_glue("The following query resulted in the following matches...\n{check_match}") %>%  message()

  block_groups_sub = block_groups_sub %>%
    filter(flag_poly_merge == "Both Tirgris and Replica") %>%
    select(!c(flag_blkgrps, flag_index, flag_poly_merge))

  stringr::str_glue("Replica query acquired {nrow(replica_queried_network)} polygons\nPolygon query returning {nrow(block_groups_sub)} polygons\n{100*nrow(block_groups_sub)/nrow(replica_queried_network)}% match{make_space('-')}") %>%  message()

  if (auto_save) {
    sf::write_sf(block_groups_sub, here::here(location, folder, "acquired_sa_polys.gpkg"))
  }

  return(block_groups_sub)
}


make_trip_origin_point_layer_2 = function(location, folder, auto_save = F
         ,first_links_object = NULL){

  # location = "data/req_dev"
  # folder = 'data_20230117_092037'

  if (is.null(first_links_object)){
    message("Trip origin link points made using file and location...")
    replica_trip_origin_links = here::here(location, folder, "replica_trip_origin_links.csv") %>%
      data.table::fread()
    replica_trip_origin_links = sf::st_as_sf(replica_trip_origin_links, coords = c('origin_lng', 'origin_lat'), crs = 4326)
  } else {
    message("Trip origin link points made using suplied network object...")
    replica_trip_origin_links = sf::st_as_sf(first_links_object, coords = c('origin_lng', 'origin_lat'), crs = 4326)
  }

  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_trip_origin_links))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_trip_origin_links))[[1]] %in% c("POINT")))

  replica_trip_origin_links = replica_trip_origin_links %>%
    select(!c(origin_bgrp, network_link_ids_unnested))

  if (auto_save) {
    sf::write_sf(replica_trip_origin_links, here::here(location, folder, "replica_trip_origin_links.gpkg"))
  }

  return(replica_trip_origin_links)
}

#
# study_area = read_sf(here::here('req_austin/data/study_area_poly_20230418.gpkg'))
#
# replica_trip_origin_links %>%
#   st_filter(study_area) %>%
# mapview::mapview(cex = "count")






















