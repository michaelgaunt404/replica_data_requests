#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script is a genreal purpose script to make polys with mapedit
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(mapedit)
library(mapview)
library(sf)
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
location = "req_portland_barrier/data"

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#make network poly==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_point_2 = mapedit::drawFeatures() %>%
  st_transform(4326)

network_point_1 = network_point_1 %>%  mapedit::editFeatures() %>%
  st_transform(4326)

mapview(network_point_3) + mapview(network_point_1) + mapview(network_point_2)

temp = read_sf(here::here(location, "network_poly.gpkg"))

write_sf(network_poly, here::here(location, "network_poly.gpkg"))

#turn count process====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#need something that defines intersection locations
#this should be the input
intersection_locations = mapedit::drawFeatures() %>%
   st_transform(4326)

# intersections get buffered to grab data
intersection_locations_buff = intersection_locations %>%
   quick_buffer(rad = 20)

intersection_locations_buff %>%
   mapview()







{
   intersection_layer = intersection_locations
   # bb_sa_layer = 'data/req_dev/study_area_network.shp'
   # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
   # # bb_network_layer = 'req_portland_barrier/data/project_locations_buffer_union.gpkg'
   # # ,bb_sa_layer = 'req_portland_barrier/data/project_locations_buffer_union.gpkg'
   # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
   # trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
   # customer_name = "replica-customer"
   # file_destination = "req_portland_barrier/data"
   # max_record = Inf
   # mode_type = c('PRIVATE_AUTO')
   # query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk"
   #                  ,"primary", "primary_link" , "secondary", "secondary_link", "tertiary", "tertiary_link")
   }

#init_error_logging_and_setup
{
   query_start = gauntlet::clean_datetime()
   folder = here::here(file_destination, stringr::str_glue("data_{query_start}"))
   stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
   dir.create(folder)
   log_file = here::here(file_destination, stringr::str_glue("data_{query_start}/log_file.txt"))
   logger = log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
   log4r::info(logger, "Query started")
   message(stringr::str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))
   mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

   # log4r::info(logger,
   #             stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_sa_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{make_space('-', n = 10)}\n{paste0(stringr::str_glue('{sort(query_links)}'),collapse = '\n')}{make_space('-', n = 10)}"))

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

   intersection_layer_buff = intersection_layer %>%
      st_transform(crs = 2781) %>%
      st_buffer(dist = 20, endCapStyle = "SQUARE") %>%
      st_transform(crs = 4326)
      # quick_buffer(rad = 20)



         # if (nrow(intersection_layer_buff) != 1){
         #    log4r::fatal(logger, "More than one polygon in a supplied boundary object detected")
         #    stopifnot("One of the bounding layers you provided has more than one polygon, please fix this...." = F)
         # }

         if (st_crs(intersection_layer)$input == "EPSG:4326"){
            gauntlet::log_and_warn('CRS for one of the bounding layers you provided is was not set to EPSG:4326, it was converted for you....', logger)
            intersection_layer = sf::st_transform(intersection_layer, 4326)
         } else {
            gauntlet::log_and_info("Provider passed CRS checks.... GOOD", logger)
         }

         # temp_wkt = wellknown::sf_convert(intersection_layer_buff)
         temp_wkt_union = wellknown::sf_convert(intersection_layer_buff %>%  st_union())

         temp_wkt_list =
            list(
               intersection_layer_buff$X_leaflet_id
               ,wellknown::sf_convert(intersection_layer_buff)
            )

}

##query Google----
{
   gauntlet::log_and_info("Starting Google query now", logger)

   {
      message(stringr::str_glue("{make_space()}\nCreating network table now...."))


      network_links = temp_wkt_list %>%
         pmap(function(x, y) {

            temp = bigrquery::bq_project_query(
               customer_name
               ,stringr::str_glue("select * from (
select
*, {x} as intersection_name,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{y}')
,geometry) as flag_contains
from `{network_table}`
where highway in ({links_pro})
)
where flag_contains = TRUE")
            )

      replica_temp_tbl_name(temp)

         })

   }





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
and network_link_ids in (select stableEdgeId from ({paste(str_glue('(select * from {network_links})'), collapse = ' union all ')}));")
      )

log4r::info(logger,stringr::str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
   }

   ###CREATE: thruzone trips----
   #returns all trips that go through zone
   #all other sections aggregate this in some way
   {
      # message(stringr::str_glue("{make_space()}\nCreating trips through zone table now...."))

      table_trips_thru_zone = bigrquery::bq_project_query(
         customer_name
         ,stringr::str_glue("select *
from `{trip_table}`
where 1 = 1
and activity_id in (select activity_id from {replica_temp_tbl_name(table_trip_network_match)});")
      )

      # log4r::info(logger,stringr::str_glue("table_trips_thru_zone: {replica_temp_tbl_name(table_trips_thru_zone)}"))
      #
      # message(stringr::str_glue("{make_space()}\nInitial queries complete, starting aggregation queries now...."))
   }


   #network_links
   {
      table_ordered_trip_links = bigrquery::bq_project_query(
         customer_name
         ,stringr::str_glue("select
    activity_id, mode, vehicle_type
    ,origin_bgrp
    ,destination_bgrp
    ,network_link_ids_unnested
    ,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_thru_zone)}
                          ,unnest(network_link_ids) as network_link_ids_unnested;"))

      log4r::info(logger,stringr::str_glue("table_ordered_trip_links: {replica_temp_tbl_name(table_ordered_trip_links)}"))

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
            str_glue("{make_space()}\nUser supplied inputs resulted in {gauntlet::pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{make_space('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{make_space('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal MAXIMUM number of records is ~500,000{gauntlet::make_space('-')}")
            ,logger)
         message(stringr::str_glue("If your selection has resulted in too many records, you can............
         1) Decrease the study area layer resulting in less originating polys
         2) Decrease the size of the network layer supplied to the function
         3) Reduce the number of link types queired by the function by changing query_links input
         4) (BEST OPTION) Increase trip volume limit"))

         check_threshold_TF = F
         while (check_threshold_TF == F) {
            check_threshold  = readline("Would you like to increase link volume limit to minimize data download? (Y/N)? ")
            check_threshold_TF = (check_threshold %in% c("y", "n", "Y", "N"))
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
            bigrquery::bq_table_download(table_network, n_max = max_record)
            , file = ., row.names = F)
   } else {
      gauntlet::log_and_warn("User did not elect to download the network at function runtime.")
   }

   here(folder, "replica_sa_poly_index.csv") %>%
      write.csv(
         bigrquery::bq_table_download(table_sa_poly_index, n_max = max_record)
         , file = ., row.names = F)

   here(folder, "replica_trip_origin_links.csv") %>%
      write.csv(
         bigrquery::bq_table_download(table_trip_first_link_pro, n_max = max_record)
         , file = ., row.names = F)

   #this one is good
   here(folder, "table_agg_by_link_subset_limited.csv") %>%
      write.csv(
         bigrquery::bq_table_download(table_agg_by_link_subset_limited, n_max = max_record)
         , file = ., row.names = F)

   here(folder, "replica_trip_origin_destination.csv") %>%
      write.csv(
         bigrquery::bq_table_download(table_simple_origin_destination, n_max = max_record)
         , file = ., row.names = F)

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




































































##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================


 counties = tigris::counties(state = "OR") %>%  st_transform(4326)


 counties_sel = counties %>%  mapedit::selectFeatures()

 roads_1 = tigris::roads(state = 41
                         ,county = 005) %>%  st_transform(4326)

 roads_2 = tigris::roads(state = 41
                         ,county = 047) %>%  st_transform(4326)

 roads = bind_rows(
   roads_1
   ,roads_2
 )


 roads = counties_sel$COUNTYFP %>%
   map(~{
     tigris::roads(state = 41
                   ,county = .x) %>%
       st_transform(4326)
   }) %>%
   reduce(bind_rows)

 roads %>%
   filter(str_detect(FULLNAME, "Barlow|99")) %>%
   mapview()






 project_locations_points = bind_rows(
   network_point_3
   ,network_point_1
   ,network_point_2)


 project_locations_buffer = project_locations_points %>%
   quick_buffer(radius = 1000)


 project_locations_buffer_union = project_locations_buffer %>%
   st_union()

mapview( project_locations_buffer_union)



write_sf(network_poly, here::here(location, "network_poly.gpkg"))
write_sf(project_locations_buffer_union, here::here(location, "project_locations_buffer_union.gpkg"))
write_sf(project_locations_buffer, here::here(location, "project_locations_buffer.gpkg"))
write_sf(network_poly, here::here(location, "network_poly.gpkg"))



roads_sel = roads %>%
  st_filter(project_locations_buffer) %>%
  filter(str_detect(FULLNAME, "- 84|State Hwy 99E|- 5|US Hwy 30|State Hwy 211|State Hwy 224|Jordan|N Lombard St|Toll House Park|State Frontage Rd|S Molalla|E Historic Columbia River Hwy|Crown Point Hwy|Barlow Rd|NW Cornelius Pass Rd|SE Milwaukie Ave|Pacific Hwy") |
           LINEARID %in% c(1104257985936, 1104257985936, 1102155320882, 1102223149165, 1102223149165
                           ,110686011838, 110501879631)) %>%
  filter(!str_detect(FULLNAME, "Bus"))

roads_sel_buf = roads_sel %>%
  quick_buffer(radius = 20)
