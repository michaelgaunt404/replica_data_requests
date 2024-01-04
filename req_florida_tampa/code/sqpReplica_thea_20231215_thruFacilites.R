#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script contains a basic replicaToolkitR workflow.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: This is a basic workflow that contains all the general steps
#-------- one will want to execute when acquiring data from Replica.
#--------
#-------- Reminder: This script is supposed to be barebones. ReplicaToolkitR
#-------- suggests that you limit computation and variable creation to
#-------- to scripts and/or `targets` and then load those variables into
#-------- Rmakrdown or other data products.
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

library(gauntlet) #devtools::install_github("michaelgaunt404/gauntlet")
library(gauntletMap) #devtools::install_github("michaelgaunt404/gauntletMap")
library(tidyverse)
library(here)
library(magrittr)
library(log4r)
library(data.table)

library(leaflet)
library(leafem)
library(leaflet.extras2)
library(mapview)
library(mapedit)
library(sf)
library(sfhotspot)
library(tigris)
library(wellknown)
library(SpatialKDE)

library(reactable)
library(crosstalk)

library(replicaToolkitR)
#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#none currently here - place your own source code here as you see fit


#i think that has been fixed but has yet to update
# popup_tbl_pretty = function(data){
#   data %>%
#     janitor::clean_names() %>%
#     sf::st_set_geometry(NULL) %>%
#     leafpop::popupTable()
# }

#put in replicatoolkitR
# get_tigris_polys_from_replica_index_2 = function(
#     location, folder, auto_save = F, network_object = NULL, year = 2010){

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

# put in replicatoolkitr
# map_replica_od_layers = function(acquired_sa_polys
#                                  ,replica_trip_origin_destination){
  temp = acquired_sa_polys %>%
    merge(replica_trip_origin_destination %>%
            group_by(origin_poly, vehicle_type) %>%
            summarise(count = sum(count)) %>%
            ungroup()
          ,by.x = "GEOID10", by.y = "origin_poly")

  temp_agg = acquired_sa_polys %>%
    merge(replica_trip_origin_destination %>%
            group_by(origin_poly) %>%
            summarise(count = sum(count)) %>%
            ungroup()
          ,by.x = "GEOID10", by.y = "origin_poly")

  od_map = unique(temp$vehicle_type) %>%
    map(~{
      temp %>%
        filter(vehicle_type == .x) %>%
        mutate(area_mi2 = round(ALAND10/2589988, 2)
               ,count_density = round(count/area_mi2, 2)) %>%
        mapview(zcol = "count_density"
                ,color = "black"
                ,layer.name  = str_glue("{str_replace(.x, '_COMMERCIAL', ' Duty')} (trips per mi2)")
                ,homebutton = F)
    }) %>%
    reduce(`+`)

  od_agg_map = temp_agg %>%
    mutate(area_mi2 = round(ALAND10/2589988, 2)
           ,count_density = round(count/area_mi2, 2)) %>%
    mapview(zcol = "count"
            ,color = "black"
            ,layer.name  = str_glue("All (trips per mi2)")
            ,homebutton = F)

  temp_od = (od_agg_map + od_map) %>%
    .@map %>%
    leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                         ,"MEDIUM Duty (trips per mi2)"))

  return(temp_od)

}
#updated
# mvmnt_pattern_vmt_smmry = function(
#     link_selections_df = NULL
#     ,replica_queried_network = NULL
#     ,replica_trip_mvmnt_seq_table = NULL
#     ,location
#     ,folder
#     ,include_mode_veh_type = TRUE
#     ,auto_save = F){

  if (is.null(link_selections_df)){
    message(str_glue("Variable _{deparse(substitute(link_selections_df))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    link_selections_df = data.table::fread(
      here::here(location, folder, "link_selections_df.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(link_selections_df))}_ is not NULL, will use instead of loading from the directory"))
  }

  if (is.null(replica_queried_network)){
    message(str_glue("Variable _{deparse(substitute(replica_queried_network))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    replica_queried_network = data.table::fread(
      here::here(location, folder, "replica_queried_network.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(replica_queried_network))}_ is not NULL, will use instead of loading from the directory"))
  }

  if (is.null(replica_trip_mvmnt_seq_table)){
    message(str_glue("Variable _{deparse(substitute(replica_trip_mvmnt_seq_table))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    replica_trip_mvmnt_seq_table = data.table::fread(
      here::here(location, folder, "replica_trip_mvmnt_seq_table.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(replica_trip_mvmnt_seq_table))}_ is not NULL, will use instead of loading from the directory"))
  }

  link_selections_df_pro = link_selections_df %>%
    merge(., replica_queried_network %>%
            select(stableEdgeId, streetName, distance, highway)
          ,by.x = "value", by.y = "stableEdgeId", all.x = TRUE) %>%
    mutate(distance_mm = distance
           ,distance_ft = distance/304.8
           ,distance_mile = distance_ft/5280)  %>%
    select(mvmnt,  sequence, streetName, index_sel_seq, value
           ,everything()) %>%
    rename(network_links = value
           # ,mvmnt = intersection
           ,mvmnt_seq = sequence) %>%
    arrange(mvmnt, mvmnt_seq, index_sel_seq) %>%
    group_by(mvmnt, mvmnt_seq) %>%
    summarise(distance_mile_ttl = sum(distance_mile)
              ,ttl_links_used = n(), .groups = "drop")


  if(include_mode_veh_type) {
    cols_index_agg = c("mvmnt",  "mvmnt_seq", "mode", "vehicle_type")
  } else {
    cols_index_agg = c("mvmnt",  "mvmnt_seq")
  }

  agg_trips_mvmnt = replica_trip_mvmnt_seq_table %>%
    select(mvmnt, mvmnt_seq, activity_id, mode, vehicle_type) %>%
    unique() %>%
    mutate(count = 1) %>%
    group_by(across({{cols_index_agg}})) %>%
    summarise(count = sum(count), .groups = "drop")

  tmp_output = agg_trips_mvmnt %>%
    merge(link_selections_df_pro
          ,by = c("mvmnt", "mvmnt_seq")) %>%
    mutate(vmt = round(count * distance_mile_ttl, 2) )

  if (auto_save) {
    write.csv(tmp_output, here::here(location, folder, "mvmnt_pattern_vmt_smmry.csv"))
  }

  return(tmp_output)

}

# spatial_agg_object = yolo
viz_robust_sttcNtwrk_map = function(spatial_agg_object
         ,list_object = T
         ,return_leaflet = F
         ,include_mvmnt = F
         ,agg_col
         ,list_index
         ,prefix = NA){

  # agg_col = "map_var"
  # list_index = 'agg_link_vehicle_type'

  if(list_object){
    data = spatial_agg_object[[list_index]]
  } else {
    data = spatial_agg_object
  }

  index_agg_col = unique(data[[agg_col]]) %>%
    sort()

  if (is.na(prefix)){prefix = ""} else {prefix = str_glue("{prefix} - ")}

  data = data %>%
    filter(!is.na(count)) %>%
    mutate(lwd_rescale = rescale_to(count, 10))

  map_object = list(
    index_agg_col
    ,viridisLite::viridis(length(index_agg_col))
  ) %>%
    pmap(function(x, y){
      filter_string = str_glue("{agg_col} == \'{x}\'")

      print(filter_string)

      temp = data %>%
        filter(eval(parse(text = filter_string))) %>%
        print()

      layer_name = str_glue("{prefix}{pretty_char(x)}")

      print(layer_name)
      temp %>%
        mapview(label = "label"
                ,color = y
                ,layer.name = layer_name
                ,lwd = 'lwd_rescale'
                ,popup = popup_tbl_pretty(temp %>%  select(-c(label, lwd_rescale)))
                ,homebutton = F)
    }) %>%
    reduce(`+`)

  if (return_leaflet){
    map_object@map %>%
      leaflet::hideGroup(index_agg_col)
  } else {
    map_object
  }

  return(map_object)

}

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
# bb_sa_layer = mapedit::drawFeatures() %>%  sf::st_transform(4326)
# bb_network_layer = bb_sa_layer

save_location = "data/req_zz/tampa_mvmnt_custom_1"
# sf::write_sf(bb_sa_layer, here::here(save_location, "bb_sa_layer.gpkg"))
# sf::write_sf(bb_network_layer, here::here(save_location, "bb_network_layer.gpkg"))
bb_sa_layer = sf::read_sf(here::here(save_location, "bb_sa_layer.gpkg"))
bb_network_layer = sf::read_sf(here::here(save_location, "bb_network_layer.gpkg"))

replicaToolkitR::query_replica_mvmnt_patterns(
  data_set_location = "south_atlantic"
  ,data_set_period = "2023_Q2"
  ,data_set_day = "thursday"
  ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
  ,query_links_net = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
  ,mode_type = c('COMMERCIAL', 'PRIVATE_AUTO')
  ,bb_sa_layer = bb_sa_layer
  ,bb_network_layer = bb_network_layer
  ,prefix_origin = "origin"
  ,prefix_dest = "destination"
  ,max_record = Inf
  ,customer_name = "replica-customer"
  ,save_location = save_location
  ,jitter_factor = 0.003
  ,mvmnt_df = data.frame(
     mvmnt_desc = c("brndnPrkwyW_seg"
                    ,"brndnPrkwyW_fullMvmnt"
                    ,"brndnPrkwyE_seg"
                    ,"brndnPrkwyE_fullMvmnt"
                    # ,'meridian_complex'
                    )
     ,ttl_seq = c(2, 6)
     ,bearing = c(NA))
)


replicaToolkitR::query_network_trip_using_bbox (
  data_set_location = "south_atlantic"
  ,data_set_period = "2023_Q2"
  ,data_set_day = "thursday"
  ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
  # ,query_links_net = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
  ,mode_type = c('COMMERCIAL', 'PRIVATE_AUTO')
  ,bb_sa_layer = bb_sa_layer
  ,bb_network_layer = bb_network_layer
  ,prefix_origin = "origin"
  ,prefix_dest = "destination"
  ,max_record = Inf
  ,customer_name = "replica-customer"
  ,file_destination = save_location

)



#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#should be the same as *file_destination* input above
location = save_location

#this is the folder that the data is written to
#it is a sub-folder to *file_destination* and will have a data_[datetime] format
folder = 'mvmnt_data_20231214_fllNetwrk'

#process acquired data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable


##create gis layers=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get_tigris_polys_from_replica_index_2(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )
#
# make_network_link_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )
#
# make_network_centroid_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )

# ##aggregate data================================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make_trip_origin_point_layer_2(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )

mvmnt_pattern_vmt_smmry = mvmnt_pattern_vmt_smmry(
  location = location
  ,folder = folder
  ,include_mode_veh_type = T
  ,auto_save = T
)

mvmnt_pattern_vmt_smmry %>%  clipr::write_clip()

#VIZ============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##load data=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NOTE:: BASIC LOAD SECTION
#LOADS BASIC SHAPEFILES

#skip poi_list
# poi_list = fread(
#   header = T, skip = 0, here(location, "poi_list.csv")) %>%
#   janitor::remove_empty("rows")

replica_queried_network_links = read_sf(
  here(location, folder ,"replica_queried_network_links.gpkg"))

# replica_queried_network_links_trunc = read_sf(
#   here(location, folder ,"replica_queried_network_links_trunc.gpkg"))

acquired_sa_polys = read_sf(here(location,folder,"acquired_sa_polys.gpkg"))

table_agg_by_link_subset_limited = fread(
  here(location, folder, "table_agg_by_link_subset_limited.csv")) %>%
  data.table() %>%
  .[,`:=`(vehicle_type = fifelse(mode == "PRIVATE_AUTO", "PRIVATE_AUTO", vehicle_type))]

replica_trip_origin_destination = fread(
  here(location, folder, "replica_trip_origin_destination.csv")) %>%
  data.table() %>%
  .[,`:=`(vehicle_type = fifelse(mode == "PRIVATE_AUTO", "PRIVATE_AUTO", vehicle_type))]


##agg_network_links=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NOTE: this section preaggregates and network links
#----- a number of different network volume layers are made  given different ways to aggregate the data
#----- aggregation was based on old,very limited scope projects using bounding box function
#----- i am going to print out the "old  way" any then try to iteratively kick out the with the movement patterns
zz = aggregate_network_links(
  location = location
  ,folder = folder
  ,auto_save = F
  ,agg_count_object = table_agg_by_link_subset_limited
  ,network_object = replica_queried_network_links
)


tmp_1 = table_agg_by_link_subset_limited %>%
  select(mvmnt, mvmnt_seq) %>%
  unique() %>%
  # filter(mvmnt == "meridian_north_full") %>%
  group_by(mvmnt, mvmnt_seq) %>%
  nest() %>%
  mutate(spatial_list = pmap(
    list(mvmnt, mvmnt_seq )
    ,function(mvmnt, mvmnt_seq)
    {
      message(str_glue("Making for aggregate_network_links list object {mvmnt}, {mvmnt_seq}"))

      tmp_object = aggregate_network_links(
        location = location
        ,folder = folder
        ,auto_save = F
        ,agg_count_object = table_agg_by_link_subset_limited %>%
          filter(mvmnt == mvmnt
                 ,mvmnt_seq == mvmnt_seq)
        ,network_object = replica_queried_network_links
      )

      return(tmp_object)
    }
  )) %>%
  select(!data)

test_object = tmp_1 %>%
  unnest(cols = spatial_list) %>%
  mutate(type = rep(c("agg_link", "agg_link_flag", "agg_link_vehicle_type"#, "agg_link_vehicle_and_trip_type"
                      , "agg_link_vehicle_type_origin"), 1)
         ,agg_col = rep(c(NA, "flag_trip_type", "vehicle_type"#, "map_var"
                          , NA), 1))

yolo = test_object %>%
  filter(type == "agg_link_flag") %>%
  # .[1, 3] %>%
  # unnest(cols = c(spatial_list))
  mutate(map = pmap(
    list(mvmnt, spatial_list, type, agg_col)
    ,function(mvmnt, spatial_list, type, agg_col) {

     print(agg_col)
      tmp_map = viz_robust_sttcNtwrk_map(
        spatial_agg_object = spatial_list
        ,list_index = type
        ,agg_col = agg_col
        ,list_object = F
        ,return_leaflet = T
        ,prefix = mvmnt)

      return(tmp_map)
    }))

yolo$map %>%
  reduce(`+`)

yolo %>%
  .[["map"]]

# aggregated_network_links = zz
aggregated_network_links = read_rds(
  here(location, folder, "aggregated_network_links.rds"))

# aggregated_network_cntds = aggregate_network_links(
#   location = location
#   ,folder = folder
#   ,auto_save = F
#   ,agg_count_object = table_agg_by_link_subset_limited
#   ,network_object = replica_queried_network_cntds
# )



###data write-out=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export_agg_links_to_spatial(
  data = aggregated_network_links
  ,location = location
  ,folder = folder
)

##inspect processed data=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section contains functions that make it easy to inspect the data made above
inspect_queried_network(
  location = location
  ,folder = folder
)

##OD stuff from origin links====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
map_replica_od_layers(
  acquired_sa_polys = acquired_sa_polys
  ,replica_trip_origin_destination = replica_trip_origin_destination
)

htmlwidgets::saveWidget(temp_od
                        ,here(location, folder,  "viz", "freight_origins_map.html"))

export_agg_links(
  data = list(
    "origin_poly_trips" = temp_agg
    ,"origin_poly_trips_by_veh" = temp
  )
  ,location = location
  ,folder = folder
)


##static link maps==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
replicaToolkitR::aggregate_network_links()

static_vis_anlt = viz_static_ntwrk_map_anlt(
  spatial_agg_object = aggregated_network_links
)

static_vis_anltpt = viz_static_ntwrk_map_anltpt(
  spatial_agg_object = aggregated_network_links
  ,return_leaflet = F
)

static_vis_anlto = viz_static_ntwrk_map_anlto_grp(
  spatial_agg_object = aggregated_network_links
)

# htmlwidgets::saveWidget(
#   static_vis_anlt
#   ,here(location, folder, "viz", str_glue("static_vis_anlt.html")))
#
# list(
#   list(static_vis_anlt#, static_vis_anltpt, static_vis_anlto
#        )
#   ,list("static_vis_anlt"#, "static_vis_anltpt", "static_vis_anlto"
#         )
# ) %>%data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#   pmap(~{
#     htmlwidgets::saveWidget(.x
#                             ,here(location, folder, "viz", str_glue("{.y}.html")))
#   })

htmlwidgets::saveWidget(static_vis_anlto@map
                        ,here(location, folder, "viz", str_glue("static_vis_anlto.html")))


##dynamic HTML widget maps======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynamic_map_anlt = make_network_map_anlt(
  network_cntrd_object = aggregated_network_links
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)

make_network_map_anltpt(
  network_cntrd_object = aggregated_network_links
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)

make_network_map_anlto(
  network_cntrd_object = aggregated_network_links
  ,poi_list = poi_list
  ,origin_polys = acquired_sa_polys
)

replicaToolkitR::check_origin_counts(
  location = location
  ,folder = folder
)





