#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
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

crash = here(location, "KBell_HV_ClackMultWash_2016_2020_20220519.xlsx") %>%
  readxl::read_excel() %>%
  st_as_sf(coords = c("LONGTD_DD", "LAT_DD"), crs = 4326)

tigris::

crashes_fltrd =
  crashes %>%
  st_join(network_buffer_c_pass) %>%
  st_join(network_buffer_l_pass) %>%
  mutate(across(c(flag_c_pass, flag_l_pass), ~replace_na(.x, 0))) %>%
  filter(NHS_FLG == 1 |
           flag_c_pass == 1 |
           flag_l_pass == 1) %>%
  st_filter(network_buffer) %>%
  select(NHS_FLG, HWY_NO, HWY_MED_NM, CITY_SECT_NM
         ,ends_with("_SHORT_DESC"), ends_with("_CNT")) %>%
  janitor::remove_constant()

###merge-----
crashes_fltrd_snppd = crashes_fltrd %>%
  st_join(., network,
          join = st_nearest_feature, left = T)


tmp %>%
  filter(CRASH_ID %in% index)


crashes %>%
  janitor::remove_constant()


(crashes %>%
  st_filter(project_locations_points %>%
              quick_buffer(radius = 500)) %>%
  st_jitter(.0005) %>%
    st_drop_geometry() %>%
    janitor::remove_constant() %>% view()
  mapview()) + mapview(roads_sel_buf)




tmp %>%
  glimpse()













crases_19 = "req_portland_barrier/data/data_crash/crashes2019/crashes2019.shp" %>%
  here::here()  %>%
  read_sf()


# crases_19$













#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































