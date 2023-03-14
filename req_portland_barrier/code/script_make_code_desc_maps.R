
crash = here::here(location, "KBell_HV_ClackMultWash_2016_2020_20220519.xlsx") %>%
  readxl::read_excel()

crash %>%
  select(starts_with("VHCL_EVNT_1")) %>%
  unique() %>%
  arrange(VHCL_EVNT_1_CD)

crash %>%
  select(starts_with("VHCL_EVNT_2")) %>%
  unique() %>%
  arrange(VHCL_EVNT_2_CD) %>%
  clipr::write_clip()

crash %>%
  select(starts_with("VHCL_CAUSE_1")) %>%
  unique() %>%
  arrange(VHCL_CAUSE_1_CD) %>%
  clipr::write_clip()

crash %>%
  select(starts_with("MVMNT_")) %>%
  unique() %>%
  arrange(MVMNT_CD)   %>%
  clipr::write_clip()

crash %>%
  select(starts_with("CRASH_CAUSE_1")) %>%
  unique() %>%
  arrange(CRASH_CAUSE_1_CD) %>%
  clipr::write_clip()

crash %>%
  select(starts_with("COLLIS_TYP")) %>%
  unique() %>%
  arrange(COLLIS_TYP_CD) %>%
  clipr::write_clip()

crash %>%
  select(starts_with("CRASH_SVRTY")) %>%
  unique() %>%
  arrange(CRASH_SVRTY_CD) %>%
  clipr::write_clip()



crash %>%
  colnames() %>%
  sort()



data_list_subset$highway_number
data_list_subset %>%
  select(crash_id, record_type, route_number, route_type
         ,collision_type, crash_severity
         ,total_vehicle_count, crash_year, highway_number, highway_suffix
         ,starts_with("crash_level_cause_1"), starts_with("collision_type"), starts_with("vehicle_movement_code")
         ,starts_with("vehicle_cause_1"), starts_with("vehicle_event_1")
         ,starts_with("latitude"), starts_with("longitude")) %>%
  mutate(longitude = str_glue("-{abs(longitude_degrees)+(longitude_minutes/60)+(longitude_seconds/3600)}")
         ,latitude = str_glue("{latitude_degrees+(latitude_minutes/60)+(latitude_seconds/3600)}")) %>%
  arrange(crash_id, record_type)
