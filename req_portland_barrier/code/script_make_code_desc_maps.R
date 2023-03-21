
crash = here::here("req_portland_barrier/data"
                   ,"KBell_HV_ClackMultWash_2016_2020_20220519.xlsx") %>%
  readxl::read_excel()


# crash %>%
#   select(starts_with("VHCL_TYP")) %>%
#   unique()




index_vchl_event = bind_rows(
  crash %>%
    select(starts_with("VHCL_EVNT_1")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_1"))
  ,crash %>%
    select(starts_with("VHCL_EVNT_2")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_2"))
  ,crash %>%
    select(starts_with("VHCL_EVNT_3")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_3"))
) %>%
  unique()

index_crash_cause = bind_rows(
  crash %>%
    select(starts_with("CRASH_CAUSE_1")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_1")) %>%
    mutate(across(everything(), as.character))
  ,crash %>%
    select(starts_with("CRASH_CAUSE_2")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_2")) %>%
    mutate(across(everything(), as.character))
  ,crash %>%
    select(starts_with("CRASH_CAUSE_3")) %>%
    unique() %>%
    rename_with(~str_remove_all(.x, "_3")) %>%
    mutate(across(everything(), as.character))
) %>%
  unique()

index_vchl_cause = crash %>%
  select(starts_with("VHCL_CAUSE_1")) %>%
  unique()

index_mvmnt = crash %>%
  select(starts_with("MVMNT_")) %>%
  unique()

index_collision_type = crash %>%
  select(starts_with("COLLIS_TYP")) %>%
  unique()

index_crash_severity =  crash %>%
  select(starts_with("CRASH_SVRTY")) %>%
  unique()




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
