




index_names = names(data)

full_location = here::here(location, folder, "spatial_output")

if (!exists(full_location)){
  dir.create(full_location)
}

message(str_glue("Aggregated link RDS object elements will be saved individually at the following location:\n{paste0('./', location, '/', folder, '/', 'spatial_output')}"))

index_names %>%
  purrr::map(~{
    write_sf(
      data[[.x]]
      ,here::here(location, folder, "spatial_output", stringr::str_glue("{.x}.shp"))
    )

  })


map_anlt = (od_agg_map + od_map  + static_vis_anlt) %>%
  .@map %>%
  leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                       ,"MEDIUM Duty (trips per mi2)"))


map_anlt %>%
  htmlwidgets::saveWidget(
    here(location, folder,  "output/viz", "map_anlt.html"))


(od_agg_map + od_map  + static_vis_anltpt)

map_anltpt = (od_agg_map + od_map  + static_vis_anltpt) %>%
  .@map %>%
  leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                       ,"MEDIUM Duty (trips per mi2)"))


map_anltpt %>%
  htmlwidgets::saveWidget(
    here(location, folder,  "output/viz", "map_anltpt.html"))







