meas_files <- reactive({

  files <- list.files(deweathered.folder,
                      pattern="meas.*",
                      full.names = T)

  tibble::tibble(
    filename=files,
    location_id=gsub("meas.|.RDS",
                     "",
                     basename(files)))
})

meas_all <- reactive({
  req(meas_files())

  lapply(meas_files()$filename,
         readRDS) %>%
    dplyr::bind_rows(.) %>%
    dplyr::filter(output=="anomaly") %>%
    tidyr::unnest(normalised)
})


meas_locations <- reactive({
  req(meas_files())
  rcrea::locations(id=unique(meas_files()$location_id),
                   level=c("city"),
                   source="mee",
                   with_metadata=T,
                   with_geometry=F,
                   collect=T) %>%
    dplyr::left_join(meas_files(),
                     by=c("id"="location_id"))
})


trajs_files <- reactive({

  files <- list.files(trajs.folder,
                      pattern="trajs.*",
                      full.names = T)

  tibble::tibble(
    filename=files,
    location_id=gsub("trajs.|.RDS",
                     "",
                     basename(files)))
})


trajs_locations <- reactive({
  req(trajs_files())
  rcrea::locations(id=unique(trajs_files()$location_id),
                   level=c("city"),
                   source="mee",
                   with_metadata=T,
                   with_geometry=T,
                   collect=T) %>%
    dplyr::left_join(trajs_files(),
                     by=c("id"="location_id"))
})
