require(creadeweather)

date_from <- "2020-10-01"
date_to <- "2021-03-31"
training_end <- lubridate::date(date_from)-lubridate::years(1)-lubridate::days(1)
training_start <- training_end - lubridate::years(3)

m.dew <- creadeweather::deweather(
  source="mee",
  poll="pm25",
  output=c("trend","anomaly"),
  training_start_anomaly = training_start,
  training_end_anomaly = training_end,
  upload_results = F
)

saveRDS(m.dew)
