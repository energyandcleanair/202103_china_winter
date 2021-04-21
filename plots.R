
plots.change_province <- function(m.change.province,
                                folder="results/plots/",
                                width=10,
                                height=10){

  m.change.province %>%
    ungroup() %>%
    dplyr::select(province, change_Q1_rel, change_Q4_rel) %>%
    tidyr::gather("quarter","change", -province) %>%
    ungroup() %>%
    mutate(quarter=recode(quarter,
                          "change_Q1_rel"="2021Q1 Vs 2020Q1",
                          "change_Q4_rel"="2020Q4 Vs 2019Q4"
                          )) %>%
    mutate(quarter = as.factor(quarter),
           province = tidytext::reorder_within(province, change, quarter),
           label=paste0(round(change*100),"%")) %>%
  ggplot(aes(x=change,
             y=province)) +
    geom_col(aes(fill="1"), show.legend = F) +
    facet_wrap(~quarter, scales = "free_y") +
    tidytext::scale_y_reordered() +
    geom_text(aes(label=label,
                  hjust=ifelse(change>0,-0.1,1.1)),
              size=3) +
    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d()+
    labs(title="PM 2.5 level changes in Chinese provinces") +
    scale_x_continuous(labels=scales::percent,
                       limits=c(-0.35,0.6))


  ggsave(file.path(folder,"bar_change_province.png"), p1,
         width=width,
         height=height)

}


plots.sandstorm_keyregion <- function(m.storm.keyregion){
  ggplot(m.storm.keyregion) +
    geom_bar(aes(season, count, fill="1"),
             stat="identity",
             show.legend = F) +
    facet_wrap(~keyregion2018) +
    scale_y_continuous(limits=c(0,10),
                       breaks=seq(0,10,2),
                       expand = expansion(mult=c(0,0.1),0)) +

    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d() +
    labs(title="Occurence of sand storms in key regions",
         subtitle="Number of sand storm days in winter",
         caption="Source: CREA",
         x=NULL, y=NULL)

  ggsave("results/plots/storm_keyregion.png", width=8, height=4)
}


plot.windrose <- function(meas,
                          weather,
                          location_id,
                          location_name,
                          dirres = 30,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){



  hp.dates <- meas %>%
    filter(location_id==!!location_id) %>%
    filter(heavy_polluted) %>%
    distinct(location_id, date)

  hp.weather <- hp.dates %>%
    left_join(weather)

  dir <- hp.weather$wd
  spd <- rep(1, length(dir))

  data <- data.frame(spd = spd,
                     dir = dir)
  spd = "spd"
  dir = "dir"

  # Tidy up input data ----
  n.in <- nrow(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  # spdseq <- seq(spdmin,spdmax,spdres)

  # get some information about the number of bins, etc.
  # n.spd.seq <- length(spdseq)
  n.colors.in.range <- length(unique(data$spd)) #n.spd.seq - 1

  # create the color map
  # spd.colors <- colorRampPalette(brewer.pal(min(max(3,
  #                                                   n.colors.in.range),
  #                                               min(9,
  #                                                   n.colors.in.range)),
  #                                           palette))(n.colors.in.range)

  # if (max(data[[spd]],na.rm = TRUE) > spdmax){
  #   spd.breaks <- c(spdseq,
  #                   max(data[[spd]],na.rm = TRUE))
  #   spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
  #                         '-',
  #                         c(spdseq[2:n.spd.seq])),
  #                   paste(spdmax,
  #                         "-",
  #                         max(data[[spd]],na.rm = TRUE)))
  #   spd.colors <- c(spd.colors, "grey50")
  # } else{
  #   spd.breaks <- spdseq
  #   spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
  #                       '-',
  #                       c(spdseq[2:n.spd.seq]))
  # }
  # data$spd.binned <- cut(x = data[[spd]],
  #                        breaks = spd.breaks,
  #                        labels = spd.labels,
  #                        ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)

  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  # deal with change in ordering introduced somewhere around version 2.2
  # if(packageVersion("ggplot2") > "2.2"){
  #   cat("Hadley broke my code\n")
  #   data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
  #   spd.colors = rev(spd.colors)
  # }

  # create the plot ----
  p.windrose <- ggplot() +
    geom_bar(data = data,aes(x = dir.binned), fill="#27a59c", color="white", size=0.3, show.legend = F) +
    geom_point(data=tibble(x=0,y=0),aes(x,y),col="white", size=0.1) +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    # scale_fill_distiller(name = "Pollution level",
    #                    palette = palette) +
    theme(axis.title.x = element_blank()) +
    theme_void() +
    theme(panel.background = element_rect(fill=rgb(1,1,1,0),linetype = 'blank')) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title=NULL)
    # labs(title=location_name)

  # adjust axes if required
  # if (!is.na(countmax)){
  #   p.windrose <- p.windrose +
  #     ylim(c(0,countmax))
  # }

  # print the plot
  print(p.windrose)

  # return the handle to the wind rose
  return(p.windrose)
}
