
map.change_province <- function(m.change.province,
                                folder="results/maps/",
                                zh_en="en",
                                width=10,
                                height=7){

  g <- data.gadm.wneighbours(level=1)
  l <- max(abs(m.change.province$`2021Q1_vs_2019Q1`),abs(m.change.province$`2020Q4_vs_2019Q4`))

  qf <- "2021Q1"
  qi <- "2019Q1"

  map_q <- function(qf, qi, filename){

    # One color scale for all (before filtering for region)
    field <- sprintf("%s_vs_%s", qf, qi)
    m.plot <- g %>%
      left_join(m.change.province,
                by=c("NAME_1"="province")) %>%
      dplyr::rename(change=all_of(field))


    m.plot$nudge_x <- recode(m.plot$NAME_1,
                           "Gansu"= 1,
                           "Tianjin"=1,
                           "Guizhou"=1,
                           "Nei Mongol"=-2,
                           .default=0)
    m.plot$nudge_y <- recode(m.plot$NAME_1,
                           "Gansu"=-3,
                           "Hebei"=-1,
                           "Guangdong"=0.5,
                           "Nei Mongol"=-1,
                           "Heilongjiang"=-1,
                           .default=0)


    p <- ggplot(m.plot) +
      geom_sf(data=m.plot %>% filter(GID_0=="CHN"),
              aes(fill=change), size=0.1) +
      geom_sf(data=m.plot %>% filter(GID_0!="CHN"),
              fill="#808080", size=0.1, inherit.aes = F) +
      geom_sf_text(aes(label=ifelse(is.na(change),
                                    "",
                                    paste0(NAME_1,"\n",
                                           ifelse(change>0,"+",""),
                                           round(change*100),"%"))),

                      nudge_x =m.plot$nudge_x,
                      nudge_y= m.plot$nudge_y,
                   size=2.5,
                   lineheight=0.8) +
      rcrea::theme_crea() +
      labs(title=sprintf("PM 2.5 concentration: %s vs %s",qf,qi),
           y=NULL,
           x=NULL,
           caption="Source: CREA") +
      rcrea::theme_crea() +
      theme(panel.background = element_rect(fill="#A0CFDF"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_fill_distiller(palette="RdBu",
                           limits=c(-l,l),
                           na.value = "#AFAFAF",
                           labels=scales::percent,
                           name=sprintf("%s\nvs\n%s",qf,qi)) +
      coord_sf(xlim=c(72, 136),
               ylim=c(17, 55),
               expand=F)

    ggsave(file.path(folder, filename), p,
           width=width,
           height=height)
  }

  map_q("2021Q1","2019Q1","change_province_q1.png")
  map_q("2020Q4","2019Q4","change_province_q4.png")
}

map.change_city <- function(m.change.city,
                            folder="results/maps/",
                            width=10,
                            height=10){

  g <- data.gadm()

  m.change.city.sf <- m.change.city %>%
    left_join(rcrea::cities(id=unique(m.change.city$location_id),
                            with_geometry=T) %>%
                dplyr::select(location_id=id, geometry)) %>%
    sf::st_as_sf()

  l <- max(abs(m.change.city.sf$`2021Q1_vs_2019Q1`),abs(m.change.city.sf$`2020Q4_vs_2019Q4`))

  p1 <- ggplot(m.change.city.sf) +
    geom_sf(aes(color=`2021Q1_vs_2019Q1`)) +
    geom_sf(data=g, color="grey", fill="transparent",inherit.aes = F) +
    scale_color_distiller(palette="RdBu",
                         limits=c(-l,l),
                         name=NULL,
                         labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2021Q1 vs 2020Q1")

  ggsave(file.path(folder,"change_city_q1.png"), p1,
         width=width,
         height=height)

  p4 <- ggplot() +
    geom_sf(data=g, color="grey", fill="transparent",inherit.aes = F) +
    geom_sf(data=m.change.city.sf, aes(color=`2020Q4_vs_2019Q4`)) +
    scale_color_distiller(palette="RdBu",
                          limits=c(-l,l),
                          name=NULL,
                          labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2020Q4 vs 2019Q4")

  ggsave(file.path(folder,"change_city_q4.png"), p4,
         width=width,
         height=height)

}

map.change_city_pols <- function(m.change.city,
                            folder="results/maps/",
                            width=10,
                            height=10){

  g <- data.gadm.wneighbours(level=2) #GADM2 assimilated to cities
  l <- max(abs(m.change.city$`2021Q1_vs_2019Q1`),abs(m.change.city$`2020Q4_vs_2019Q4`),na.rm=T)

  qf <- "2021Q1"
  qi <- "2019Q1"

  map_q <- function(qf, qi, filename){

    # One color scale for all (before filtering for region)
    field <- sprintf("%s_vs_%s", qf, qi)
    m.pts <- m.change.city %>%
      left_join(rcrea::cities(id=unique(m.change.city$location_id),
                              with_geometry=T) %>%
                  dplyr::select(location_id=id, geometry)) %>%
      sf::st_as_sf() %>%
      dplyr::rename(change=all_of(field))

    m.plot <- g %>%
      sf::st_join(m.pts) %>%
      group_by(GID_0, GID_1, GID_2, NAME_2) %>%
      summarise(change=mean(change, na.rm=T))


    p <- ggplot() +
      geom_sf(data=m.plot %>% filter(GID_0=="CHN"),
              aes(fill=change), size=0.1) +
      geom_sf(data=m.plot %>% filter(GID_0!="CHN"),
              fill="#808080", size=0.1, inherit.aes = F) +
      rcrea::theme_crea() +
      theme(panel.background = element_rect(fill="#A0CFDF"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_fill_distiller(palette="RdBu",
                           limits=c(-l,l),
                           na.value = "#AFAFAF",
                           labels=scales::percent,
                           name=sprintf("%s\nvs\n%s",qf,qi)) +
      coord_sf(xlim=c(72, 136),
               ylim=c(17, 55),
               expand=F) +
      labs(title=sprintf("Change in PM2.5 levels: %s vs %s",qf,qi),
           caption="Source: CREA")


    ggsave(file.path(folder, filename), p,
         width=width,
         height=height)
  }

  map_q("2021Q1","2019Q1","change_city_polygons_Q1.png")
  map_q("2020Q4","2019Q4","change_city_polygons_Q4.png")

}

map.hp_city_pols <- function(m.hp.city,
                                 folder="results/maps/",
                                 width=10,
                                 height=7){

  g <- data.gadm.wneighbours(level=2) #GADM2 assimilated to cities
  # l <- max(abs(m.change.city$`2021Q1_vs_2019Q1`),abs(m.change.city$`2020Q4_vs_2019Q4`),na.rm=T)

  map_q <- function(filename){

    # One color scale for all (before filtering for region)

    m.pts <- m.hp.city %>%
      left_join(rcrea::cities(id=unique(m.change.city$location_id),
                              with_geometry=T) %>%
                  dplyr::select(location_id=id, geometry)) %>%
      sf::st_as_sf()

    m.plot <- g %>%
      sf::st_join(m.pts) %>%
      group_by(GID_0, GID_1, GID_2, NAME_2) %>%
      summarise(count=max(count, na.rm=T)) %>%
      mutate(count=cut(count,
                       breaks=c(-1,0,5,10,15,20,25,Inf),
                       labels=c("0","1-5","6-10","11-15",
                                "16-20","21-25", ">25")))


    p <- ggplot() +
      geom_sf(data=m.plot %>% filter(GID_0=="CHN"),
              aes(fill=count), size=0.1) +
      geom_sf(data=m.plot %>% filter(GID_0!="CHN"),
              fill="#808080", size=0.1, inherit.aes = F) +
      rcrea::theme_crea() +
      theme(panel.background = element_rect(fill="#A0CFDF"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_fill_brewer(palette="RdYlBu",
                        direction = -1,
                           # limits=c(-l,l),
                           na.value = "#AFAFAF",
                        name=NULL,
                           # labels=scales::percent,
                           # name=sprintf("%s\nvs\n%s",qf,qi)
                           ) +
      coord_sf(xlim=c(72, 136),
               ylim=c(17, 55),
               expand=F) +
      labs(
        # title=sprintf("Number of heavy pollution days in March 2020 - March 2021"),
           caption="Source: CREA")

    ggsave(file.path(folder, filename), p,
           width=width,
           height=height)
  }

  map_q("hp_city.png")

}

map.change_city_keyregion <- function(m.change.city,
                            folder="results/maps/",
                            width=9,
                            height=7){

  g <- data.gadm.wneighbours(lev=2) %>%
    sf::st_transform(3857)

  kr <- "2+26"
  kr.name <- "2+26"
  qf <- "2021Q1"
  qi <- "2019Q1"


  map_q_kr <- function(qf, qi, kr, kr.name, filename){

    # One color scale for all (before filtering for region)
    l <- max(abs(m.change.city$`2021Q1_vs_2019Q1`), abs(m.change.city$`2020Q4_vs_2019Q4`), na.rm=T)
    margin <- 1e5 #0.5

    field <- sprintf("%s_vs_%s", qf, qi)
    m.plot <- m.change.city %>%
      filter(keyregion2018==kr) %>%
      left_join(rcrea::cities(id=unique(m.change.city$location_id),
                              with_geometry=T) %>%
                  dplyr::select(location_id=id, geometry, name)) %>%
      sf::st_as_sf() %>%
      rename(change=all_of(field)) %>%
      sf::st_transform(3857)

    m.plot$change_str <- paste0(round(m.plot$change*100),"%")
    m.plot$label <- sprintf("%s\n%s%s", m.plot$name, ifelse(m.plot$change>0,"+",""), m.plot$change_str)


    # Make width height ratio fixed by user, so that all regions look alike in doc
    bbox <- sf::st_bbox(m.plot) + c(-margin,-margin,margin,margin)
    wh.obs <- (bbox[3]-bbox[1])/(bbox[4]-bbox[2])
    wh.tgt <- width / height
    bbox.tgt <- tmaptools::bb(bbox,
                              height=max(1,wh.obs/wh.tgt),
                              width=max(1,wh.tgt/wh.obs),
                              relative = T)
    wh.tgt2 <- (bbox.tgt[3]-bbox.tgt[1])/(bbox.tgt[4]-bbox.tgt[2])
    print(wh.tgt2)

    m.plot.g <- g %>% sf::st_join(m.plot)

    m.plot.g$nudge_x <- recode(m.plot.g$name,
                             "Wuhu"=0.4,
                             .default=0,
                             .missing=0) * 0.5e5
    m.plot.g$nudge_y <- recode(m.plot.g$name,
                               "Xianyang"=-0.5,
                               "Wuhu"=0.1,
                             "Wuxi"=0.1,
                             "Zhenjiang"=0.2,
                             .default=0,
                             .missing = 0) * 0.5e5


    (p <- ggplot(m.plot.g) +
        geom_sf(aes(fill=change),
                color="grey",
                inherit.aes = F) +
        geom_sf_text(
          # data=sf::st_centroid(sf::st_crop(g, bbox.tgt)),
                        # filter(!NAME_2 %in% c("Beijing","Tianjin")),
          aes(label=label),
          size=3.5,
          lineheight=0.8,
          nudge_x = m.plot.g$nudge_x,
          nudge_y = m.plot.g$nudge_y,
          # color="darkgrey",
          fill="transparent",
          inherit.aes = F) +

          # geom_sf_text(aes(label=label),
          #            # vjust=m.plot$vjust,
          #            # hjust=m.plot$hjust,
          #            lineheight=0.8,
          #            size=3.5) +
        # geom_sf(aes(color=change), show.legend = F) +
        scale_fill_distiller(palette="RdBu",
                              limits=c(-l,l),
                              name=NULL,
                              labels=scales::percent) +
        scale_color_distiller(palette="RdBu",
                              limits=c(-l,l),
                              name=NULL,
                              labels=scales::percent) +
        coord_sf(xlim = c(bbox.tgt[1], bbox.tgt[3]),
                 ylim = c(bbox.tgt[2], bbox.tgt[4]),
                 expand = F)+
        rcrea::theme_crea() +
        theme(panel.background = element_rect(fill="#A0CFDF"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title=sprintf("PM 2.5 concentration in %s: %s vs %s", kr.name, qf, qi),
             caption="Source: CREA",
             x=NULL, y=NULL))

    ggsave(file.path(folder, filename), plot=p, width=width, height=height)
    print(p)
    return(p)
  }

  map_q_kr("2021Q1","2019Q1","2+26","2+26 cities","change_city_226_Q1.png")
  map_q_kr("2020Q4","2019Q4","2+26","2+26 cities","change_city_226_Q4.png")
  map_q_kr("2021Q1","2019Q1","YRD","YRD","change_city_yrd_Q1.png")
  map_q_kr("2020Q4","2019Q4","YRD","YRD","change_city_yrd_Q4.png")
  map_q_kr("2021Q1","2019Q1","Fenwei","Fenwei","change_city_fenwei_Q1.png")
  map_q_kr("2020Q4","2019Q4","Fenwei","Fenwei","change_city_fenwei_Q4.png")
}


map.change_keyregion <- function(meas){

}

map.change_interpolated <- function(meas, res=0.1, max=0.6){

  library(gstat)
  library(raster)
  library(magrittr)

  m.city <- meas %>%
    filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2019Q1"),
           !sand_storm) %>%
    group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
    summarise_at(c("pm25"), mean, na.rm=T) %>%
    tidyr::spread("quarter","pm25")

  m.city$new1 <- m.city$`2021Q1`
  m.city$old1 <- m.city$`2019Q1`
  m.city$new4 <- m.city$`2020Q4`
  m.city$old4 <- m.city$`2019Q4`

  m.city.sf <- m.city %>%
    left_join(rcrea::cities(id=unique(m.city$location_id),
                            with_geometry=T) %>%
                dplyr::select(location_id=id, geometry)) %>%
    sf::st_as_sf()

  g <- data.gadm()
  r <- raster::raster(g, resolution=res)


  # r.idw.new1 <- gstat::idw(new1 ~ 1, as(m.city.sf %>% filter(!is.na(new1)), "Spatial"), newdata=as(r, "SpatialPixels")) %>% raster::raster() %>% raster::mask(g)
  r.interp.new1 <- utils.interp.ok(r, m.city.sf %>% rename(value=new1), mask=g)
  r.interp.old1 <- utils.interp.ok(r, m.city.sf %>% rename(value=old1), mask=g)
  r.interp.new4 <- utils.interp.ok(r, m.city.sf %>% rename(value=new4), mask=g)
  r.interp.old4 <- utils.interp.ok(r, m.city.sf %>% rename(value=old4), mask=g)

  r.change1 <- r.interp.new1/r.interp.old1 -1
  r.change4 <- r.interp.new4/r.interp.old4 -1

  d.plot <- list(r.change4, r.change1) %>%
    lapply(function(x) x %>% min(max_abs) %>% max(-max_abs) %>% multiply_by(100)) %>% raster::stack()

  (plt.contour <-  d.plot %>%
    rasterVis::levelplot(names.attr=c(
      "2020Q4 vs 2019Q4",
      "2021Q1 vs 2019Q1"),
                         main=NULL,
       xlab=NULL, ylab=NULL,
      scales=list(x=list(draw=F),
                  y=list(draw=FALSE)),
                         par.settings = rasterVis::BuRdTheme(
                           layout.widths = list(axis.key.padding = 0,
                                                ylab.right = 2)),
                         at=seq(-max_abs*100, max_abs*100, length.out=100),
                         ylab.right = "Change in percentage"

    )  +
    latticeExtra::layer(sp::sp.lines(as(g,"Spatial"), col='gray'),
                        data=list(g=g))
    )

  return(plt.contour)
}


#' Map trajectories
#'
#' @param trajs
#' @param location_id
#' @param location_name
#' @param date
#' @param meas
#' @param filename
#' @param met_type
#' @param duration_hour
#' @param height
#' @param fires
#' @param basemap
#' @param add_fires
#' @param fire_raster
#' @param powerplants
#' @param add_plot
#' @param folder
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
map.trajs <- function(trajs, location_id, location_name, date,
                      meas, filename, met_type, duration_hour, height,
                      fires=NULL, basemap=NULL,
                      add_fires=F, fire_raster=NULL, powerplants=NULL, add_plot=NULL, ...){

  if(!is.null(powerplants)){
    powerplants$geometry <- st_centroid(powerplants$geometry)
  }

  tryCatch({
    source_legend <- if(!is.null(meas)) paste0(rcrea::source_str(unique(meas$source)), collapse=",") else ""


    # For powerplants and active fires
    dot_values <- c()
    dot_colors <- c()

    # Get measurements values in subtitle
    subtitle_poll <- ifelse(!is.null(meas),
                            paste0(rcrea::poll_str(meas$poll)," level: ",round(meas$value)," ",meas$unit,collapse=". "),
                            "")

    subtitle <- paste0(date, ". ", subtitle_poll)

    m <- ggmap(basemap) +
      coord_cartesian() +
      # geom_point(data=wri_power %>% dplyr::filter(country=="IDN"), inherit.aes = F, aes(x=longitude,y=latitude),
      #            shape=2, stroke=1.5, color='darkred') +
      # geom_point(data=ct %>% dplyr::filter(country=="Indonesia"), inherit.aes = F, aes(x=lng,y=lat),
      #            shape=2, stroke=1.5, color='darkred') +

      # Cluster trajectories
      geom_path(data = trajs %>%
                  dplyr::arrange(hour_along) %>%
                  mutate(subcluster=paste(traj_dt_i, hour_along %/% 8)),
                arrow = ggplot2::arrow(angle=18, length=ggplot2::unit(0.1,"inches")),
                aes(x = lon, y = lat, group=subcluster), color="darkred", alpha=0.6) +

      geom_path(data = trajs,
                aes(x = lon, y = lat, group=traj_dt_i), color="darkred", alpha=0.6) +

      # geom_line(data = trajs_meas %>% dplyr::filter(value>=threshold) , aes(x = lon, y = lat, group=traj_dt_i), alpha=0.6, color='darkred')+

      rcrea::theme_crea() +
      ggplot2::theme(panel.background = element_rect(fill='lightgray'),
                     panel.border = element_rect(color='black', fill=NA),
                     panel.grid = element_line(color=NA),
                     plot.caption = element_text(lineheight = 0.9),
                     # legend.key = element_rect(fill='white'),
                     legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.margin=margin(0,0,0,0),
                     legend.box.margin=margin(-20,0,10,0)) +
      scale_shape_manual(name="Sector", values=c(0,1,2,3,4,5)) +
      labs(title=paste0("Sources of air flowing into ", location_name),
           subtitle = subtitle,
           x='', y='',
           caption=paste0("CREA based on ",source_legend, ifelse(add_fires,", VIIRS and HYSPLIT.\nSize reflects the maximum fire intensity.\n",""),
                          "HYSPLIT parameters: ", duration_hour,"h | ",met_type," | ",height,"m." ))

    if(add_fires){

      if(!is.null(fire_raster)){
        bb <- ggmap::bb2bbox(attr(basemap, "bb"))
        bb <- as.numeric(bb)
        names(bb) <- c("xmin","ymin","xmax","ymax")
        crop <- sf::st_as_sfc(st_bbox(bb)) %>% sf::st_set_crs(4326) %>% sf::st_transform(crs=attr(fire_raster,"crs"))
        r.cropped <- raster::crop(fire_raster, as(crop, 'Spatial'))

        # r.cropped.max <- calc(r.cropped, function(x) max(x, na.rm = TRUE))
        fire_pol <- do.call("rbind", lapply(unstack(r.cropped),
                                            FUN=function(x){tryCatch({p <- rasterToPolygons(x); names(p)="fire"; p},
                                                                     error=function(c){NULL})}))


        if(!is.null(fire_pol)){
          m <- m + geom_sf(data=st_as_sf(fire_pol),
                           inherit.aes = F,
                           fill="blue",
                           color="blue")
        }
      }

      if(nrow(fires %>% filter(!is.na(acq_date)))>0){
        frp.min <- 0
        frp.max <- 8

        fires$frp <- min(fires$frp, frp.max)
        fires$frp <- max(fires$frp, frp.min)

        m <- m + geom_point(data=fires, inherit.aes = F,
                            aes(x=st_coordinates(geometry.fire)[,1],
                                y=st_coordinates(geometry.fire)[,2],
                                size=frp,
                                color="Active fire"),
                            fill="orange",
                            shape="triangle",
                            stroke=1,
                            alpha=0.8,
                            position="jitter") +
          scale_size_continuous(range=c(1,9), limits=c(frp.min, frp.max), guide="none")

        dot_values <- c(dot_values, "Active fire")
        dot_colors <- c(dot_colors, "red")
      }
    }

    if(!is.null(powerplants)){

      m <- m + geom_point(data=powerplants, inherit.aes = F,
                          aes(x=st_coordinates(st_centroid(geometry))[,1],
                              y=st_coordinates(st_centroid(geometry))[,2],
                              size=frp,
                              color="Thermal power plant"),
                          shape="triangle",
                          size=2,
                          stroke=1,
                          alpha=0.8,
                          position="jitter", show.legend = T)

      dot_values <- c(dot_values, "Thermal power plant")
      dot_colors <- c(dot_colors, "black")
    }

    m <- m +
      scale_color_manual(name=NULL,
                         values=dot_colors,
                         breaks=dot_values)

    if(!is.null(add_plot)){
      m <- m + add_plot
    }


    ggsave(plot=m,
           filename = filename,
           width=8,
           height=7)

    return(filename)
  }, error=function(c){
    warning(paste("Error on  ", location_id, date))
    return(NA)
  })
}


map.windrose <- function(meas, weather, filename, met_type, duration_hour, height){

  capitals <- data.capitals(meas)
  date_from <- "2020-10-01"
  date_to <- "2021-03-31"


  hp.meas <-  capitals %>%
    left_join(meas) %>%
    filter(heavy_polluted) %>%
    filter(date>=date_from) %>%
    filter(date<=date_to)

  hp.weather <- hp.meas %>%
    distinct(location_id, location_name, date) %>%
    left_join(weather)

  g <- data.gadm()
  g.3857 <- sf::st_transform(g, 3857)


  ggmap::register_google(key=Sys.getenv("GOOGLE_MAP_API_KEY"))

  map <- ggmap::get_map(
    location = sf::st_bbox(g) %>% set_names(c("left","bottom","right","top")),
    # "China",
    zoom = 5,
    # maptype = "terrain",
    source="stamen"
  )

  # overwrite the bbox of the ggmap object with that from the uk map that has
  # been transformed to 3857
  attr(map, "bb")$ll.lat <- st_bbox(g.3857)["ymin"]
  attr(map, "bb")$ll.lon <- st_bbox(g.3857)["xmin"]
  attr(map, "bb")$ur.lat <- st_bbox(g.3857)["ymax"]
  attr(map, "bb")$ur.lon <- st_bbox(g.3857)["xmax"]

  m <- ggmap(map, extent = sf::st_bbox(g)) +
    coord_sf(crs = st_crs(3857)) # force the ggplot2 map to be in 3857
    # geom_sf(data = g, fill="transparent", inherit.aes = FALSE)



  for(i in seq(nrow(capitals))){

    tryCatch({
      p <- plot.windrose(hp.meas,
                         hp.weather,
                         location_id=capitals$location_id[i],
                         location_name=capitals$location_name[i])
      # geometry <- hp.weather[hp.weather$location_id==capitals$location_id[i],"geometry"][1,] %>% pull()
      coords <- hp.weather[hp.weather$location_id==capitals$location_id[i],"geometry"][1,] %>%
        sf::st_as_sf() %>%
        sf::st_transform(3857) %>%
        st_coordinates()

      x = coords[1]
      y = coords[2]
      size=250000

      m <- m+annotation_custom(ggplotGrob(p),
                               xmin=x-size,
                               ymin=y-size,
                               xmax=x+size,
                               ymax=y+size)
    },error=function(e){})
  }


  m

}


