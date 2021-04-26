
map.change_province <- function(m.change.province,
                                folder="results/maps/",
                                width=10,
                                height=10){

  g <- data.gadm()
  m.change.province.sf <- g %>%
    # mutate(gadm1_id=tolower(GID_1)) %>%
    left_join(m.change.province,
              by=c("NAME_1"="province"))

  l <- max(abs(m.change.province.sf$`2021Q1_vs_2019Q1`),abs(m.change.province.sf$`2020Q4_vs_2019Q4`))

  p1 <- ggplot(m.change.province.sf) +
    geom_sf(aes(fill=`2021Q1_vs_2019Q1`)) +
    geom_sf_text(aes(label=ifelse(is.na(`2021Q1_vs_2019Q1`),
                              "",
                              paste0(round(`2021Q1_vs_2019Q1`*100),"%"))),
                 size=3) +
    scale_fill_distiller(palette="RdBu",
                         limits=c(-l,l),
                         name=NULL,
                         labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2021Q1 vs 2019Q1",
         y=NULL,
         x=NULL)

  ggsave(file.path(folder,"change_province_q1.png"), p1,
         width=width,
         height=height)


  p4 <- ggplot(m.change.province.sf) +
    geom_sf(aes(fill=`2020Q4_vs_2019Q4`)) +
    geom_sf_text(aes(label=ifelse(is.na(`2020Q4_vs_2019Q4`),
                                  "",
                                  paste0(round(`2020Q4_vs_2019Q4`*100),"%"))),
                 size=3) +
    scale_fill_distiller(palette="RdBu",
                         limits=c(-l,l),
                         name=NULL,
                         labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2020Q4 vs 2019Q4",
         y=NULL,
         x=NULL)

  ggsave(file.path(folder,"change_province_q4.png"), p4,
         width=width,
         height=height)

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

map.change_city_keyregion <- function(m.change.city,
                            folder="results/maps/",
                            width=9,
                            height=7){

  g <- data.gadm()%>%
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
    m.plot$label <- sprintf("%s\n%s", m.plot$name, m.plot$change_str)
    m.plot$hjust <- recode(m.plot$name,
                             "Zhengzhou"=1.2,
                             "Hebi"=1.2,
                             "Xianyang"=1.2,
                             .default=0.5)
    m.plot$vjust <- recode(m.plot$name,
                             "Zhengzhou"=0.5,
                             "Hebi"=0.5,
                           "Xianyang"=0.5,
                             .default=-0.3)

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

    (p <- ggplot(m.plot) +
        geom_sf(data=g, aes(label=NAME_1), color="grey", fill="#DFDFDF", inherit.aes = F) +
        geom_sf_text(data=sf::st_centroid(sf::st_crop(g, bbox.tgt)) %>%
                       filter(!NAME_1 %in% c("Beijing","Tianjin")),
                     aes(label=NAME_1), color="darkgrey", fill="transparent",inherit.aes = F) +
        geom_sf_text(aes(label=label),
                     vjust=m.plot$vjust,
                     hjust=m.plot$hjust,
                     lineheight=0.8,
                     size=3.5) +
        geom_sf(aes(color=change), show.legend = F) +
        scale_color_distiller(palette="RdBu",
                              # limits=c(-l,l),
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

  map_q_kr("2021Q1","2019Q1","2+26","2+26 cities","change_city_2+26_Q1.png")
  map_q_kr("2020Q4","2019Q4","2+26","2+26 cities","change_city_2+26_Q4.png")
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
map.trajs <- function(trajs, location_id, location_name,
                      filename, met_type, duration_hour, height,
                      basemap,
                      add_no2=F,
                      plot.width=9,
                      plot.height=7,
                      add_plot=NULL, ...){


  tryCatch({

    library(geosphere)


    # Filter trajs with below 700m
    alt.max <- 700
    trajs$ID = group_indices(trajs, traj_dt_i)
    trajs %>%
      group_by(ID) %>%
      arrange(desc(hour_along)) %>%
      do(
      mutate(.,
             maxheight.sofar=cummax(height),
             speed=tidyr::replace_na(distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat)))/1000,0),
             maxspeed.sofar=cummax(speed))
      ) %>%
      filter(maxheight.sofar <= alt.max,
             maxspeed.sofar <= 40) %>%
      filter(lubridate::hour(date) %in% 3:21) %>%
      sf::st_as_sf(coords=c("lon","lat")) %>%
      sf::st_set_crs(4326) -> t


    # Make width height ratio fixed by user, so that all regions look alike in doc
    margin <- 0.2
    bb <- sf::st_bbox(t) + c(-margin,-margin,margin,margin)
    wh.obs <- (bb[3]-bb[1])/(bb[4]-bb[2])
    wh.tgt <- plot.width / plot.height
    bb.tgt <- tmaptools::bb(bb,
                              height=max(1,wh.obs/wh.tgt),
                              width=max(1,wh.tgt/wh.obs),
                              relative = T)

    bb.tgt.3857 <- sf::st_transform(sf::st_as_sfc(bb.tgt), 3857) %>%
      sf::st_bbox()

    ggmap::register_google(key=Sys.getenv("GOOGLE_MAP_API_KEY"))

    basemap <- ggmap::get_map(
      location = bb.tgt %>% set_names(c("left","bottom","right","top")),
      zoom = 6,
      # maptype = "terrain",
      source="stamen"
    )

    # overwrite the bbox of the ggmap object with that from the 3857 map
    # bb <- attr(map, "bb")
    # bb.3857 <- ggmap::make_bbox(bb)
    attr(basemap, "bb")$ll.lat <- bb.tgt.3857["ymin"]
    attr(basemap, "bb")$ll.lon <- bb.tgt.3857["xmin"]
    attr(basemap, "bb")$ur.lat <- bb.tgt.3857["ymax"]
    attr(basemap, "bb")$ur.lon <- bb.tgt.3857["xmax"]


    m <- ggmap(basemap, extent=bb.tgt, padding=0) +
      coord_sf(crs = st_crs(3857))



    t.3857 <- t %>%
      sf::st_transform(3857) %>%
      rowwise() %>%
      mutate(lat=sf::st_coordinates(geometry)[[2]],
             lon=sf::st_coordinates(geometry)[[1]]
      )

    if(add_no2){
      no2_raster <- raster::raster("data/chinaNO2Winter2020_10km.tif")
      no2_raster_3857 <- raster::projectRaster(no2_raster, crs=3857)
      no2_df <- as.data.frame(no2_raster_3857, xy = TRUE)
      m <- m + geom_raster(data = no2_df,
                           aes(x = x, y = y, fill = chinaNO2Winter2020_10km), alpha=0.4) +
        scale_fill_distiller(palette="RdBu")
    }

    m <- m +
      # coord_cartesian(
      #   xlim=c(min(trajs.filtered$lon)-margin, max(trajs.filtered$lon)+margin),
      #   ylim=c(min(trajs.filtered$lat)-margin, max(trajs.filtered$lat)+margin),
      #
      # ) +
      # geom_point(data=wri_power %>% dplyr::filter(country=="IDN"), inherit.aes = F, aes(x=longitude,y=latitude),
      #            shape=2, stroke=1.5, color='darkred') +
      # geom_point(data=ct %>% dplyr::filter(country=="Indonesia"), inherit.aes = F, aes(x=lng,y=lat),
      #            shape=2, stroke=1.5, color='darkred') +

      # Cluster trajectories
      # geom_path(data = trajs.filtered %>%
      #             dplyr::arrange(hour_along) %>%
      #             mutate(subcluster=paste(traj_dt_i, hour_along %/% 8)),
      #           arrow = ggplot2::arrow(angle=18, length=ggplot2::unit(0.1,"inches")),
      #           aes(x = lon, y = lat, group=subcluster), color="darkred", alpha=0.6) +

      geom_path(data = t.3857,
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
           x='', y='',
           caption=paste0("CREA based on MEE and HYSPLIT.\n",
                          "HYSPLIT parameters: ", duration_hour,"h | ",met_type," | ",height,"m." ))


    if(!is.null(add_plot)){
      m <- m + add_plot
    }



    m <- m +
      coord_cartesian(xlim = c(bb.tgt.3857[1], bb.tgt.3857[3]),
               ylim = c(bb.tgt.3857[2], bb.tgt.3857[4]),
               expand = F)

    ggsave(plot=m,
           filename = filename,
           width=plot.width,
           height=plot.height)

    return(filename)
  }, error=function(c){
    warning(paste("Error on  ", location_id))
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

  no2_raster <-
    raster::raster("data/chinaNO2Winter2020_10km.tif")

  no2_raster_3857 <- raster::projectRaster(no2_raster, crs=3857)

  no2_df <- as.data.frame(no2_raster_3857, xy = TRUE)
  m <- m + geom_raster(data = no2_df,
              aes(x = x, y = y, fill = chinaNO2Winter2020_10km), alpha=0.8) +
    scale_fill_distiller(palette="RdBu")

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
    },error=function(e){print(e)})
  }


  m

}


