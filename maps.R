
map.change_province <- function(m.change.province,
                                folder="results/maps/",
                                width=10,
                                height=10){

  g <- data.gadm()
  m.change.province.sf <- g %>%
    mutate(gadm1_id=tolower(GID_1)) %>%
    left_join(m.change.province,
              by="gadm1_id")

  l <- max(abs(m.change.province.sf$change_Q1_rel),abs(m.change.province.sf$change_Q4_rel))

  p1 <- ggplot(m.change.province.sf) +
    geom_sf(aes(fill=change_Q1_rel)) +
    geom_sf_text(aes(label=paste0(round(change_Q1_rel*100),"%")),
                 size=3) +
    scale_fill_distiller(palette="RdBu",
                         limits=c(-l,l),
                         name=NULL,
                         labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2021Q1 vs 2020Q1")

  ggsave(file.path(folder,"change_province_q1.png"), p1,
         width=width,
         height=height)


  p4 <- ggplot(m.change.province.sf) +
    geom_sf(aes(fill=change_Q4_rel)) +
    geom_sf_text(aes(label=paste0(round(change_Q4_rel*100),"%")),
                 size=3) +
    scale_fill_distiller(palette="RdBu",
                         limits=c(-l,l),
                         name=NULL,
                         labels=scales::percent) +
    rcrea::theme_crea() +
    labs(title="PM 2.5 concentration: 2020Q4 vs 2019Q4")

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

  l <- max(abs(m.change.city.sf$change_Q1_rel),abs(m.change.city.sf$change_Q4_rel))

  p1 <- ggplot(m.change.city.sf) +
    geom_sf(aes(color=change_Q1_rel)) +
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
    geom_sf(data=m.change.city.sf, aes(color=change_Q4_rel)) +
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

map.change_keyregion <- function(meas){

}

map.change_interpolated <- function(meas, res=0.1){

  m.city <- meas %>%
    filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2020Q1"),
           !sand_storm) %>%
    group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
    summarise_at(c("pm25"), mean, na.rm=T) %>%
    tidyr::spread("quarter","pm25")

  m.city$new1 <- m.city$`2021Q1`
  m.city$old1 <- m.city$`2020Q1`
  m.city$new4 <- m.city$`2020Q4`
  m.city$old4 <- m.city$`2019Q4`

  m.city.sf <- m.city %>%
    left_join(rcrea::cities(id=unique(m.city$location_id),
                            with_geometry=T) %>%
                dplyr::select(location_id=id, geometry)) %>%
    sf::st_as_sf()

  g <- data.gadm()
  r <- raster::raster(g, resolution=res)


  # library(gstat)
  # library(magrittr)
  # library(rasterVis)
  # library(sp)
  # r.idw.new1 <- gstat::idw(new1 ~ 1, as(m.city.sf %>% filter(!is.na(new1)), "Spatial"), newdata=as(r, "SpatialPixels")) %>% raster::raster() %>% raster::mask(g)

  r.interp.new1 <- utils.interp.ok(r, m.city.sf %>% rename(value=new1), mask=g)
  r.interp.old1 <- utils.interp.ok(r, m.city.sf %>% rename(value=old1), mask=g)
  r.interp.new4 <- utils.interp.ok(r, m.city.sf %>% rename(value=new4), mask=g)
  r.interp.old4 <- utils.interp.ok(r, m.city.sf %>% rename(value=old4), mask=g)

  r.change1 <- r.interp.new1/r.interp.old1 -1
  r.change4 <- r.interp.new4/r.interp.old4 -1

  d.plot <- list(r.change4, r.change1) %>%
    lapply(function(x) x %>% min(.6) %>% max(-.6) %>% multiply_by(100)) %>% raster::stack()

  (plt.contour <-  d.plot %>%
    rasterVis::levelplot(names.attr=c(
      "2020Q4 vs 2019Q4",
      "2021Q1 vs 2020Q1"),
                         main=NULL,
       xlab=NULL, ylab=NULL,
      scales=list(x=list(draw=F),
                  y=list(draw=FALSE)),
                         par.settings = rasterVis::BuRdTheme(
                           layout.widths = list(axis.key.padding = 0,
                                                ylab.right = 2)),
                         at=seq(-0.6*100, 0.6*100, length.out=100),
                         ylab.right = "Change in percentage"

    )  +
    latticeExtra::layer(sp::sp.lines(as(g,"Spatial"), col='gray')))

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
           caption=paste0("CREA based on ",source_legend, ", VIIRS and HYSPLIT.\nSize reflects the maximum fire intensity.\n",
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
