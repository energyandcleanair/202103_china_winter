
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


  library(gstat)
  library(magrittr)
  library(rasterVis)
  library(sp)
  # r.idw.new1 <- gstat::idw(new1 ~ 1, as(m.city.sf %>% filter(!is.na(new1)), "Spatial"), newdata=as(r, "SpatialPixels")) %>% raster::raster() %>% raster::mask(g)

  r.interp.new1 <- utils.interp.ok(r, m.city.sf %>% rename(value=new1), mask=g)
  r.interp.old1 <- utils.interp.ok(r, m.city.sf %>% rename(value=old1), mask=g)
  r.interp.new4 <- utils.interp.ok(r, m.city.sf %>% rename(value=new4), mask=g)
  r.interp.old4 <- utils.interp.ok(r, m.city.sf %>% rename(value=old4), mask=g)

  r.change1 <- r.interp.new1/r.interp.old1 -1
  r.change4 <- r.interp.new4/r.interp.old4 -1

  d.plot <- list(r.change4, r.change1) %>%
    lapply(function(x) x %>% min(.25) %>% max(-.25) %>% multiply_by(100)) %>% raster::stack()

  (plt.contour <-  d.plot %>%
    rasterVis::levelplot(names.attr=c("2020Q4 vs 2019Q4", "2021Q1 vs 2020Q1"),
                         main=NULL, xlab=NULL, ylab=NULL,
                         par.settings = rasterVis::BuRdTheme(
                           layout.widths = list(axis.key.padding = 0,
                                                ylab.right = 2)),
                         at=seq(-0.25*100, 0.25*100, length.out=100),
                         ylab.right = "Change in percentage"

    )  +
    latticeExtra::layer(sp::sp.lines(as(g,"Spatial"), col='gray')))

  return(plt.contour)
}
