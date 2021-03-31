utils.interp.ok <- function(r, v, mask=NULL, use_parallel=F){
  tryCatch({
    v.sp <- as(v[!is.na(v$value),], "Spatial")
    # crs(v.sp) <- crs(pop,T)
    var <-  variogram(value~1, v.sp, width=20, cutoff=500)
    m <- fit.variogram(var,
                       vgm(NA, model="Sph", range=300, cutoff=500)
    )
    # plot(var, m)
    print("Vectorize")
    r.sp <- raster::raster(r) %>% raster::rasterToPoints(spatial=T)
    print("Kriging")
    if(use_parallel){
      no_cores <- parallel::detectCores() - 2

      cl <- makeCluster(no_cores)
      parts <- split(x = 1:length(r.sp), f = 1:no_cores)
      clusterExport(cl = cl, varlist = c("v.sp", "r.sp", "m", "parts"), envir = environment())
      clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
      parallelX <- parLapply(cl = cl, X = 1:no_cores, fun = function(x) gstat::krige(change~1, v.sp, r.sp[parts[[x]],], model=m, nmax=30))
      stopCluster(cl)

      kriged.sp <- do.call("rbind", parallelX)
    }else{
      kriged.sp <- gstat::krige(value~1, v.sp, r.sp, model=m, maxdist=2000, nmax=30)
    }
    print("Done")
    print("Rasterize")
    r.kriged <- raster::rasterize(kriged.sp, raster(r), kriged.sp$var1.pred, fun=mean)
    if(!is.null(mask)){
      r.kriged <- r.kriged %>% raster::mask(mask)
    }

    raster::plot(r.kriged)
    r.kriged
  }, error=function(err){
    return(NA)
  })
}
