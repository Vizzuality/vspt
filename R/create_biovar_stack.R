#' Create masked bioclimatic variable raster stack
#'
#' Select historic and future  bioclimatic variable rasters from directory,
#' create ensembles, create a stacks, optionally convert to principle components
#' and optionally mask by land. Note this function can take a while....
#'
#' @param region String representing the region directory
#' @param biovar_list List of string representing the biovar codes
#' @param path String representing the path to the bioclimatic directory
#'
#' @return
#' A named list with the region as first level containing lists of;
#' data file paths, processed historical and future biovariable rasters,
#' polygon of land, the biovariable and scenario lists used, and optionally
#' a pca model of the historical biovariables.
#' @export
#'
#' @examples
#' bvso <- create_biovar_stack(
#' region='biovar_Sweden',
#' biovar_list= c("biovar01","biovar02","biovar03","biovar04","biovar05","biovar06",
#'             "biovar07","biovar08","biovar09","biovar10","biovar11","biovar12",
#'                         "biovar13","biovar14","biovar15","biovar16"),
#'                         scenario_list=c("rcp45", "rcp85", "rcp26", "rcp60"),
#'                         path="inst/ext_data/bioclimatic",
#'                         mask_by_land = T,
#'                         pca_transform = T
#'                         )
#'                         names(bvso)
#'                         names(bvso[[region]])
create_biovar_stack <- function(
  region,
  biovar_list,
  scenario_list,
  time_interval_list,
  path,
  mask_by_land,
  pca_transform,
  use_future_pca
){

  # Create output file dir
  dir.create("bioclimatic_ensemble_rasters")

  # Set raster::raster options
  raster::rasterOptions(progress = 'text',
                        timer=TRUE,
                        tmpdir = './tmp_rasters',
                        #create=T,
                        chunksize=1e+09)

  # Start cluster
  raster::beginCluster()

  # Create the output biovar stacks object for a single region
  bvso <- list(
    list(
      df_file_paths = NA
      , historical = list(raw=NA, pc=NA, raw_masked=NA, pc_masked=NA)
      , future = list(raw=NA, pc=NA, raw_masked=NA, pc_masked=NA)
      , pca_model = NA
      , land_poly = NA
      , biovar_list = biovar_list
      , scenario_list = scenario_list
      , time_interval_list = time_interval_list
    )
  )
  #print(region)
  names(bvso) <- region

  # Parse the bioclimatic directory
  print(paste('Parsing bioclimatic directory:', path))
  df_bc <- vspt::parse_bioclimatic(path=path)
  df <- df_bc[[region]]
  bvso[[region]]$df_file_paths <- df

  # Historical
  dfh <- df[df$time_period == 'biovar_historical' & df$bio_variable %in% biovar_list,]

  # Future
  dff <- df[df$time_period == 'biovar_future' &
              df$bio_variable %in% biovar_list &
              df$scenario %in% scenario_list,]

  # Create raster stacks. For future create enemsbles and then stacks as a list
  process_future <- function(dff){
    fl <- split(dff[, "file_name"], dff[, c("time_interval", "scenario", "bio_variable")])
    # make ensemble mean per year, scenario, biovar
    s_list <- lapply(fl, function(x){
      # create stack
      out <- raster::stack(unlist(x), quick=T)
      raster::crs(out) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      # ensemble mean

      out <- raster::clusterR(out, raster::calc, args=list(fun=mean))
      return(out)
    })

    # make stacks per year per scenario
    sdf <- data.frame(matrix(unlist(strsplit(names(s_list), "[.]")), ncol = 3, byrow = T))
    sdf <- sdf[order(sdf$X1, sdf$X2, sdf$X3),]
    ss <- list()
    years <- unique(sdf[,1])
    scenes <- unique(sdf[,2])
    for(y in years){
      for(s in scenes){
        sl <- paste(y, s , biovar_list, sep=".")
        #print(sl)
        #print(na.exclude(match(names(s_list), sl, incomparables = F)))
        ssl <-  s_list[names(s_list) %in% sl]
        st <- raster::stack(ssl, quick=T)
        names(st) <- biovar_list
        raster::writeRaster(st, file.path("bioclimatic_ensemble_rasters", paste(y, s, "biovars.grd", sep="_")))
        ss[[y]][[s]] <- list(st)

        }
    }
    return(ss)
  }
  process_historical <- function(dfh){
    fl <- unlist(dfh$file_name)
    s <- raster::stack(fl)
    sub(".tif", ".grd", sub("01", "", basename(fl[1])))
    raster::crs(s) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    names(s) <- biovar_list
    raster::writeRaster(s, file.path("bioclimatic_ensemble_rasters",
                                     sub(".tif", ".grd", sub("01", "", basename(fl[1])))))
    return(s)
  }

  print('Creating historical biovar ensembles and raster stack(s)')
  bvso[[region]]$historical$raw <- process_historical(dfh)
  print('Creating future biovar raster stack(s)')
  bvso[[region]]$future$raw <- process_future(dff)

  # Optionally convert to principle components
  if(pca_transform){
    print('Converting all stacks to principle components')
    if(use_future_pca){pca_stack <- raster::stack(bvso[[region]]$future$raw$`2090`$rcp85)
    }else{pca_stack <- raster::stack(bvso[[region]]$historical$raw)}
    bvso[[region]]$pca_model <- prcomp(raster::values(pca_stack),
                                       scale=T,
                                       rank. = 3)
    bvso[[region]]$historical$pc <- raster::clusterR(bvso[[region]]$historical$raw,
                                             raster::predict,
                                             args = list(model=bvso[[region]]$pca_model,
                                                         index=1:ncol(bvso[[region]]$pca_model$x))
    )
    names(bvso[[region]]$historical$pc) <- colnames(bvso[[region]]$pca_model$x)
    bvso[[region]]$future$pc <- lapply(
      bvso[[region]]$future$raw,
      function(l){
        return(
          lapply(l,
                 function(x){
                   #print(class(x[[1]]))
                   names(x[[1]]) <- bvso[[region]]$biovar_list
                   out <- raster::clusterR(x[[1]],
                                    raster::predict,
                                    args = list(model=bvso[[region]]$pca_model,
                                                index=1:ncol(bvso[[region]]$pca_model$x))
                                    )
                   names(out) <- colnames(bvso[[region]]$pca_model$x)
                   return(out)
                   }
                 )
        )
      }
    )
  }

  # Add land polygon
  load(system.file("ext_data/boundary-vectors/land_mask.rda", package = "vspt"))
  bvso[[region]]$land_poly <- sf::st_crop(land_mask, sf::st_bbox(bvso[[region]]$historical$raw))

  if(mask_by_land){
    print('Masking all stacks by land')
    try(bvso[[region]]$historical$raw_masked <- raster::mask(bvso[[region]]$historical$raw, bvso[[region]]$land_poly))
    try(bvso[[region]]$historical$pc_masked <- raster::mask(bvso[[region]]$historical$pc, bvso[[region]]$land_poly))
    try(
      bvso[[region]]$future$raw_masked <- lapply(bvso[[region]]$future$raw, function(l){
        return(
          lapply(l, function(x){
            #print(class(x[[1]]))
            return(raster::mask(x[[1]], bvso[[region]]$land_poly))
            })
        )
        })
      )
    try(
      bvso[[region]]$future$pc_masked <- lapply(bvso[[region]]$future$pc, function(l){
        return(
          lapply(l, function(x){
            print(class(x))
            return(raster::mask(x, bvso[[region]]$land_poly))
          })
        )
      })
    )
    }
  # End cluster
  raster::endCluster()
  return(bvso)
}
