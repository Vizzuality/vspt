################################################################################
# PIPELINE ARGUMENTS
################################################################################

# Load packages
require(vspt)

# Set the args yaml file path!
arg_list = yaml::read_yaml(system.file("ext_data/arg_list.yaml", package = 'vspt'))

# Parse creds
arg_list$creds <- yaml::read_yaml(arg_list$creds_path)

# resume pipeline from specific step or calculate everything?
resume = TRUE

# recalculate a specific object
# 0 Do not start at a specific point
# (if resume is True the last object, otherwise the start)
# 1 PREPARE BIOCLIMATIC VARIABLES
# 2 GBIF SPECIES OCCURENCE TABLES
# 3 BIOMOD2 SPECIES OCCURENCE MODELLING
# 4 PROJECTIONS
# 5 PREPARE SPECIES ZONAL STATS
# 6 PREPARE BIOVAR ZONAL STATS
# 7 PREPARE SUMMARY STATS
recalculate = 0

# Set working directory for outputs
try(setwd(do.call(file.path, as.list(arg_list$out_path))))

################################################################################
# PREPARE BIOCLIMATIC VARIABLES
################################################################################
# Should contain bioclimatic directory with nested
# directories of bioclimatic rasters:
#  /bioclimatic
#     - /<ISO3>
#       - /historical
#         - <biovar[01-16]>
#       - /future
#         - /<time_periods>
#           - /<scenarios>
#             -<model_biovar[01-16]>
#

# Create biovar stack object (everything bioclimatic we need for the modelling)
print("Searching for biovar stack object")
st <- Sys.time()
if (resume & recalculate != 1) {
  try(mt <- file.mtime(arg_list$bvso_path))
  try(load(arg_list$bvso_path))
}
if (exists('bvso')) {
  print("Found existing")
}
if (!exists('bvso')) {
  print("Creating new biovar stack")
  bvso <- vspt::create_biovar_stack(
    region = arg_list$region,
    biovar_list = arg_list$biovar_list,
    scenario_list = arg_list$scenario_list,
    time_interval_list = arg_list$time_interval_list,
    path = file.path(arg_list$path),
    mask_by_land = arg_list$mask_by_land,
    pca_transform = arg_list$pca_transform,
    use_future_pca = arg_list$use_future_pca
  )
  save(bvso, file = arg_list$bvso_path)
}
stopifnot(exists('bvso'))
print(paste("Loaded biovar stack object version: ", mt))
# Get land mask
land_poly <- bvso[[arg_list$region]]$land_poly
# Select only the stacks we need
if (arg_list$mask_by_land) {
  bvso[[arg_list$region]]$historical$raw <- NULL
  bvso[[arg_list$region]]$historical$pc <- NULL
  bvso[[arg_list$region]]$future$raw <- NULL
  bvso[[arg_list$region]]$future$pc <- NULL
}
et <- Sys.time()
print(et - st)

################################################################################
# GBIF SPECIES OCCURENCE TABLES
################################################################################

# Get species occurence from gbif
# TODO Is there more info about each spp from GBIF that would be useful?
print("Searching for species occurence sfc list object")
st <- Sys.time()
if (resume & recalculate != 2) {
  try(mt <- file.mtime(arg_list$spp_occurence_sfc_list))
  try(load(arg_list$spp_occurence_sfc_list))
}
if (exists('spp_occurence_sfc_list')) {
  print("Found existing")
}
if (!exists('spp_occurence_sfc_list')) {
  print("Creating species occurence sfc list")
  spp_occurence_sfc_list <- vspt::create_spp_occurence_sf_list(
    spp_list = arg_list$search_spp_list,
    bb = sf::st_bbox(land_poly),
    creds =  arg_list$creds,
    path =  "./species-occurence-tables",
    start_year =  arg_list$start_year,
    end_year = arg_list$end_year
  )
  # Save the spp_occurence_sfc_list
  save(spp_occurence_sfc_list, file = "spp_occurence_sfc_list.rda")
  print(paste(
    "Number of species with occurence data = ",
    length(spp_occurence_sfc_list)
  ))
  et <- Sys.time()
  print(et - st)
}
if (exists('spp_occurence_sfc_list')) {
  # Update the spp_list with this output, as order changes!!
  arg_list$spp_list <- names(spp_occurence_sfc_list)
  # Save the arg_list
  # remove creds
  arg_list$creds <- NULL
  save(arg_list, file = "arg_list.rda")
  # Export as spp occurence sfc CSV
  print("Writing species occurence sfc list to CSV")
  sf::st_write(
    obj = do.call(rbind, spp_occurence_sfc_list),
    dsn = arg_list$spp_output_path,
    layer_options = c(
      "GEOMETRY=AS_WKT",
      "GEOMETRY_NAME=geometry",
      "SEPARATOR=COMMA",
      "OGR_WKT_PRECISION=7"
    ),
    delete_dsn = TRUE
  )
}
stopifnot(exists('spp_occurence_sfc_list'))
print(paste("Loaded species occurence sfc list object version: ", mt))
et <- Sys.time()
print(et - st)

################################################################################
# BIOMOD2 SPECIES OCCURENCE MODELLING
################################################################################

# Convert spp occurence to presence-absence rasters
print("Searching for spp occurence presence rasters")
st <- Sys.time()
if (resume & recalculate != 3) {
  try(mt <- file.mtime(arg_list$spp_occurence_raster_list))
  try(load(arg_list$spp_occurence_raster_list))
}
if (exists('spp_occurence_raster_list')) {
  print("Found existing")
  rm(spp_occurence_sfc_list)
}
if (!exists('spp_occurence_raster_list')) {
  spp_occurence_raster_list <-
    vspt::create_spp_occurence_raster_list(
      spp_occurence_sfc_list =  spp_occurence_sfc_list,
      r = bvso[[arg_list$region]]$historical$pc_masked,
      land_mask = land_poly
    )
  save(spp_occurence_raster_list,  file = "spp_occurence_raster_list.rda")
  remove(spp_occurence_sfc_list)
}
stopifnot(exists('spp_occurence_raster_list'))
print(paste("Loaded species occurence raster list object version: ", mt))
et <- Sys.time()
print(et - st)

# Create biomod2 data packages
print("Searching for biomod2 data package rasters")
st <- Sys.time()
if (resume & recalculate != 3) {
  try(mt <- file.mtime(arg_list$biomod_data_package_list))
  try(load(arg_list$biomod_data_package_list))
}
if (exists('biomod_data_package_list')) {
  print("Found existing")
  rm(spp_occurence_raster_list)
}
if (!exists('biomod_data_package_list')) {
  biomod_data_package_list <-
    vspt::create_biomod_data_package_list(spp_occurence_raster_list,
                                          bvso[[arg_list$region]]$historical$pc_masked)
  save(biomod_data_package_list,  file = "biomod_data_package_list.rda")
  rm(spp_occurence_raster_list)
}
stopifnot(exists('biomod_data_package_list'))
print(paste("Loaded biomod data packager list object version: ", mt))
et <- Sys.time()
print(et - st)

# Set modelling options
biomod_model_options <-
  biomod2::BIOMOD_ModelingOptions() #saving object with default options
save(biomod_model_options,  file = "biomod_model_options.rda")

# Calibrate the models
print("Searching for biomod2 model list")
st <- Sys.time()
if (resume & recalculate != 3) {
  try(mt <- file.mtime(arg_list$biomod_model_list))
  try(load(arg_list$biomod_model_list))
}
if (exists('biomod_model_list')) {
  print("Found existing")
  rm(biomod_data_package_list, biomod_model_options)
}
if (!exists('biomod_model_list')) {
  biomod_model_list <-
    vspt::create_biomod_model_list(biomod_data_package_list = biomod_data_package_list,
                                   biomod_model_options =  biomod_model_options)
  save(biomod_model_list,  file = "biomod_model_list.rda")
  rm(biomod_data_package_list, biomod_model_options)
}
stopifnot(exists('biomod_model_list'))
print(paste("Loaded biomod model list object version: ", mt))
et <- Sys.time()
print(et - st)

# Create ensemble of the calibrated models
print("Searching for ensemble biomod2 model list")
st <- Sys.time()
if (resume & recalculate != 3) {
  try(mt <- file.mtime(arg_list$biomod_ensemble_model_list))
  try(load(arg_list$biomod_ensemble_model_list))
}
if (exists('biomod_ensemble_model_list')) {
  print("Found existing")
  rm(biomod_model_list)
}
if (!exists('biomod_ensemble_model_list')) {
  biomod_ensemble_model_list <-
    vspt::create_biomod_ensemble_model_list(biomod_model_list)
  save(biomod_ensemble_model_list,  file = "biomod_ensemble_model_list.rda")
  rm(biomod_model_list)
}
stopifnot(exists('biomod_ensemble_model_list'))
print(paste("Loaded biomod ensemble model list object version: ", mt))
et <- Sys.time()
print(et - st)

################################################################################
# PROJECTIONS
################################################################################

# Project the models to historical and future biovariables
print("Searching for biomod2 model projection list")
st <- Sys.time()
if (resume & recalculate != 4) {
  try(mt <- file.mtime(arg_list$biomod_model_projection_list))
  try(load(arg_list$biomod_model_projection_list))

}
if (exists('biomod_model_projection_list')) {
  print("Found existing")
  rm(biomod_model_list)
}
if (!exists('biomod_model_projection_list')) {
  try(load('arg_list.rda'))
  try(load(arg_list$bvso_path))
  try(load(arg_list$biomod_model_list_path))

  # create list structure for storing biomod2::BIOMOD.projection.out
  # objects (with links to data files stored on disk)
  biomod_model_projection_list <- list()
  biomod_model_projection_list$historical <-
    vspt::create_biomod_model_projection_list(
      biomod_model_list =  biomod_model_list,
      biovar_stack = bvso[[arg_list$region]]$historical$pc_masked,
      proj_name = 'historical'
    )
  # pre-create list structure
  biomod_model_projection_list$future <-
    setNames(lapply(arg_list$time_interval_list, function(y) {
      setNames(lapply(arg_list$scenario_list, function(s) {
        return(NA)
      }),
      arg_list$scenario_list)
    }),
    arg_list$time_interval_list)
  # fill list with objects
  for (y in arg_list$time_interval_list) {
    for (s in arg_list$scenario_list) {
      proj_name <- paste("future", y, s, sep = "_")
      print(proj_name)
      if (any(is.na(biomod_model_projection_list$future[[y]][[s]]))) {
        if (!is.null(bvso[[arg_list$region]]$future$pc_masked[[y]][[s]])) {
          biomod_model_projection_list$future[[y]][[s]] <-
            vspt::create_biomod_model_projection_list(biomod_model_list,
                                                      bvso[[arg_list$region]]$future$pc_masked[[y]][[s]],
                                                      proj_name = proj_name)
        } else{
          biomod_model_projection_list$future[[y]][[s]] <- NA
        }
      }
    }
  }

  save(biomod_model_projection_list,  file = "biomod_model_projection_list.rda")
  rm(biomod_model_list)
  gc(reset = T)
}
stopifnot(exists('biomod_model_projection_list'))
print(paste("Loaded biomod model projection list object version: ", mt))
et <- Sys.time()
print(et - st)

# Project the ensemble model to historical and biovariables
print("Searching for biomod2 ensemble model projection list")
st <- Sys.time()
if (resume & recalculate != 4) {
  try(mt <-
        file.mtime(arg_list$biomod_ensemble_model_projection_list))
  try(load(arg_list$biomod_ensemble_model_projection_list))
}
if (exists('biomod_ensemble_model_projection_list')) {
  print("Found existing")
  rm(biomod_ensemble_model_list, biomod_model_projection_list)
}
if (!exists('biomod_ensemble_model_projection_list')) {
  try(load('arg_list.rda'))
  try(load(arg_list$biomod_ensemble_model_projection_list))
  try(load(arg_list$biomod_ensemble_model_list))

  biomod_ensemble_model_projection_list <- list()
  biomod_ensemble_model_projection_list$historical <-
    vspt::create_biomod_ensemble_model_projection_list(
      biomod_ensemble_model_list =  biomod_ensemble_model_list,
      biomod_model_projection_list = biomod_model_projection_list$historical
    )
  # create list structure
  biomod_ensemble_model_projection_list$future <-
    setNames(lapply(arg_list$time_interval_list, function(y) {
      setNames(lapply(arg_list$scenario_list, function(s) {
        return(NA)
      }),
      arg_list$scenario_list)
    }),
    arg_list$time_interval_list)
  for (y in arg_list$time_interval_list) {
    for (s in arg_list$scenario_list) {
      proj_name <- paste("future", y, s, sep = "_")
      print(proj_name)
      biomod_ensemble_model_projection_list$future[[y]][[s]] <-
        vspt::create_biomod_ensemble_model_projection_list(biomod_ensemble_model_list,
                                                           biomod_model_projection_list$future[[y]][[s]])
    }
  }
  save(biomod_ensemble_model_projection_list,  file = "biomod_ensemble_model_projection_list.rda")
  rm(biomod_ensemble_model_list, biomod_model_projection_list)
  gc(reset = T)
}
stopifnot(exists('biomod_ensemble_model_projection_list'))
print(paste(
  "Loaded biomod ensemble_model projection list object version: ",
  mt
))
et <- Sys.time()
print(et - st)

# Stack ensemble model projection rasters
# this relies structure of model outputs in arg_list$out_path
print("Searching for ensemble model projection raster list")
st <- Sys.time()
if (resume & recalculate != 4) {
  try(mt <-
        file.mtime(arg_list$biomod_ensemble_raster_list))
  try(load(arg_list$biomod_ensemble_raster_list))
}
if (exists('biomod_ensemble_raster_list')) {
  print("Found existing")
  rm(biomod_ensemble_model_projection_list)
}
if (!exists('biomod_ensemble_raster_list')) {
  try(load('arg_list.rda'))

  biomod_ensemble_raster_list <- list()
  biomod_ensemble_raster_list$historical <-
    vspt::create_ensemble_model_raster_stack(arg_list$spp_list, proj_nm = 'historical')
  if (is.null(biomod_ensemble_raster_list$future)) {
    biomod_ensemble_raster_list$future <- create_future_list(arg_list)
  }
  # fill list with objects
  for (y in arg_list$time_interval_list) {
    for (s in arg_list$scenario_list) {
      proj_name <- paste("future", y, s, sep = "_")
      print(proj_name)
      biomod_ensemble_raster_list$future[[y]][[s]] <-
        vspt::create_ensemble_model_raster_stack(arg_list$spp_list, proj_name)
    }
  }
  save(biomod_ensemble_raster_list,  file = "biomod_ensemble_raster_list.rda")
  rm(biomod_ensemble_model_projection_list)
  gc(reset = T)
}
stopifnot(exists('biomod_ensemble_raster_list'))
print(paste("Loaded biomod ensemble raster list object version: ", mt))
et <- Sys.time()
print(et - st)

################################################################################
# PREPARE SPP ZONAL STATS
################################################################################

# Create vector polygon grid object (to vectorise the output data)
print("Searching for vector polygon grid object")
st <- Sys.time()
if (resume & recalculate != 5) {
  try(mt <-
        file.mtime(arg_list$vpgo_path))
  try(load(arg_list$vpgo_path))
}
if (exists('vpgo')) {
  print("Found existing")
  rm(land_poly)
}
if (!exists('vpgo')) {

  # create grid
  vpgo <-
    vspt::create_grid_from_poly(land_poly,
                                grid_res = arg_list$grid_res,
                                set_seed = arg_list$region)
  # crop to raster bbox
  vpgo <-
    sf::st_crop(vpgo, sf::st_bbox(bvso[[arg_list$region]]$historical[[1]]))
  save(vpgo, file = arg_list$vpgo_path)
}
stopifnot(exists('vpgo'))
print(paste("Loaded vector polygon grid object version: ", mt))
et <- Sys.time()
print(et - st)

# Do zonal statistics for species distribution rasters
print("Searching for species zonal stats object")
st <- Sys.time()
if (resume & recalculate != 5) {
  try(mt <- file.mtime(arg_list$spp_zstat_path))
  try(load(arg_list$spp_zstat_path))
}
isch <- F
iscf <- F
if (exists('spp_zstat')) {
  print("Found existing")
  # Check if complete
  isch <- vspt::chk_output(arg_list,
                           spp_zstat[[arg_list$region]]$historical,
                           type = 'data.frame')
  iscf <- all(vspt::chk_output_future(arg_list,
                                      spp_zstat[[arg_list$region]]$future,
                                      type = 'data.frame'))
  rm(land_poly)
}
# define zonal statistic function (suitable for lapply on a raster::stack)
if (!exists('spp_zstat') | !isch | !iscf) {
  do_zs <- function(so)
  {
    soa <- raster::stack(so, raster::area(so))
    names(soa) <- c(names(so), 'area')
    # Define function to apply
    f1 <- function(values)
    {
      nms <- names(values)
      nms <- names(values)[1:(length(nms) - 2)]
      return(sapply(nms, function(nm)
      {
        if (!all(is.na(values[, nm])))
        {
          out <-
            weighted.mean(values[, nm], values$area * values$coverage_frac, na.rm =
                            T)
        }
        else{
          out <- NA
        }
      }))
    }
    return(
      vspt::zonal_stats_poly(
        fun = f1,
        vpgo = vpgo,
        ro = soa,
        remove_na = F,
        return_df = T,
        out_path = F
      )
    )

  }

  # do zonal stats for historical spp distribution
  if (!exists('spp_zstat')) {
    spp_zstat <- list()
  }
  if (!isch) {
    print("Calculating historical zstats")
    spp_zstat[[arg_list$region]]$historical <-
      parallel::mclapply(biomod_ensemble_raster_list$historical, do_zs, mc.cores = 6)
    save(spp_zstat, file = "spp_zstat.rda")
  }
  # do zonal stats for future spp distribution
  if (!iscf) {
    print("Calculating future zstats")
    spp_zstat[[arg_list$region]]$future <-
      vspt::create_future_list(arg_list)
    # fill list with objects
    for (y in arg_list$time_interval_list) {
      for (s in arg_list$scenario_list) {
        print(paste("future", y, s, sep = "_"))
        spp_zstat[[arg_list$region]]$future[[y]][[s]] <-
          parallel::mclapply(biomod_ensemble_raster_list$future[[y]][[s]],
                             function(x) {
                               return(do_zs(x))
                             }, mc.cores = 6, mc.silent = F)
      }
    }
    save(spp_zstat, file = "spp_zstat.rda")
  }
}
stopifnot(exists('spp_zstat'))
print(paste("Loaded species zonal stats object version: ", mt))
et <- Sys.time()
print(et - st)

# Add to grid
print("Searching for species zonal stats object")
st <- Sys.time()
if (resume & recalculate != 5) {
  try(mt <- file.mtime(arg_list$spp_zstat_grid_path))
  try(load(arg_list$spp_zstat_grid_path))
}
if (exists('spp_out')) {
  print("Found existing")
  rm(spp_zsat)
}
if (!exists('spp_out')) {
  print("Adding zstats to grid")
  spp_out <- sf::st_sf(data.frame(vpgo, spp_zstat))
  # remove geometries with NA values
  spp_out <- na.exclude(spp_out)
  attr(spp_out, "spp_list") <-
    arg_list$spp_list
  # Write to file
  save(spp_out, file = "spp_zstat_grid.rda")
  print("Writing to file")
  lapply(arg_list$zstats_output_formats, function(x) {
    fn <- paste0(arg_list$zstats_output_basename, x)
    sf::st_write(
      spp_out,
      fn,
      delete_dsn = TRUE,
      layer_options = c(
        "RFC7946=YES",
        "ID_FIELD=uuid",
        "WRITE_BBOX=YES",
        "COORDINATE_PRECISION=7",
        "GEOMETRY=AS_WKT",
        "GEOMETRY_NAME=geometry",
        "SEPARATOR=COMMA",
        "OGR_WKT_PRECISION=7"
      )
    )
  })
}
stopifnot(exists('spp_out'))
print(paste("Loaded species zonal stats grid object version: ", mt))
et <- Sys.time()
print(et - st)

################################################################################
# PREPARE BIOVAR ZONAL STATS
################################################################################
rm(spp_out, spp_zstat, iscf, isch)
gc()

# Do zonal statistics for bioclimatic indicator rasters
print("Searching for biovar zonal stats object")
st <- Sys.time()
if (resume & recalculate != 6) {
  try(mt <- file.mtime(arg_list$bv_zstat_path))
  try(load(arg_list$bv_zstat_path))
}
isch<- F
iscf <- F
if (exists('bv_zstat')) {
  print("Found existing")
  # Check if complete
  isch <- is(bv_zstat$historical, class2 = 'data.frame')
  iscf <- all( !is.null(bv_zstat$future) ,
               sapply(bv_zstat$future, function(y){
                 sapply(y, is, class2 = 'data.frame')}))
}
# define zonal statistic function (suitable for lapply on a raster::stack)
if (!exists('bv_zstat') | !isch | !iscf) {
  try(load('arg_list.rda'))
  if(!exists('vpgo')){try(load(arg_list$vpgo_path))}else{stop(!exists('vpgo'))}
  if(!exists('bvso')){try(load(arg_list$bvso_path))}else{stop(!exists('bvso'))}

  do_zs <- function(so)
  {
    soa <- raster::stack(so, raster::area(so))
    names(soa) <- c(names(so), 'area')
    # Define function to apply
    f1 <- function(values)
    {
      nms <- names(values)
      nms <- names(values)[1:(length(nms) - 2)]
      return(sapply(nms, function(nm)
      {
        if (!all(is.na(values[, nm])))
        {
          out <-
            weighted.mean(values[, nm], values$area * values$coverage_frac, na.rm =
                            T)
        }
        else{
          out <- NA
        }
      }))
    }
    return(
      vspt::zonal_stats_poly(
        fun = f1,
        vpgo = vpgo,
        ro = soa,
        remove_na = F,
        return_df = T,
        out_path = F
      )
    )

  }

  # do zonal stats for historical
  if (!exists('bv_zstat')) {
    bv_zstat <- list()
  }
  if (!isch) {
    print("Calculating historical zstats")
    bv_zstat$historical <-
      do_zs(bvso[[arg_list$region]]$historical$raw_masked)
    save(bv_zstat, file = arg_list$bv_zstat_path)
  }
  # do zonal stats for future spp distribution
  if (!iscf) {
    print("Calculating future zstats")
    bv_zstat$future <-
      parallel::mclapply(bvso[[arg_list$region]]$future$raw_masked, function(l) {
        return(lapply(l, function(x) {
          return(do_zs(x))
        }))
      }, mc.cores = 4)
    save(bv_zstat, file = arg_list$bv_zstat_path)
  }
}
stopifnot(exists('bv_zstat'))
print(paste("Loaded biovar zonal stats object version: ", mt))
et <- Sys.time()
print(et - st)

# Add to grid
print("Searching for biovar zonal stats grid object")
st <- Sys.time()
if (resume & recalculate != 5) {
  try(mt <- file.mtime(arg_list$bv_zstat_grid_path))
  try(load(arg_list$bv_zstat_grid_path))
}
if (exists('bv_out')) {
  print("Found existing")
  rm(bv_zsat)
}
if (!exists('bv_out')) {
  print("Adding zstats to grid")
  bv_out <- sf::st_sf(data.frame(vpgo, bv_zstat))
  # remove geometries with NA values
  bv_out <- na.exclude(bv_out)
  attr(bv_out, "biovar_list") <-
    arg_list$biovar_list
  # Write to file
  save(bv_out, file = "bv_zstat_grid.rda")
  print("Writing to file")
  tmp <- lapply(arg_list$zstats_output_formats, function(x) {
    fn <- paste0(arg_list$bv_zstats_output_basename, x)
    sf::st_write(
      bv_out,
      fn,
      delete_dsn = TRUE,
      layer_options = c(
        "RFC7946=YES",
        "ID_FIELD=uuid",
        "WRITE_BBOX=YES",
        "COORDINATE_PRECISION=7",
        "GEOMETRY=AS_WKT",
        "GEOMETRY_NAME=geometry",
        "SEPARATOR=COMMA",
        "OGR_WKT_PRECISION=7"
      )
    )
  })
}
stopifnot(exists('bv_out'))
print(paste("Loaded biovar zonal stats grid object version: ", mt))
et <- Sys.time()
print(et - st)

################################################################################
# PREPARE SUMMARY STATS
################################################################################
# clean up
rm(bv_out, bv_zstat, vpgo)
gc()

print("Searching for species summary stats object")
st <- Sys.time()
if (resume & recalculate != 7) {
  try(mt <- file.mtime(arg_list$spp_sstat_path))
  try(load(arg_list$spp_sstat_path))
}
if (exists('spp_sstat')) {
  print("Found existing")
}
# define zonal statistic function (suitable for lapply on a raster::stack)
if (!exists('spp_sstat')) {
  # FIXME why can i not access units::units?
  require(units)
  # Get GADM v 3.6 adm0 polygon
  g <-
    raster::getData(
      country = arg_list$iso3,
      level = 0,
      path = ".",
      download = T
    )
  area_g <- raster::area(g)
  units(area_g) <- with(ud_units, m ^ 2)
  units(area_g) <- with(ud_units, km ^ 2)
  summary_stats <- function(r, arg_list, g) {
    #r <- biomod_ensemble_raster_list$historical$`Picea glauca`$EMmean
    ra <- r$EMmean * raster::area(r)
    names(ra) <- "area"
    out <- exactextractr::exact_extract(ra, sf::st_as_sf(g), 'sum')
    units(out) <- with(ud_units, km ^ 2)
    return(out)
  }

  print("Calculating historical summary stats")
  spp_sstat <- list()
  spp_sstat$historical <-
    parallel::mclapply(
      biomod_ensemble_raster_list$historical,
      summary_stats,
      arg_list = arg_list,
      g = g,
      mc.cores = 4
    )
  spp_sstat$historical <- (unlist(spp_sstat$historical) / area_g)

  print("Calculating future summary stats")
  spp_sstat$future <- vspt::create_future_list(arg_list)
  for (y in arg_list$time_interval_list) {
    for (s in arg_list$scenario_list) {
      print(paste("future", y, s, sep = "_"))
      spp_sstat$future[[y]][[s]] <-
        parallel::mclapply(
          biomod_ensemble_raster_list$future[[y]][[s]],
          summary_stats,
          arg_list = arg_list,
          g = g,
          mc.cores = 4
        )
      spp_sstat$future[[y]][[s]] <-
        (unlist(spp_sstat$future[[y]][[s]]) / area_g)
    }
  }
  save(spp_sstat, file = arg_list$spp_sstat_path)

  # Write
  load('arg_list.rda')
  tmp <- round(unlist(c(
    list('1995' = list('current'= spp_sstat$historical)),
    spp_sstat$future)),1)
  nms <- matrix(unlist(strsplit(names(tmp), "[.]")), ncol=3, byrow = T)
  gbif_id <- unlist(sapply(arg_list$spp_list, rgbif::name_backbone, rank='species')['usageKey',])
  gbif_id_df <- data.frame(species=names(gbif_id), gbifId=gbif_id)
  df <- data.frame(
    iso=rep(arg_list$iso3, length(nms)),
    timeInterval=nms[,1],
    scenario=nms[,2],
    species=nms[,3],
    gbifId = gbif_id_df$gbifId[match(nms[,3], gbif_id_df$species)],
    propTotalArea=tmp
  )
  write.csv(df, arg_list$spp_sstat_out_path, na = 'NA', row.names = F)
}
stopifnot(exists('spp_sstat'))
print(paste("Loaded species summary stats object version: ", mt))
et <- Sys.time()
print(et - st)

# clean up
rm(biomod_ensemble_raster_list)
gc()

# Summary stats biovars
print("Searching for biovar summary stats object")
st <- Sys.time()
if (resume & recalculate != 7) {
  try(mt <- file.mtime(arg_list$bv_sstat_path))
  try(load(arg_list$bv_sstat_path))
}
if (exists('bv_sstat')) {
  print("Found existing")
}
# define zonal statistic function (suitable for lapply on a raster::stack)
if (!exists('bv_sstat')) {

  # Get GADM v 3.6 adm0 polygon
  g <- sf::st_as_sf(raster::getData(
      country = arg_list$iso3,
      level = 0,
      path = ".",
      download = T
    ))

  do_zs <- function(so, g)
  {
    soa <- raster::stack(so, raster::area(so))
    names(soa) <- c(names(so), 'area')
    # Define function to apply
    f1 <- function(values)
    {
      nms <- names(values)
      nms <- names(values)[1:(length(nms) - 2)]
      return(sapply(nms, function(nm)
      {
        if (!all(is.na(values[, nm])))
        {
          out <-
            weighted.mean(values[, nm], values$area * values$coverage_frac, na.rm =
                            T)
        }
        else{
          out <- NA
        }
      }))
    }
    return(
      vspt::zonal_stats_poly(
        fun = f1,
        vpgo = g,
        ro = soa,
        remove_na = F,
        return_df = T,
        out_path = F
      )
    )

  }

  print("Calculating historical summary stats")
  bv_sstat <- list()
  bv_sstat$historical <-
   do_zs(bvso[[arg_list$region]]$historical$raw_masked, g = g)

  print("Calculating future summary stats")
  bv_sstat$future <-
    parallel::mclapply(bvso[[arg_list$region]]$future$raw_masked, function(l) {
      return(lapply(l, function(x) {
        return(do_zs(x, g=g))
      }))
    }, mc.cores = 4)
  save(bv_sstat, file = arg_list$bv_sstat_path)

  # Write
  tmp <- round(unlist(c(
    list('1995' = list('current'= bv_sstat$historical)),
    bv_sstat$future)),1)
  nms <- matrix(unlist(strsplit(names(tmp), "[.]")), ncol=3, byrow = T)
  df <- data.frame(
    iso=rep(arg_list$iso3, length(nms)),
    timeInterval=nms[,1],
    scenario=nms[,2],
    biovar=nms[,3],
    propTotalArea=tmp
  )
  write.csv(df, arg_list$bv_sstat_out_path, na = 'NA', row.names = F)
}
stopifnot(exists('bv_sstat'))
print(paste("Loaded biovar summary stats object version: ", mt))
et <- Sys.time()
print(et - st)

print("Job completed!!")
