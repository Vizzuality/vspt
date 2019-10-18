#' Create biomod2 data package list
#'
#' TODO: how to add evaluation data?
#'
#' @param spp_occurence_raster_list List of species occurence rasters
#' @param biovar_stack raster::stack of bioclimatic explanationary values
#' @param split_ratio Ratio which to split occurences data into training and testing
#'
#' @return
#' A list of biomod2 data packages
#' @export
#'
#' @examples
create_biomod_data_package_list <-
  function(spp_occurence_raster_list,
           biovar_stack,
           split_ratio = 0.7) {
    return(
      parallel::mclapply(spp_occurence_raster_list, function(x) {
        #x <- spp_occurence_raster_list[[1]]
        # spp name
        spp_name <- names(x)
        print(paste('creating data package for', spp_name))

        # convert spp raster to points
        species <- as.data.frame(raster::rasterToPoints(x))

        # select presences only
        presences <- species[which(species[, 3] == 1) ,]
        #absences <- species[which(species[,3]==0) , ]

        # split into training and testing data
        #set.seed(999)
        #sample  <- caTools::sample.split(species[,3], SplitRatio = split_ratio)
        #train <- species[which(sample == T),]
        # species presences vector (only 'ones')
        #presences <- train[which(train[,3]==1) , ]
        # test assume absences are true
        #test <- species[which(sample == F),]

        # training
        resp_var <- as.numeric(presences[, 3])
        resp_xy <- as.matrix(presences[, 1:2])

        # testing, add absences
        #l <- length(subset(presences[,3], sample == F))
        #sa  <- absences[sample.int(n=nrow(absences), size = l),]
        #eval_resp_var <- as.numeric(test[,3])
        #c(as.numeric(subset(presences[,3], sample == F)), as.numeric(sa[,3]))
        #eval_resp_xy <- as.matrix(test[,1:2])
        #rbind(as.matrix(subset(presences[,1:2], sample == F)),as.matrix(sa[,1:2]))

        # create biomod data object
        return(
          biomod2::BIOMOD_FormatingData(
            resp.var = resp_var,
            expl.var = raster::stack(biovar_stack),
            resp.xy = resp_xy,
            resp.name = spp_name,
            #eval.resp.var = eval_resp_var,
            #eval.expl.var = raster::stack(biovar_stack),
            #eval.resp.xy = eval_resp_xy,
            PA.nb.rep = 2,
            PA.nb.absences = 300,
            PA.strategy = "sre",
            PA.sre.quant = 0.25,
            na.rm = T
          )
        )

      }, mc.cores = parallel::detectCores() - 1)
    )
  }
