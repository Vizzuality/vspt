#' Creat species occurence sf::sfc list
#'
#' @param spp_list List of scientific species or genus names
#' @param bb sf::bbox object like, used to crop extent of occurences
#' @param creds GBIF user credentials
#' @param start_year Start year for occurence query
#' @param end_year End year for occurence query
#'
#' @return
#' A named list of sf::sf data.frame(s) per species name.
#' @export
#'
#' @examples
create_spp_occurence_sf_list <-
  function(spp_list,
           bb,
           creds,
           path = "species-occurence-tables",
           start_year,
           end_year,
           min_ocurences = 50,
           keep_top_n = F,
           n_to_keep = 5) {

    # Create spp occurence GBIF data list, converted to sf::sf
    try(load(file.path(path, "download_list.rda")), silent=T)
    try(if(exists("download_list")){print("Found download list")})
    try(if(!exists("download_list")){
      print("Downloading species data")
      # Check if path exists and create if needed
      dir.create(file.path(path), showWarnings = FALSE)

      # Get keys and nms
      keys <- lapply(spp_list, rgbif::name_backbone, rank='species')
      match_type <- sapply(keys, function(x) x$matchType)
      keys <- keys[match_type != 'NONE']
      nms <- sapply(keys, function(x) x$canonicalName)

      # Create spp occurence GBIF query list
      q_list <-
        vspt::create_spp_query_list(
          spp_list,
          bb = bb,
          creds,
          start_year = start_year,
          end_year = end_year
        )
      names(q_list) <- nms
      download_list <- rgbif::occ_download_queue(.list = q_list)
      save(download_list, file=file.path(path, "download_list.rda"))
    })

    try(load(file.path(path, "dat_list.rda")), silent=T)
    try(if(exists("dat_list")){print("Found download data list")})
    try(if(!exists("dat_list")){
      print("Extracting species data")
      dat_list <- lapply(download_list, rgbif::occ_download_get, path = path, overwrite=TRUE)
      save(dat_list, file=file.path(path, "dat_list.rda"))
    })

    print("Creating species sfc list")
    sfc_list <- lapply(dat_list, function(x) {
      #x <- dat_list[[1]]
      out <- NA
      try({
        df <- rgbif::occ_download_import(x)[, c(
          "speciesKey",
          "genus",
          "species",
          "decimalLongitude",
          "decimalLatitude"
        )]
        out <- sf::st_crop(
          sf::st_as_sf(
            df,
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = 4326,
            agr = "constant"
          ),
          bb
        )
      })
    })
    try(
      names(sfc_list) <-
      mapply(function(x, y){
        sel <- tolower(y$rank)
        #print(sel)
        if(nrow(x)>0){return(paste(x[[sel]][1]))}else{return("NA")}
        }, sfc_list, keys)
      )
    # remove NAs
    sfc_list <- sfc_list[names(sfc_list) != "NA"]

    # Check number of occurences and filter out species with < 30 occurences
    n_occurences <- sapply(sfc_list, nrow)
    save(n_occurences, file="n_occurences.rda")
    print("Number of occurences per specicies: ")
    print(n_occurences)
    sfc_list <- sfc_list[n_occurences > min_ocurences]
    print(paste("Number of species with > 30 occurences: ", length(sfc_list)))
    if(keep_top_n){
      print(paste("Selecting top", n_to_keep, "species"))
      order(no)[1:n_to_keep]
      sfc_list <- sfc_list[order(no)[1:n_to_keep]]
    }
    return(sfc_list)
  }
