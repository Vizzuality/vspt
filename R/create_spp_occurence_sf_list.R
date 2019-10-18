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
           end_year) {
    # Check if path exists and create if needed
    dir.create(file.path(path), showWarnings = FALSE)

    # Get keys and nms
    keys <- lapply(spp_list, rgbif::name_backbone, rank='species')
    nms <- lapply(keys, function(x) x$canonicalName)

    # Create spp occurence GBIF query list
    q_list <-
      vspt::create_spp_query_list(
        spp_list,
        bb = bb,
        creds,
        start_year = start_year,
        end_year = end_year
      )

    # Create spp occurence GBIF data list, converted to sf::sf
    print("Downloading species data")
    download_list <- rgbif::occ_download_queue(.list = q_list)
    save(download_list, file=file.path(path, "download_list.rda"))
    print("Extracting species data")
    dat_list <- lapply(download_list, rgbif::occ_download_get, path = path, overwrite=TRUE)
    save(dat_list, file=file.path(path, "dat_list.rda"))
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
        print(sel)
        if(nrow(x)>0){return(paste(x[[sel]][1]))}else{return("NA")}
        }, sfc_list, keys)
      )
    # remove NAs
    sfc_list <- sfc_list[names(sfc_list) != "NA"]

    # Check number of occurences and filter out species with < 30 occurences
    no <- sapply(sfc_list, nrow)
    print("Number of occurences per specicies: ")
    print(no)
    sfc_list <- sfc_list[no > 30]
    print("Number of species with > 30 occurences: ", length(sfc_list))
    return(sfc_list)
  }
