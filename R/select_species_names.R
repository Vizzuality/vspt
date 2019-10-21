#' Select species names by country iso3 code
#'
#' @param path Path to csv with columns `iso3` and the defined field
#' @param iso3 ISO alpha 3 code for country
#' @param field The name of the field in the csv with the name
#'
#' @return
#' Vector string of names
#' @export
#'
#' @examples
#' arg_list = yaml::read_yaml("arg_list.yaml")
#' path = do.call(file.path, as.list(arg_list$search_spp_list_path))
#' iso3 = 'TZA'
#' spp_list <- select_species_names(path, iso3)
select_species_names <- function(path, iso3, field="canonicalName"){
  spp_df <- read.csv(path, stringsAsFactors = F)
  return(na.exclude(spp_df[spp_df$iso3 == iso3 & spp_df$search == "TRUE", field]))
}
