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
select_species_names <- function(path, iso3, field="canonicalName"){
  spp_df <- read.csv(path, stringsAsFactors = F)
  return(spp_df[spp_df$iso3 == iso3, field])
}
