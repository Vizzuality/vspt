#' Plot historical, future and the difference in occurence probability
#'
#' @param spp_out A sf::sfc object with spp occurence probability
#' @param species Species to plot
#' @param stat The statistic to plot
#' @param year The future time-interval to plot
#' @param scenario The future scenario to plot
#' @param crs Choose a map projection
#' @param iso3 Country iso alpha 3 code
#'
#' @return
#' Nothing, side effect is a plot
#' @export
#'
#' @examples
# Plot species distributions
#require(vspt)
#require(ggplot2)
#load("arg_list.rda")
#load(arg_list$spp_zstat_grid_path)
#species_list <- arg_list$spp_list
#print(species_list)
#species <- species_list[2]
#plot_spp_scenario(spp_out, species)
plot_spp_scenario <- function(spp_out,
                              species,
                              stat = 'EMca',
                              year = 2090,
                              scenario = 'rcp85',
                              crs = "mercator",
                              iso3 = NULL) {
  pfp <- function(iso,
                  tp,
                  spp,
                  stat,
                  y = NULL,
                  s = NULL) {
    spp <- gsub(" ", ".", spp)
    if (tp == 'historical')
      out <- paste(iso, tp, spp, stat, sep = ".")
    if (tp == 'future')
      out <- paste(iso, tp, y, s, spp, stat, sep = ".")
    return(out)
  }

  h <- pfp(arg_list$region, 'historical', species, stat)
  f <- pfp(arg_list$region, 'future', species, stat, year, scenario)
  spp <- spp_out[, c(h, f, 'geometry')]
  names(spp) <- c('historical', 'future', 'geometry')
  spp$historical <- spp$historical / 1000
  spp$future <- spp$future / 1000
  spp$diff <- spp$future - spp$historical
  cs <-
    (colorRampPalette(c('#7044ff', '#F7F7F71A', '#7fbf7b'))(5))


  # Get iso3 GADM polygon
  poly <- sf::st_as_sf(raster::getData('GADM', country=iso3, level=1))
  overlay <- ggplot2::fortify(poly, region="NAME_1")
  g_overlay <- ggplot2::geom_sf(data=overlay,
                         fill = NA,
                         lwd = 0.08,
                         alpha=0.9, color="#222222")


  # Set some options
  ps <- 0.01
  lwd <- 0.02
  if (crs == "laea")
  {
    dcrs <-
      "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "
  }
  if (crs == "mercator") {
    dcrs <-
      "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  }
  ggplot2::theme_set(theme_bw())
  ggh <- ggplot2::ggplot(spp) +
    ggplot2::geom_sf(ggplot2::aes(fill = historical), lwd = lwd) +
    ggplot2::scale_fill_gradientn(colors = cs, name = "Probability") +
    ggplot2::coord_sf(crs = dcrs) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Historical distribution", subtitle = species) +
    ggplot2::theme(plot.margin = rep(unit(ps, "null"), 4),
                   panel.spacing = unit(ps, "null")) +
    g_overlay
  ggf <- ggplot2::ggplot(spp) +
    ggplot2::geom_sf(ggplot2::aes(fill = future), lwd = lwd) +
    ggplot2::scale_fill_gradientn(colors = cs, name = "Probability") +
    ggplot2::coord_sf(crs = dcrs) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Future distribution", subtitle = paste(species, year, scenario)) +
    ggplot2::theme(plot.margin = rep(unit(ps, "null"), 4),
                   panel.spacing = unit(ps, "null")) +
    g_overlay
  ggd <- ggplot2::ggplot(spp) +
    ggplot2::geom_sf(ggplot2::aes(fill = diff), lwd = lwd) +
    ggplot2::scale_fill_gradientn(colors = cs,
                                  name = "Difference",
                                  limits = c(-1, 1)) +
    ggplot2::coord_sf(crs = dcrs) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Difference", subtitle = 'Future - historical') +
    ggplot2::theme(plot.margin = rep(unit(ps, "null"), 4),
                   panel.spacing = unit(ps, "null")) +
    g_overlay

  gridExtra::grid.arrange(ggh, ggf, ggd, ncol = 2, nrow = 2)
}
