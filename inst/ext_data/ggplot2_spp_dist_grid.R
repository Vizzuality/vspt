#raster::plot(biomod_ensemble_raster_list$historical$`Picea glauca`)
#raster::plot(biomod_ensemble_raster_list$future$`2090`$rcp85$`Picea glauca`)
#raster::plot(
#  biomod_ensemble_raster_list$future$`2090`$rcp85$`Picea glauca` -
#     biomod_ensemble_raster_list$historical$`Picea glauca`)

# Plot species distributions
load(arg_list$spp_zstat_grid_path)
species <- attr(spp_out, "spp_list")[4]
stat <- 'EMca'
year <-2090
scenario <-'rcp85'

pfp <- function(iso, tp, spp, stat, y=NULL, s=NULL){
  spp <- gsub(" ",".", spp)
  if(tp=='historical') out <- paste(iso, tp, spp, stat, sep = ".")
  if(tp=='future') out <- paste(iso, tp, y, s, spp, stat, sep = ".")
  return(out)
}

h <- pfp(arg_list$region, 'historical', species, stat)
f <- pfp(arg_list$region, 'future', species, stat, year, scenario)
spp <- spp_out[,c(h, f)]
names(spp) <- c('historical', 'future', 'geometry')
spp$historical <- spp$historical/1000
spp$future <- spp$future/1000
spp$diff <- spp$future - spp$historical
names(spp)
cs <-
  (colorRampPalette(c('#7044ff', '#F7F7F71A', '#7fbf7b'))(5))
print(species)

require(ggplot2)
ps <- 0.01
lwd <- 0.02
dcrs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "
theme_set(theme_bw())
ggh <- ggplot(spp)+
  geom_sf(aes(fill = historical), lwd = lwd)+
  scale_fill_gradientn(colors = cs, name = "Probability")+
  coord_sf(crs = dcrs)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Historical distribution", subtitle = species)+
  theme(plot.margin = rep(unit(ps,"null"),4),
        panel.spacing = unit(ps,"null"))
ggf <- ggplot(spp)+
  geom_sf(aes(fill = future), lwd = lwd)+
  scale_fill_gradientn(colors = cs, name = "Probability")+
  coord_sf(crs = dcrs)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Future distribution", subtitle = paste(species, year, scenario))+
  theme(plot.margin = rep(unit(ps,"null"),4),
        panel.spacing = unit(ps,"null"))
ggd <- ggplot(spp)+
  geom_sf(aes(fill = diff), lwd = lwd)+
  scale_fill_gradientn(colors = cs, name = "Difference", limits = c(-1,1))+
  coord_sf(crs = dcrs)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Difference", subtitle = 'Future - historical')+
  theme(plot.margin = rep(unit(ps,"null"),4),
        panel.spacing = unit(ps,"null"))

gridExtra::grid.arrange(ggh, ggf, ggd, ncol=2, nrow=2)
