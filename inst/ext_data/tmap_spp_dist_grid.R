# Plot species distributions using tmap
load("ISEA-3-HEXAGON_grid.rda")
species <- attr(spp_out, "spp_list")[1]
stat <- 'EMca'
year <-2090
scenario <-'rcp85'

# Get spp occurence
#load("spp_occurence_sfc_list.rda")
oc <- spp_occurence_sfc_list[[species]]

pfp <- function(iso, tp, spp, stat, y=NULL, s=NULL){
  spp <- gsub(" ",".", spp)
  if(tp=='historical') out <- paste(iso, tp, spp, stat, sep = ".")
  if(tp=='future') out <- paste(iso, tp, y, s, spp, stat, sep = ".")
  return(out)
}

h <- pfp('biovar_Spain', 'historical', species, stat)
f <- pfp('biovar_Spain', 'future', species, stat, year, scenario)
spp <- spp_out[,c("uuid", h, f)]
names(spp) <- c('uuid', 'historical', 'future', 'geometry')
spp$historical <- spp$historical/1000
spp$future <- spp$future/1000
spp$diff <- spp$future - spp$historical
names(spp)
cs <-
  (colorRampPalette(c('#7044ff', '#F7F7F71A', '#7fbf7b'))(5))
print(species)



# Plot
require(tmap)
  tmap_mode("view")
  #adjustcolor( '#f7f7f7', alpha.f = 0.1)
  cs <-
    (colorRampPalette(c('#7044ff', '#F7F7F71A', '#7fbf7b'))(5))
  alpha <- 0.7
  tm_shape(spp) +
    tm_borders(alpha = 0.2) +
    tm_fill(
      c("historical", "future"),
      style = "fixed"
      , breaks = c(0, 0.25, 0.5,0.75, 1)#c(-1, -0.500, -0.25, 0.25, 0.500, 1)
      , title = c(paste(species,"historical"), paste(species, year, scenario))
      , alpha = alpha
      , palette = cs
      , midpoint = 0) +
    #tm_shape(oc) +
    #tm_dots(
    #  alpha = 0.4,
    #  size = 0.01,
    #  clustering = T,
    #  col = "darkgrey"
    #)
    tm_facets(sync = TRUE, ncol = 2)+
    tm_scale_bar()
