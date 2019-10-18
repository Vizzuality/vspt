spp_list =
  c('Picea abies',
    'Picea glauca',
    'Picea pungens',
    'Picea obovata',
    'Picea omorika',
    'Picea sitchensis',
    'Pinus cembra',
    'Pinus contorta',
    'Pinus mugo',
    'Pinus nigra',
    'Pinus sibirica',
    'Pinus strobus',
    'Pinus sylvestris'
  )

split_table <- function(spp_name, sfc){
  nm <- paste0(unlist(strsplit(spp_list[1], " ")), collapse = ".")
  nms <- names(spp_out)
  return(sfc[c('uuid', 'area_m2', 'permieter_m', grep(nm, nms)])
}

sfc_list <- lapply(spp_list, split_table, sfc=spp_out)
names(sfc_list) <- spp_list
# csv
mapply(function(x, y){
  nm <- paste0(unlist(strsplit(y, " ")), collapse = "-")
  sf::st_write(x,
             paste0("SWE_", nm, ".csv"),
             layer_options = c("GEOMETRY=AS_WKT", "GEOMETRY_NAME=geometry", "SEPARATOR=COMMA", "OGR_WKT_PRECISION=7"),
             delete_dsn=TRUE
)}, sfc_list, spp_list)
