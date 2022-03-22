#' retrieve vegetation structure data from NEON
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @seealso [neonUtilities::loadByProduct()] which this function wraps.
#' @export
#' @examples
#' @importFrom magrittr "%>%"
#'
#'  pt = "./outdir/field_data/neon_vst_data_geoNeon_2022.csv"

retrieve_VST_data <- function(site = "all"
                              , start = NA
                              , enddate = NA
                              , method = "shp"
                              , pt = NA
                              ){

  # load NEON woody vegetation structure data product into R
  vst <- neonUtilities::loadByProduct("DP1.10098.001", check.size=F,
                                      site=site) #, "2021-01", "2022-01")

  vst$vst_apparentindividual %>% dim
  # calculate UTM coordinates of vst entries based on azimuth and distance
  # measurements from plot reference points
  # if(method == "geoneon"){
  #   vst <- calc_tree_geolocations(vst, dataProd = "vst_mappingandtagging")
  # }else
  if(method == "shp"){
    vst$vst_mappingandtagging <- retrieve_coords_itc(vst$vst_mappingandtagging)
    #get latitude and longitude of all points
    #vst$vst_mappping_latlon <- get_lat_long(vst$vst_mappingandtagging)
  }
  # }else if(method == "all"){
  #   vst2 <- calc_tree_geolocations(vst, dataProd = "vst_mappingandtagging")
  #   vst$vst_mappingandtagging <- retrieve_coords_itc(vst$vst_mappingandtagging)
  #   #get latitude and longitude of all points
  #   vst$vst_mappping_latlon <- get_lat_long(vst$vst_mappingandtagging)
  #   colnames(vst$vst_mappping_latlon)
  #   vst2_coords = vst2$vst_mappingandtagging %>% select(individualID, northing, easting, adjDecimalLatitude, adjDecimalLongitude)
  #   colnames(vst2_coords)[2:3]<- c("adjNorthing", "adjEasting")
  #   vst$vst_mappping_latlon = left_join(vst$vst_mappping_latlon, vst2_coords)
  #   vst$vst_mappping_latlon = unique(vst$vst_mappping_latlon)
  # }

  # tmp_mapping_and_tag = vst$vst_mappping_latlon %>% select(-one_of("uid"))
  attributes = vst$vst_apparentindividual %>%
    select(uid, individualID, eventID, tagStatus, growthForm, plantStatus, stemDiameter,
           measurementHeight, height,baseCrownHeight, breakHeight,
           breakDiameter, maxCrownDiameter, ninetyCrownDiameter,
           canopyPosition, shape, basalStemDiameter,
           basalStemDiameterMsrmntHeight, maxBaseCrownDiameter, ninetyBaseCrownDiameter)
  colnames(vst$vst_mappingandtagging)[4] = "tagEventID"
  csv_vst = left_join( attributes, vst$vst_mappingandtagging, by="individualID") %>% unique
  vst$vst_apparentindividual %>% nrow()
  #colnames(vst$vst_mappingandtagging)[35:36] <- c("plotEasting", "plotNorthing")

  if(!is.na(pt)) write_csv(csv_vst, pt)
  return(list(vst = csv_vst, raw_dat = vst))
}

