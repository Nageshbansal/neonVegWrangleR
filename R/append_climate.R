library(data.table)
library(tidyverse)
df_environment = fread("/Volumes/Data/FIA/PLOT.csv",
                         select = c("STATECD", "COUNTYCD","UNITCD", "PLOT", "LAT", "LON")) %>% unique
df_environment = df_environment[complete.cases(df_environment), ]
df_environment = df_environment %>% group_by(STATECD, UNITCD, COUNTYCD, PLOT)%>%
  summarize_each(mean)


# fhm_coords = FHM %>% data.frame
# colnames(fhm_coords)[2:3] = c("STATECD", "COUNTYCD")
# neon_environment = left_join(fhm_coords, neon_environment)
df_environment = df_environment %>% filter(LAT > -999, LON > -999)
df_environment = sf::st_as_sf(df_environment, coords=c("LON", "LAT"), crs = 4326)


get_climate_df <- function(neon_environment
                           , in_pt = "/Volumes/Data/PRISM_normals_80_10/"
                           , out_pt =  "/Volumes/Data/PRISM_normals_80_10/FIA.csv"
                           ){
  #list files
  ls_clim = list.files(in_pt, pattern = "\\.asc",
                       recursive = T, full.names = T)
  ls_clim = ls_clim[!ls_clim %like% ".aux.xml"]
  ls_clim = ls_clim[!ls_clim %like% "annual"]
  clim = list()
  for(fl in ls_clim){
    tryCatch(expr = {
      r = raster::raster(fl)
      neon_environment = sf::st_transform(neon_environment, crs = sf::st_crs(r))
      tmp = raster::extract(r, neon_environment, df=T)
      feat = strsplit(colnames(tmp)[2], split = "_")[[1]]
      feat = paste(feat[2], feat[6], sep="_")
      colnames(tmp)[2] = feat
      clim[[feat]] = tmp[[feat]]
    },    error=function(cond) {})
  }
  results = do.call(cbind.data.frame, clim)
  results = cbind.data.frame(neon_environment, results)
  write_csv(results, out_pt)
}


get_nadp_df <- function(neon_environment
                        , in_pt = "/Volumes/Data/NADP/"
                        , out_pt =  "/Volumes/Data/NADP/FIA.csv"
                        ){
  #list files
  ls_clim = list.files(in_pt, pattern = "\\.tif",
                       recursive = T, full.names = T)
  ls_clim = ls_clim[!ls_clim %like% ".aux.xml"]
  ls_clim = ls_clim[!ls_clim %like% ".tif.ov"]
  ls_clim = ls_clim[!ls_clim %like% ".tif.xml"]
  clim = list()
  for(fl in ls_clim){
    r = raster::raster(fl)
    neon_environment = sf::st_transform(neon_environment, crs = sf::st_crs(r))
    tmp = raster::extract(r, neon_environment, df=T)
    feat = (colnames(tmp)[2])
    #feat = paste(feat[2], feat[6], sep="_")
    #colnames(tmp)[2] = feat
    clim[[feat]] = tmp[[feat]]
  }
  results = do.call(cbind.data.frame, clim)
  results = cbind.data.frame(neon_environment, results)
  fhm = results %>% data.frame %>% filter(SplusN_dep_2018 > 0) %>%
    select(-one_of("geometry", "PLOT", "UNITCD"))%>%
    group_by(STATECD,COUNTYCD) %>% summarize_each(mean)
  write_csv(fhm, out_pt)

}



neon_environment = fread("/blue/ewhite/s.marconi/Chapter3/neonVegWrangleR/indir/neon_vst_data_022021.csv",
                         select = c("individualID", "latitude", "longitude")) %>% unique
neon_environment = neon_environment[complete.cases(neon_environment), ]
neon_environment = sf::st_as_sf(neon_environment, coords=c("longitude", "latitude"), crs = 4326)


neon_environment = fread("/orange/ewhite/s.marconi/FIA/PLOT.csv",
                         select = c("STATECD", "COUNTYCD","UNITCD", "PLOT", "LAT", "LON")) %>% unique
neon_environment = neon_environment[complete.cases(neon_environment), ]
neon_environment = sf::st_as_sf(neon_environment, coords=c("LON", "LAT"), crs = 4326)

get_soil_df <- function(neon_environment){
  #list files
  ls_clim = list.files("/orange/idtrees-collab/environmental_data/", pattern = "\\.tif",
                       recursive = T, full.names = T)
  ls_clim = ls_clim[!ls_clim %like% ".aux.xml"]
  #ls_clim = ls_clim[!ls_clim %like% "annual"]
  clim = list()
  for(fl in ls_clim){
    tryCatch(expr = {

      r = raster::raster(fl)
      neon_environment = sf::st_transform(neon_environment, crs = sf::st_crs(r))
      tmp = raster::extract(r, neon_environment, df=T)
      feat = strsplit(colnames(tmp)[2], split = "_")[[1]]
      feat = paste(feat[2], feat[6], sep="_")
      colnames(tmp)[2] = feat
      clim[[feat]] = tmp[[feat]]
    },    error=function(cond) {})
  }
  results = do.call(cbind.data.frame, clim)
  results = cbind.data.frame(neon_environment, results)
  write_csv(results, "./soil_CSC.csv")
}

