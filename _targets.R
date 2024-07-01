source("src/packages.R")
source("src/functions.R")

rootdir = getwd() 
datdir = file.path(rootdir, 'data') #Directory where sample spreadsheet is and where additional data will be downloaded
resdir = file.path(rootdir, 'results') #Directory where reproduceable results will be written

if (!dir.exists(resdir)) {
  dir.create(resdir)
}

global_overwrite = TRUE #Global parameter whether to overwrite output files

#--- Data repository for RiverATLAS and LakeATLAS ----
#URLs to HydroATLAS data
riveratlas_url <- "https://figshare.com/ndownloader/files/20087486" 
lakeatlas_url <- "https://figshare.com/ndownloader/files/35959547"

#Directory where HydroATLAS data will be downloaded 
hydroatlas_dir <- file.path(datdir, "hydroatlas")
if (!dir.exists(hydroatlas_dir)) {
  dir.create(hydroatlas_dir)
}

#Storage option for target (qs is smaller and faster)
tar_option_set(error="stop",
               format = "qs")

#--- Define targets plan ------------------------------------------------------
#This plan lists all the steps to conduct and their order for the analysis

list(
  #Download RiverATLAS
  tar_target(
    riveratlas_pathlist,
    download_hydroatlas_shp(in_url = riveratlas_url, 
                            in_dirpath = hydroatlas_dir,
                            out_zipname = "RiverATLAS_Data_v10_shp.zip", 
                            quiet=F) 
  ),
  
  #Download LakeATLAS
  tar_target(
    lakeatlas_pathlist,
    download_hydroatlas_shp(in_url = lakeatlas_url, 
                            in_dirpath = hydroatlas_dir,
                            out_zipname = "LakeATLAS_Data_v10_shp.zip", 
                            quiet=F) %>%
      grep('LakeATLAS_v10_pol_[a-z]+[.]shp$', ., value=T)
  ),
  
  #Download HydroBASINS level 4
  tar_target(
    hydrobasins4_pathlist,
    lapply(
      c('af', 'ar', 'as', 'au','eu', 'gr', 'na', 'sa', 'si'), function(continent) {
        download_hydroatlas_shp(
          in_url = paste0("https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_",
                          continent, "_lev04_v1c.zip"), 
          in_dirpath = file.path(hydroatlas_dir, 'hydrobasins'),
          quiet=F) 
      }) %>% unlist,
    format = "file"
  )
  ,

  #Download HydroBASINS level 12
  tar_target(
    hydrobasins12_pathlist,
    lapply(
      c('af', 'ar', 'as', 'au','eu', 'gr', 'na', 'sa', 'si'), function(continent) {
        download_hydroatlas_shp(
          in_url = paste0("https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_",
                          continent, "_lev12_v1c.zip"),
          in_dirpath = file.path(hydroatlas_dir, 'hydrobasins'),
          quiet=F)
      }) %>% unlist,
    format = "file"
  )
  ,

  #Download version 3 of Global Aridity Index (example raster to extract values from)
  tar_target(
    gai_path,
    {
      gaidir = file.path(datdir, 'GAI')
      if (!dir.exists(gaidir)) {
        dir.create(gaidir)
      }

      gai_url <- "https://figshare.com/ndownloader/files/34377245"
      gai_zip <- file.path(gaidir, 'Global-AI_ET0_annual_v3.zip')

      if (!file.exists(gai_zip)) {
        options(timeout=1000)
        download.file(url = gai_url,
                      destfile = gai_zip,
                      mode = "wb")
      }

      gai_annual_path <- file.path(gaidir,
                                   'Global-AI_ET0_v3_annual',
                                   'ai_v3_yr.tif')
      if (!file.exists(gai_annual_path)) {
        tryCatch(utils::unzip(zipfile = gai_zip,
                              exdir = gaidir),
                 warning= function(w) rlang::abort(conditionMessage(w)))
      }

      return(gai_annual_path)
    }
  )
  ,

  #Build target for data file path.
  #If file path or the file itself is changed, the analysis will be re-run
  tar_target(
    sites_path,
    file.path(datdir, "Sample.xlsx"),
    format = 'file'
  )
  ,

  #Read data files
  #If content changes, analysis will be re-run
  tar_target(
    sites_pts_river_path,
    read_xlsx(sites_path, sheet="River sample") %>%
      setDT %>%
      create_sitepoints_raw(
            lon_col = 'Longitude',
            lat_col = 'Latitude',
            out_points_path = file.path(resdir, 
                                        paste0('sites_points_river',
                                               format(Sys.Date(), "%Y%m%d"),
                                               ".gpkg")
                                        ),
            columns_to_include = c("FW_ID", "Ecosystem", "Country_ISO", "Country",
                                   "Site_name")
          )
  )
  ,
  
  tar_target(
    sites_pts_lake_path,
    read_xlsx(sites_path, sheet="Lake sample") %>%
      setDT %>%
      create_sitepoints_raw(
        lon_col = 'Longitude',
        lat_col = 'Latitude',
        out_points_path = file.path(resdir, 
                                    paste0('sites_points_lake',
                                           format(Sys.Date(), "%Y%m%d"),
                                           ".gpkg")
        ),
        columns_to_include = c("FW_ID", "Ecosystem", "Country_ISO", "Country",
                               "Site_name")
      )
  )
  ,


  #Identify which HydroBASINS level 4 each site is in to subset RiverATLAS data
  tar_target(
    sites_basins_list,
    intersect_sites_basins(in_sites_path = sites_pts_river_path,
                           in_basins4_pathlist = hydrobasins4_pathlist,
                           in_basins12_pathlist = hydrobasins12_pathlist)
  )
  ,

  #Subset river segments to keep only those in basins with sites
  tar_target(
    riveratlas_sub,
    subset_riveratlas(
      in_basins_list = sites_basins_list$basin_ids,
      in_riveratlas_pathlist = riveratlas_pathlist,
      out_gpkg_path = file.path(resdir,
                                paste0('netsub_',
                                       format(Sys.Date(), '%Y%m%d'),
                                       '.gpkg')),
      overwrite = global_overwrite
    )
  )
  ,

  #Snap river sites to nearest segment in RiverATLAS and extract associated attributes
  tar_target(
    river_sites_snapped,
    snap_river_sites(
      in_sites_path = sites_pts_river_path,
      in_sites_pfafid = sites_basins_list$pfafid4,
      in_riveratlas_sub = riveratlas_sub,
      out_snapped_sites_path = file.path(resdir,
                                         paste0('river_samples_snapped',
                                                format(Sys.Date(), '%Y%m%d'),
                                                '.gpkg')),
      overwrite = global_overwrite)
  )
  ,

  # #Snap river sites to nearest segment in LakeATLAS and extract associated attributes
  tar_target(
    lake_sites_snapped,
    snap_lake_sites(
      in_sites_path = sites_pts_lake_path,
      in_lakeatlas_pathlist = lakeatlas_pathlist,
      out_snapped_sites_path = file.path(resdir,
                                         paste0('lake_samples_snapped',
                                                format(Sys.Date(), '%Y%m%d'),
                                                '.gpkg')),
      overwrite = global_overwrite)
    )
  ,

  #Example extraction of raster values (Global Aridity Index version 3)
  tar_target(
    sites_gai,
    #For both river and lake sites
    lapply(c(river_sites_snapped$geom_path, lake_sites_snapped$geom_path),
           function(pts_path) {
             pts <- vect(pts_path) #read site points
             pts_extract <- terra::extract(rast(gai_path), pts) #read raster and extract data
             pts_extract[, 'FW_ID'] <- values(pts)[, 'FW_ID'] #Re-assign unique IDs to table

             return(as.data.table(pts_extract[, -1]))
           }
    ) %>% rbindlist %>%
      as.data.frame
  )
  ,

  #Join raster data to HydroATLAS attributes and export to csv tables for
  #rivers and lakes, respectively
  tar_target(
    output_sitestab_pathlist,
    {
      river_attri_tab <- file.path(resdir, 
                                   paste0('river_samples_attri',
                                          format(Sys.Date(), '%Y%m%d'),
                                          '.csv'))
      if (!file.exists(river_attri_tab) | global_overwrite) {
        merge(river_sites_snapped$attri_df, sites_gai, by='FW_ID') %>%
          fwrite(river_attri_tab)
      }

      lake_attri_tab <- file.path(resdir, 
                                  paste0('lake_samples_attri',
                                         format(Sys.Date(), '%Y%m%d'),
                                         '.csv'))
      if (!file.exists(lake_attri_tab) | global_overwrite) {
        merge(lake_sites_snapped$attri_df, sites_gai, by='FW_ID') %>%
          fwrite(lake_attri_tab)
      }

      return(c(river_attri_tab, lake_attri_tab))
    },
    format='file'
  )
)

