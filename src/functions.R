################## UTILITY FUNCTIONS ###########################################
#Those functions are embedded in workflow functions in the next section

#--- Define custom projection  -------------------------------------------------
# Define standard two-point equidistance projection for a given bounding box
#https://gis.stackexchange.com/questions/313721/automatically-get-an-adequate-projection-system-based-on-a-bounding-box
## distance projection (tpeqd - two-point equidistant) with projection parameters 
## derived from feature extent
dist_proj <- function(x) {
  bb <- sf::st_bbox(x)
  paste0("+proj=tpeqd +lat_1=",
         bb[2],
         " +lon_1=", 
         bb[1],
         " +lat_2=",
         bb[4], 
         " +lon_2=", 
         bb[3],
         " +x_0=0",
         " +y_0=0",
         " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

#--- Snap sites to nearest segment ---------------------------------------------
#Custom snap method. Lighter and faster than other tests options
#sf::st_snap doesn't work
#maptools::snapPointsToLines requires SpatialPoint and SpatialLines - too heavy/slow for this dataset
#The option used here relies on terra::nearest, which is faster and returns only 
# one line for each point compared to sf::st_nearest_points
snap_sites <- function(in_sites_point, 
                       in_sitesSQL="", #SQL expression to subset sites to be snapped
                       in_target_path, 
                       in_targetSQL="", #SQL expression to subset objects that sites will be snapped to
                       sites_idcol = 'FW_ID', 
                       custom_proj = T, #Whether to first re-project data
                       attri_to_join = NULL, #Either a single name, a vector of character, or "all"
                       write_snapped = F, #Whether to write snapped points (out_path)
                       out_path=NULL,
                       overwrite = F) {
  
  #Read target feature class
  target <- terra::vect(in_target_path, query = in_targetSQL)
  
  #Project sites 
  # Global datasets tend to be in geographic coordinates. The unit of these 
  # coordinates are decimal degrees, whose west-east length decreases
  # with increasing latitude. Therefore, for identifying the nearest line,
  # which is based on distance calculation, the point dataset needs to be 
  # projected. However, no single projection is valid for the entire planet. 
  # Consequently, for each basin, the sites are projected using a custom 
  # projection which minimizes distortions for distance calculations within the
  # network bounding box.
  if (custom_proj) {
    if (nrow(target) > 1 & ((xmax(target) != xmin(target)) | 
                            (ymax(target) != ymin(target)))
    ){
      target_proj <- terra::project(target, 
                                    dist_proj(target))
    } else {
      #if only one target object, project to UTM
      target_proj <- terra::project(
        target,
        paste0('+proj=utm +zone=', 
               floor((xmin(target) + 180) / 6) + 1,
               ' +datum=WGS84 +units=m +no_defs +ellps=WGS84')
      )
    } 
  } else {
  }

  remove(target)
  
  #Read sites
  if (is.character(in_sites_point)) {
    sitesp <- terra::vect(in_sites_point, query = in_sitesSQL)
  } else if (inherits(in_sites_point, 'SpatVector')) {
    sitesp <- in_sites_point  
  }
  
  #Project sites to custom projection
  sitesp_proj <- terra::project(sitesp, crs(target_proj))
  
  #Snap points (fastest custom way in R, it seems):
  #first computing a line between site and snapping place on nearest segment
  sitesnap_l <- terra::nearest(sitesp_proj, target_proj, centroids = F, lines = T)
  values(sitesnap_l) <- values(sitesp_proj)
  sitesnap_l$snap_dist_m <- perim(sitesnap_l)
  
  #convert the line to a point (the line's end point)
  sitesnap_p <- terra::as.points(sitesnap_l) %>%
    .[duplicated(values(.)[, sites_idcol]),]
  
  #Join attributes of nearest line to that point
  if (!is.null(attri_to_join)) {
    if (attri_to_join == 'all') { 
      sitesnap_p[, names(target_proj)] <- terra::nearby(
        sitesnap_p, target_proj, k=1, centroids=FALSE)[,'k1'] %>% #Could grab the nth nearest or place a distance limit
        as.data.frame(target_proj)[.,] 
    } else {
      sitesnap_p[, attri_to_join] <- terra::nearby(
        sitesnap_p, target_proj, k=1, centroids=FALSE)[,'k1'] %>%
        as.data.frame(target_proj)[., attri_to_join] 
    }
  }
  
  #Reproject points to WGS84
  sitesnap_p <- terra::project(sitesnap_p, "+proj=longlat +datum=WGS84")
  
  if (write_snapped) {
    terra::writeVector(sitesnap_p,
                       out_path,
                       overwrite=overwrite)
  }
  
  return(sitesnap_p)
}


################## WORKFLOW FUNCTIONS ##########################################
#--- Download and unzip  data ------------------
#in_dirpath is the directory to which the files will be downloaded
#if in_dirpath does not exist, the directory will be created
#out_zipname is the name of the downloaded zip file
#Downloading will not take place if the file already exists
#Unzipping will not take place for existing files
download_hydroatlas_shp <- function(in_url, 
                                    in_dirpath,
                                    out_zipname = NULL, 
                                    quiet=T,
                                    overwrite=F) {
  if (!dir.exists(in_dirpath)) {
    dir.create(in_dirpath)
  }
  
  #Download shp
  if (is.null(out_zipname)) {
    out_zipname <- basename(in_url)
  }
  
  shp_zip_path <- file.path(in_dirpath, out_zipname)
  if (!(file.exists(shp_zip_path))) {
    httr2::request(in_url) %>%
      httr2::req_perform(path = shp_zip_path)
  }
  
  #List all files in zipped file
  files_to_unzip <- utils::unzip(zipfile = shp_zip_path, 
                                 list=TRUE) %>%
    setDT
  
  #Get path of unzipped directory
  shp_unzip_path <- tools::file_path_sans_ext(shp_zip_path)
  if (file.exists(shp_unzip_path)) {
    files_to_unzip <- files_to_unzip[
      Length > 0 &
        !(Name %in% 
            list.files(shp_unzip_path, recursive=T)
        ),]
  }
  
  #Unzip data (not those files already present)
  if (nrow(files_to_unzip) > 0L) {
    tryCatch(utils::unzip(zipfile = shp_zip_path, 
                          files = files_to_unzip$Name,
                          exdir = shp_unzip_path,
                          overwrite = overwrite),
             warning= function(w) rlang::abort(conditionMessage(w)))
  }
  
  #Get list of unzipped files
  shplist <- file.path(
    shp_unzip_path,
    list.files(shp_unzip_path, pattern = '.*[.]shp$', recursive=T)
  )
  
  return(shplist)
}

#--- Create points from coordinates --------------------------------------------
create_sitepoints_raw <- function(in_dt, lon_col, lat_col, out_points_path,
                                  columns_to_include) {
  #Create point feature class from formatted site data
  sitesp <- terra::vect(in_dt,
                        geom = c(lon_col, lat_col),
                        crs = "+proj=longlat +datum=WGS84")
  terra::writeVector(sitesp[, columns_to_include], 
                     out_points_path, overwrite=TRUE)
  return(out_points_path)
}

#--- Situate points in basins---------------------------------------------------
#Identify which HydroBASINS level 4 each site is in and the IDs of all HydroBASINS
#level 12 in those basins.
intersect_sites_basins <- function (in_sites_path,
                                    in_basins4_pathlist,
                                    in_basins12_pathlist) {
  #Import sites as vector
  if (length(in_sites_path) > 1) {
    sitesp <- do.call(rbind, lapply(in_sites_path, vect))
  } else {
    sitesp <- terra::vect(in_sites_path)
  }

  #Read HydroBASINS level 4 and get PFAF_ID for each site
  # (see technical doc; it's a hierarchical ID system for nested basins)
  PFAF_ID_list <- lapply(in_basins4_pathlist, vect) %>%
    vect %>%
    terra::extract(y = sitesp)
  PFAF_ID_list$FW_ID <- sitesp$FW_ID

  #Create reference of abbreviations in file names associated with continents
  #(from HydroBASINS technical documentation:
  # https://data.hydrosheds.org/file/technical-documentation/HydroBASINS_TechDoc_v1c.pdf)
  continent_IDs <- data.table(
    continent_digit = seq(1,9),
    continent_name = c('Africa', 'Europe', 'Siberia', 'Asia', 'Australia',
                       'South America', 'North America (excluding Arctic)',
                       'Arctic (North America)', 'Greenland'),
    continent_abbr = c('af', 'eu', 'si', 'as', 'au', 'sa', 'na', 'ar', 'gr')
  )

  #Identify all HydroBASINS level 12 (the finest level) in hydroBASINS level 4
  #where sites are located. This intermediate step is necessary because
  #RiverATLAS is associated only with HydroBASINS level 12
  basins12_intersecting_IDs <- lapply(in_basins12_pathlist, function(in_path) {
    read.dbf(gsub("[.]shp$", ".dbf", in_path)) %>% #Import attribute data (.dbf file)
      setDT %>% #Convert it to a data.table object for fast data manipulation
      .[, `:=`(
        PFAF_ID4 = PFAF_ID %/% 10^8, #Get PFAF_ID of basin level 4 of which it is part
        continent_digit = HYBAS_ID%/%10^9 #Get digit ID corresponding to the continent where it is located
      )] %>%
      merge(PFAF_ID_list[!is.na(PFAF_ID_list$HYBAS_ID) & !duplicated(PFAF_ID_list$HYBAS_ID), 
                         c('SORT', 'PFAF_ID')], #Associate with sites
            by.x='PFAF_ID4', by.y='PFAF_ID',
            all.x=F, all.y=F) %>%
      .[, c('PFAF_ID4', 'HYBAS_ID', 'continent_digit'), with=F]
  }) %>%
    rbindlist %>% #Merge data for all continents
    merge(continent_IDs, by='continent_digit') # Get ancillary data on continent names and abbreviations

  return(list(basin_ids = as.data.frame(basins12_intersecting_IDs), #Output as dataframe rather than data table because of targets bug
              pfafid4 = PFAF_ID_list[, c('FW_ID', 'PFAF_ID')])
  )
}

#--- Read network from basins --------------------------------------------------
#Subset RiverATLAS to only include segments within the HydroBASINS level 4
#where sites are located.
subset_riveratlas <- function(in_basins_list,
                              in_riveratlas_pathlist,
                              out_gpkg_path,
                              overwrite=F) {
  
  setDT(in_basins_list)
  
  #Get list of continents where there are sites
  continent_list <- in_basins_list[, unique(continent_abbr)]
  
  #For each continent
  net_sub_attri <- lapply(continent_list, function(cont) {
    print(cont)
    basins_sub <- in_basins_list[continent_abbr == cont,]
    net_path <- grep(paste0('RiverATLAS_v10_', cont, '[a-z_]*[.]shp$'), #Get RiverATLAS file path
                     in_riveratlas_pathlist,
                     value=T)
    
    #Read RiverATLAS attributes (dbf file associated with shapefile)
    if (length(net_path) > 1) {
      riveratlas_attri_sub <- lapply(net_path, function(in_path) {
        read.dbf(gsub("[.]shp$", ".dbf", in_path)) %>%
          setDT %>% #Convert to data.table format for fast manipulation
          .[HYBAS_L12 %in% basins_sub$HYBAS_ID,] %>% #Only keep segment attributes in basins where there are sites
          .[, continent_abbr := cont]
      }) %>% rbindlist
    } else {
      riveratlas_attri_sub <- read.dbf(gsub("[.]shp$", ".dbf", net_path)) %>%
        setDT %>% #Convert to data.table format for fast manipulation
        .[HYBAS_L12 %in% basins_sub$HYBAS_ID,] %>% #Only keep segment attributes in basins where there are sites
        .[, continent_abbr := cont]
    }
    
    return(riveratlas_attri_sub)
  }) %>% rbindlist
  
  if (!file.exists(out_gpkg_path) | overwrite) { #If output file doesn't already exist or overwrite==TRUE
    net_sub_geom <- lapply(continent_list, function(cont) { #For each continent
      print(cont)
      net_path <- grep(paste0('RiverATLAS_v10_', cont, '[a-z_]*[.]shp$'),
                       in_riveratlas_pathlist,
                       value=T)

      #Only read geometry data and ID column
      if (length(net_path) > 1) {
        net_sub_cont <- lapply(net_path, function(in_path) {
          net_lyr_name <- tools::file_path_sans_ext(basename(in_path))
          terra::vect(in_path, 
                      query=paste("SELECT HYRIV_ID FROM",
                                  net_lyr_name))
        }) %>% vect
      } else {
        net_lyr_name <- tools::file_path_sans_ext(basename(net_path))
        net_sub_cont <- terra::vect(net_path, 
                                    query=paste("SELECT HYRIV_ID FROM",
                                                net_lyr_name))
      }
      
      #Only keep segment geometries in basins where there are sites
      return(merge(net_sub_cont, 
                   net_sub_attri[continent_abbr == cont,.(HYRIV_ID, HYBAS_L12)],
                   by='HYRIV_ID', all.x=F)
      )
    }) %>%
      vect(.) %>% #Merge data for all continents
      merge(in_basins_list, by.x='HYBAS_L12', by.y='HYBAS_ID') #Join basin IDs
    
    terra::writeVector(net_sub_geom, out_gpkg_path, overwrite=T)
  } 
  
  return(list(attri_df =  as.data.frame(net_sub_attri), #Attribute table
              geom_path = out_gpkg_path) #Path to geopackage containing segments in basins where there are sites
  )
}

#--- Snap sites to nearest segment and get attributes --------------------------
snap_river_sites <- function(in_sites_path, 
                             in_sites_pfafid,
                             in_riveratlas_sub,
                             out_snapped_sites_path, 
                             overwrite = F) {
  
  if (!file.exists(out_snapped_sites_path) | overwrite) {
    #Iterate over every basin where there is a site
    if (length(in_sites_path) > 1) {
      sites <- do.call(rbind, lapply(in_sites_path, vect)) 
    } else {
      sites <- terra::vect(in_sites_path)
    }
    
    sites <- merge(sites, in_sites_pfafid, by.x='FW_ID', by.y='FW_ID')
    
    sites_snapped <- lapply(unique(sites[!is.na(sites$PFAF_ID),]$PFAF_ID), 
                            function(in_pfafid) {
      #Subset sites
      sites_sub <- sites[sites$PFAF_ID == in_pfafid,]
      
      #Expression to subset network (only in basin where there is a site)
      SQLquery_net <- paste0(
        "SELECT * FROM ",
        tools::file_path_sans_ext(basename(in_riveratlas_sub$geom_path)),
        " WHERE PFAF_ID4 =", 
        in_pfafid)
      
      #Snap sites to nearest segment in RiverATLAS and get initial distance to that segment
      sites_snapped_sub <- snap_sites(in_sites_point = sites_sub, 
                                      in_target_path = in_riveratlas_sub$geom_path,
                                      in_targetSQL= SQLquery_net,
                                      sites_idcol = 'FW_ID',
                                      attri_to_join = 'HYRIV_ID',
                                      custom_proj = T,
                                      write_snapped = F)
      
      return(sites_snapped_sub)
    }) %>%
      vect(.) #Merge snapped sites from all basins
    
    #Get RiverATLAS attributes for segments that sites where snapped to
    sites_snapped_attri <- merge(sites_snapped, 
                                 in_riveratlas_sub$attri_df, 
                                 by='HYRIV_ID')
    
    #Write resulting dataset
    terra::writeVector(sites_snapped_attri, out_snapped_sites_path, 
                       overwrite = overwrite)
  } else{
    sites_snapped_attri <- vect(out_snapped_sites_path)
  }
  
  return(list(
    attri_df = values(sites_snapped_attri), #Attribute table 
    geom_path = out_snapped_sites_path #Path to geopackage containing site points with attribute data
  ))
}


#--- Snap sites to nearest lake ------------------------------------------------
snap_lake_sites <- function(in_sites_path, 
                            in_lakeatlas_pathlist,
                            out_snapped_sites_path, 
                            overwrite = F) {
  
  if (!file.exists(out_snapped_sites_path) | overwrite) {
    #Iterate over every basin where there is a site
    if (length(in_sites_path) > 1) {
      sites <- do.call(rbind, lapply(in_sites_path, vect)) 
    } else {
      sites <- terra::vect(in_sites_path)
    }
    
    #LakeATLAS is divided in two zones (east and west), so iterate over these
    sites_snapped <- lapply(in_lakeatlas_pathlist, function(in_lakeatlas_path) {
      print(in_lakeatlas_path)
      #Snap sites to nearest lake, get initial distance to that lake
      #and join all attributes from that lake
      sites_snapped_sub <- snap_sites(in_sites_point = sites, 
                                      in_target_path = in_lakeatlas_path,
                                      sites_idcol = 'FW_ID',
                                      attri_to_join = 'all',
                                      custom_proj = T,
                                      write_snapped = F)
      
      return(sites_snapped_sub)
    }) %>%
      vect(.) #Merge snapped sites from eastern and western portions of LakeATLAS
    
    #Only keep unique site-lake pairs for matching LakeATLAS portions
    #(i.e., sites in western portion where matched to very distance lakes in 
    #eastern portion, and vice versa, during the process so all sites are duplicated)
    sites_snapped_nodupli <- sites_snapped[order(sites_snapped$snap_dist_m),] %>%
      .[!duplicated(.$'FW_ID'),] 
    
    terra::writeVector(sites_snapped_nodupli, out_snapped_sites_path, 
                       overwrite = overwrite)
  } else {
    sites_snapped_nodupli <- vect(out_snapped_sites_path)
  }
  
  return(list(
    attri_df = values(sites_snapped_nodupli), #Attribute table
    geom_path = out_snapped_sites_path #Path to geopackage containing site points with attribute data
  ))
}

#------ extract data from raster -----------------------------------------------
extract_inner <- function(in_sites_path, in_rast_path) {
  pts <- vect(in_sites_path) #read site points
  pts_extract <- terra::extract(rast(in_rast_path), pts) #read raster and extract data
  pts_extract[, 'FW_ID'] <- values(pts)[, 'FW_ID']
  return(as.data.table(pts_extract[, -1]))
}
extract_raster_to_sites <- function(in_sites_path,
                                    in_rast_path) {
  if (length(in_sites_path) > 1) {
    pts_extract_bind <- lapply(in_sites_path, function(x) {
      return(extract_inner(x, in_rast_path))
    }) %>%
      rbindlist
      
  } else {
    pts_extract_bind <- extract_inner(in_sites_path, in_rast_path) 
  }
  return(as.data.frame(pts_extract_bind))
}
