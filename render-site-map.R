library(rgeos)
library(dplyr)
library(raster)
library(mapview)
library(sf)

# render leaflet map from traits for a given date
render_site_map <- function(traits, render_date, legend_title, image_paths = NULL) {
  image_dir <- '~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs'
  print('starting render site map')
  # get most recent traits for each site
  # convert each site's geometry to a sfc object
  print('1')
  latest_traits <- subset(traits, as.Date(date) <= render_date) %>% 
    group_by(geometry) %>% 
    top_n(1, date) %>% 
    mutate(site_poly = st_as_sfc(geometry))
  
  print('2')
  pal <- colorNumeric(
    palette = 'Greens',
    domain = traits[[ 'mean' ]]
  )
  
  print('3')
  map <- leaflet(options = leafletOptions(minZoom = 18, maxZoom = 21))  %>%
    addProviderTiles(providers$Esri.WorldImagery) 
  
  map <- fitBounds(map,
                   lng1 = -111.97520,
                   lng2 = -111.97470,
                   lat1 = 33.07650,
                   lat2 = 33.07440)
  print('4')
  # add polygon for each site, color by trait mean value
  # coerce data to multipolygon
  map <- addFeatures(map,
                     data = st_cast(latest_traits[[ 'site_poly' ]], "MULTIPOLYGON"),
                     color = pal(latest_traits[[ 'mean' ]]),
                     opacity = 0,
                     fillColor = pal(latest_traits[[ 'mean' ]]),
                     fillOpacity = 0.8,
                     group = 'Heat map')
  
  print('5')
  map <- addLegend(map, "bottomright", 
                   pal = pal, 
                   title = legend_title,
                   values = traits[[ 'mean' ]])
  
  print('6')
  map <- addLayersControl(map,
                          overlayGroups = "Heat map",
                          position = "topleft")
  
  
  
  print('7')
  if(!is.null(image_paths) & !(FALSE %in% grep(render_date, list.files(image_dir), value = TRUE))){
    # eventually want to overlay with stitched image from current day
    # see /data/terraref/sites/ua-mac/Level_1/fullfield/
    # cannot use addRasterImage (https://rstudio.github.io/leaflet/raster.html) for RasterStack 
    # using multiband RGB thumbs, so use mapview::viewRGB() instead
    print('a')
    for(path in image_paths){
      print('a')
      print(path)
      print('b')
      scan_number <- which(image_paths == path)
      scan_name <- paste0('scan ', scan_number)
      print(scan_number)
      print('c')
      fullfield_image <- stack(paste0('~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs/',
                                      path))
      print('d')
      map <- viewRGB(x = fullfield_image, map = map, layer.name = scan_name)
      print('e')
      map <- removeHomeButton(map@map)
     3}
    #for(i in 1:length(image_paths)){
    #  print(i)
    #  print('b')
    #  scan_choice <- paste0('scan ', i)
    #  print(image_paths[i])
    #  print('c')
    #  fullfield_image <- stack(paste0('~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs/',
    #                                  image_paths[i]))
    #  print('d')
    #  map <- viewRGB(x = fullfield_image, map = map, layer.name = scan_choice)
    #  print('e')
    #  map <- removeHomeButton(map@map)
    #}
  }
  
  print('about to print map')
  map
}

