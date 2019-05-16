# Check required package --------------------------------------------------

pkgs <- c('mapsapi',"xml2", "censusr")
check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}


KEY <- Sys.getenv("GOOGLE_MAP_API")

add2FIPS <- function (add,KEY){
  # get the lat and lng from google map, using full address (street + county + state)
  result <- mp_geocode(add,key = KEY)

  t <- try({
    lat <- result[[1]] %>%
      xml_find_first("/GeocodeResponse/result/geometry/location/lat")%>%
      xml_text()%>%
      as.numeric
    lng <- result[[1]] %>%
      xml_find_first("/GeocodeResponse/result/geometry/location/lng")%>%
      xml_text()%>%
      as.numeric

    # use census geolocator to get fips code from coordinates
    FIPS <- censusr::call_geolocator_latlon(lat, lng)},
    silent = TRUE)

  print(paste0(lat,",", lng))

  if("try-error" %in% class(t)) FIPS <- NA


  return(FIPS)

}

# test
# add2FIPS("Albany State University, Albany, Georgia",KEY)
#
# add2FIPS("Garnet Valley, Pennsylvania", KEY)

fips <- add2FIPS("Landenberg, Pennsylvania", KEY)
