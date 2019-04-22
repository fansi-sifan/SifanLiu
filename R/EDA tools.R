
# To do ==========
# error handling

# MAIN ===========

# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n))

# Share of NA values
na.share <- function(x)(sum(is.na(x))/length(x))

# quickly summarize factor levels
sfactor <- function(x)summary(as.factor(as.character(x)))

# read all csv files in specified folder and bind_rows
read_all <- function(file.location){
  map_df(list.files(file.location, full.names = T), read.csv)
}

# Sat Apr 13 04:07:09 2019 ------------------------------
# wrap getcensus::getacs to simplify metro geography
GetACS <- function(name,varlist,geotype, time = NULL, vintage = NULL){
  if(geotype == "msa"){
    region = "metropolitan statistical area/micropolitan statistical area:*"
  } else if (geotype == "MSAs"){
    region = "metropolitan statistical areas:*"
  } else if (geotype == "MSA"){
    region = "metropolitan statistical area:*"
  } else if (geotype == "county"){
    region = "county:*"
  } else {
    region = "state:*"
  }

  data <- getCensus(name = name,
                    vintage = vintage,
                    vars = varlist,
                    region = region,
                    time = time,
                    key = Sys.getenv("CENSUS_API_KEY"))

  return(data)
}


readxl_online <- function(url, type = NULL, ...) {
  test <- stringr::str_detect(url, "[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions for xls use look forward, xls not
  # followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
    httr::GET(url, write_disk("tmp.zip", overwrite = TRUE))
    tmp <- unzip("tmp.zip")
    # On osx more than one file name is returned, select first element.
    df <- readxl::read_excel(tmp[[1]],...)
    return(df)
  }
  if (!is.null(type)) {
    type = type

  }
  df <- httr::GET(url, write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type),...)

}
