# Author: Sifan Liu
# Date: Mon Apr 22 12:02:06 2019
# --------------
pkgs <- c('tidyverse',"tidycensus", "censusapi","readxl","httr")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  }

# update delinations, population and employment from census ------------------------------
# url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"
# cbp_year <- 2016
# acs_year <- 2017

key <- Sys.getenv("CENSUS_API_KEY")

update.county_cbsa_st <- function(url, cbp_year, acs_year){
  # get latest 5 year population estimates from acs

  # county_pop <- map_df(fips_codes$state_code, function(x){
  #   get_acs(geography = "county", variables = "B01003_001",state = x)
  # })
  county_pop <- getCensus(name = "acs/acs5",
                          vintage = acs_year,
                          vars = c("NAME","B01003_001E"),
                          region = "county:*",
                          key = key)%>%
    filter(state != "72")


  county_emp <- getCensus(name = "cbp",
                          vintage = cbp_year,
                          vars = c("EMP", "GEO_TTL"),
                          region = "county:*",
                          key = key)%>%
    mutate(EMP = as.numeric(EMP),
           # fix two county name changes
           county = case_when(
             GEO_TTL == "Shannon County, South Dakota" ~ "102",
             GEO_TTL == "Wade Hampton Census Area, Alaska" ~ "158",
             TRUE ~ county
           ))

  # download and read xls file
  msa2county <- readxl_online(url,skip=2)

  # merge three files
  county <- county_pop %>%
    left_join(county_emp,by = c("state","county"))%>%
   mutate(GEOID = paste0(padz(state,2),padz(county,3)))%>%
    left_join(msa2county %>%
                mutate(GEOID = paste0(padz(`FIPS State Code`,2),
                                      padz(`FIPS County Code`,3))),
              by = "GEOID")

  # create master relation file
  county_cbsa_st <- county %>%
    # create msa typopogy
    mutate(type.cbsa = case_when(
      `Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area" ~ "metro",
      `Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area" ~ "micro",
      is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro",
      TRUE ~ "NA"
    ))%>%
    select(code.county = GEOID,
           name.county = GEO_TTL,
           population.county = B01003_001E,
           employment.county = EMP,
           code.cbsa = `CBSA Code`,
           name.cbsa = `CBSA Title`,
           type.cbsa)%>%
    # Substract state code and name from counties
    mutate(code.state = substr(code.county,1,2),
           name.state = gsub(".+\\, ","",name.county))%>%
    # Construct population sum from county
    group_by(code.cbsa)%>%
    mutate(population.cbsa = sum(population.county),
           employment.cbsa = sum(employment.county))%>%
    group_by(code.state)%>%
    mutate(population.state = sum(population.county),
           employment.state = sum(employment.county))%>%
    ungroup()

  # rank cbsa by population
  metro100 <- county_cbsa_st %>%
    filter(type.cbsa=="metro") %>%
    select(code.cbsa, name.cbsa, population.cbsa) %>%
    unique()%>%
    mutate(rank.pop.cbsa = rank(desc(population.cbsa)),
           istop100.cbsa = rank.pop.cbsa <=100)

  # save output
  save(county_cbsa_st,file = "data/county_cbsa_st.RData")
  save(metro100, file = "data/metro100.RData")
}
