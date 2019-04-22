# Author: Sifan Liu
# Date: Mon Apr 22 12:02:06 2019
# --------------
pkgs <- c('tidyverse',"tidycensus", "readxl","httr")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  }

# get latest 5 year population estimates from acs
Sys.getenv("CENSUS_API_KEY")
get_state_pop <- get_acs("state",variables = "B01003_001")
get_county_pop <- map_df(fips_codes$state_code, function(x){
  get_acs(geography = "county", variables = "B01003_001",state = x)
})


# download msa delination files from census ------------------------------
# use the latest updates
url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"
# download and read xls file
msa2county <- readxl_online(url,skip=2)
county <- totalpop %>% left_join(msa2county %>%
                                   mutate(GEOID = paste0(padz(`FIPS State Code`,2),
                                                        padz(`FIPS County Code`,3))),
                                 by = "GEOID")

# create master relation file: county_msa_state ---------------------------
county_cbsa_st <- county %>%
  # create msa typopogy
  mutate(type.cbsa = case_when(
    `Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area" ~ "metro",
    `Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area" ~ "micro",
    is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro",
    TRUE ~ "NA"
  ))%>%
  select(code.county = GEOID,
         name.county = NAME,
         population.county = estimate,
         code.cbsa = `CBSA Code`,
         name.cbsa = `CBSA Title`,
         type.cbsa)%>%
  # Substract state code and name from counties
  mutate(code.state = substr(code.county,1,2),
         name.state = gsub(".+\\, ","",name.county))%>%
  # Construct population sum from county
  group_by(code.cbsa)%>%
  mutate(population.cbsa = sum(population.county))%>%
  group_by(code.state)%>%
  mutate(population.state = sum(population.county))%>%
  ungroup()

metro100 <- county_cbsa_st %>%
  filter(type.cbsa=="metro") %>%
  select(code.cbsa, name.cbsa, population.cbsa) %>%
  unique()%>%
  mutate(rank.pop.cbsa = rank(desc(population.cbsa)),
         istop100.cbsa = rank.pop.cbsa <=100)

save(county_cbsa_st,file = "data/county_cbsa_st.RData")
save(metro100, file = "metro100.RData")
