## code to prepare `pl2fips.rda` dataset goes here

# this is a container for result from SifanLiu::add2FIPS geocoding
# manually update the dataset after each geocoding task, and save to the local project folder

# update the package data here
load("data/pl2fips.rda")

usethis::use_data(pl2fips, overwrite = T)
