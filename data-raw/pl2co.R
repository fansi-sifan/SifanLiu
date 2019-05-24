# XWALKS: place2county ==========================

# further clean the place names
library(dplyr)
place2county <- metro.data::place2county

pl2co <- place2county  %>%
  mutate(pl_label = gsub(" city| town| CDP| village| municipality| borough", "", pl_name),
         pl_label = tolower(gsub("\\,.+", "", pl_label)),
         pl_label = ifelse(grepl("(balance)",pl_label),
                           gsub(" \\(balance\\)","",gsub(" \\w.+|\\-\\w.+","",pl_label)),
                           pl_label),
         st_name = state.name[match(st_ab,state.abb)])

usethis::use_data(pl2co)
