# Webscraper for immobilienscout24.de
#
# Patrick Bäurer
# 2020-05-02
#

library(tidyverse)
library(magrittr)


# extract ids from webpage ----
extract_ids <- function(webpage){
  
  SearchString <- 'href=\"/expose/'
  FilteredWebpage <- webpage[str_detect(webpage, SearchString)]
  locations <- FilteredWebpage %>% str_locate_all(SearchString)
  
  id_list <- character()
  for(i in seq_len(length(locations))){
    for(j in seq_len(nrow(locations[[i]]))){
      string <- str_sub(FilteredWebpage[i], locations[[i]][j, 1] + 14, locations[[i]][j, 1] + 30)
      id_list <- c(id_list, string %>% str_sub(1, string %>% str_locate('\\" ') %>% extract(1,1) - 1))
    }
  }
  id_list %<>% unique()
  
  return(id_list)
  
}

BasicURL <- "https://www.immobilienscout24.de/Suche/de/baden-wuerttemberg/freiburg-im-breisgau/wohnung-kaufen?pagenumber="
id_list_all <- character()

for(i in 1:100){
  
  PageNumber <- i
  print(PageNumber)
  Webpage <- BasicURL %>% paste0(PageNumber) %>% readLines()
  ids <- extract_ids(Webpage)
  if(length(ids) == 0 | PageNumber > 100){
    break()
  }
  id_list_all <- c(id_list_all, ids)
  
}

id_list_all %<>% unique()
length(id_list_all)




# Get data from exposes ----
extract_KeyValues <- function(webpage){
  
  SearchString <- "var keyValues "
  KeyValueString <- webpage[webpage %>% str_detect(SearchString)][1]
  KeyValueString_Location <- KeyValueString %>% str_locate("= \\{") %>% extract(1, 1)
  KeyValueString_Split <- KeyValueString %>% str_sub(KeyValueString_Location + 3, -3) %>% str_split("\\,")
  KeyValueString_Split2 <- KeyValueString_Split[[1]] %>% str_split('\\"[:]\\"')
  KeyValue_Tibble <- tibble(Key = character(), Value = character())
  
  for(i in seq_len(length(KeyValueString_Split2))){
    
    # i <- 1
    string <- KeyValueString_Split2[[i]]
    if(!(length(string) == 2)){
      next()
    }
    
    add_tibble <- tibble(Key = string[1] %>% str_sub(2),
                         Value = string[2] %>% str_sub(1, -2))
    KeyValue_Tibble %<>% bind_rows(add_tibble)
    
  }
  
  return(KeyValue_Tibble)
}


ExposeURL <- "https://www.immobilienscout24.de/expose/"
KeyValue_Tibble_All <- tibble(Id = character(),
                              Key = character(),
                              Value = character())

for(i in seq_len(length(id_list_all))){
  
  print(paste0(i, "/", length(id_list_all)))
  
  webpage <- ExposeURL %>% paste0(id_list_all[i]) %>% readLines()
  
  KeyValue_Tibble <- extract_KeyValues(webpage) %>%
    mutate(Id = id_list_all[i])
  
  KeyValue_Tibble_All %<>% bind_rows(KeyValue_Tibble)
  
}

relevant_features <- c("obj_assistedLiving",
                       "obj_balcony", "obj_barrierFree", "obj_cellar", "obj_condition", "obj_courtage",
                       "obj_energyType", "obj_ExclusiveExpose", "obj_firingTypes", "obj_garden", "obj_hasKitchen",
                       "obj_heatingType", "obj_immotype", "obj_interiorQual", "obj_international", "obj_lift",
                       "obj_livingSpace", "obj_newlyConst", "obj_noParkSpaces", "obj_noRooms", "obj_parkingSpace",
                       "obj_picturecount", "obj_purchasePrice", "obj_regio1", "obj_regio2", "obj_regio3",
                       "obj_rented", "obj_thermalChar", "obj_typeOfFlat", "obj_yearConstructed", "obj_zipCode",
                       "obj_floor", "obj_nbp", "obj_numberOfFloors")

KeyValue_Tibble_All %<>% filter(Key %in% relevant_features)
Dataset_Raw <- KeyValue_Tibble_All %>% pivot_wider(id_cols = Id, names_from = Key, values_from = Value)


numerical_features <- c("obj_livingSpace", "obj_noParkSpaces", "obj_noRooms", "obj_picturecount", "obj_purchasePrice",
                        "obj_thermalChar", "obj_yearConstructed", "obj_floor", "obj_numberOfFloors")
for(i in seq_len(length(numerical_features))){
  
  # i <- 1
  feature <- numerical_features[i]
  Dataset_Raw %<>% mutate(UQ(rlang::sym(feature)) := as.numeric(UQ(rlang::sym(feature))))
  
}

ImmoData <- Dataset_Raw %>%
  mutate(Timestamp = Sys.time(),
         Link = paste0(ExposeURL, Id))


# Attach new ImmoData ----
ImmoData_old <- read_rds(paste0(getwd(), "/Datasets/ImmoScout/ImmoData.rds"))
ImmoData_New <- ImmoData_old %>%
  bind_rows(ImmoData)
write_rds(ImmoData_New, paste0(getwd(), "/Datasets/ImmoScout/ImmoData.rds"))



