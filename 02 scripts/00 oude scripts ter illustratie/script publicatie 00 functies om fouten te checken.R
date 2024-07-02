library(tidyverse)
library(openxlsx)

### stap 0. inlezen wijkcodes

gebieden15<- read.xlsx("02 formats/koppeltabel_geb15.xlsx")
gebieden22<- read.xlsx("02 formats/koppeltabel_geb22.xlsx")


# aanpassen naamgeving koppelbestand in lijn met bbga -  aangepast op 25 januari 2024
gebieden15 <- gebieden15 %>% 
  rename(spatial_code=gebied_code_15,
         spatial_name=gebied_naam_15)
gebieden22 <- gebieden22 %>% 
  rename(spatial_code=gebied_code_22,
         spatial_name=gebied_naam_22)


### stap 1. checks --

# om alle sheets in een excel in te lezen
path <- readxl_example("datasets.xls")
lapply(excel_sheets(path), read_excel, path = path)

#check op dubbelen-
functie_duplicates <- function (x) {
  
  y<- x %>%
    
    select( measure,
            spatial_code,
            gebied_niveau,
            spatial_date,
            temporal_date )%>%
    
    filter(duplicated(.))
  
  if (nrow(y) == 0){
    
    print ("er zijn geen duplicates")
    
  } else {
    
    print ("waarschuwing, er zijn duplicates")
    
    return(y)
  }
  
  
}


#check op gebiedcodes -
functie_gebiedcode <- function(x) {
  
    if(first(x[["temporal_date"]]) %in% c('20230101', '20230401' , '20220324' ))  { # enige optie is hier eigenlijk 20220324
    
    y<- x %>%
      
      filter(!spatial_code  %in% gebieden22$gebied_code_22)
    
  } 
  
  if (nrow(y) == 0){
    
    print ("alle gebiedscodes kloppen")
    
  } else {
    
    print ("waarschuwing, er staan onbekende gebiednamen in!")
    
    return(y) } 
  
  }

# check op variabelnamen -
functie_varcheck   <- function(data, meta) {
  
  
  vars_meta <- meta[["measure"]]
  
  vars_data <- unique(data[["measure"]])
  
  y<-setdiff(vars_data, vars_meta )

  
  
  if (length(y) == 0 ){
    
    print ("variabelnamen komen overeen")
    
  } else  {
    
    print ("waarschuwing, niet alle variabelen zitten in de metafile")
    
    } 
  
  return(y)

}

# verzamelmandje van checks
functie_check      <- function(data, meta) {
  

   a <- functie_duplicates(data)
   c <- functie_spaticalcodes(data)
   d <- functie_measurecheck(data, meta)

   list <- list(a, c, d )
   names(list) <- c("duplicates", "spatial codes", "measures")
   
   return(list)
  
}




