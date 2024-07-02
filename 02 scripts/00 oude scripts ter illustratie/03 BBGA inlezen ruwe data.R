
library(tidyverse)
library(openxlsx)
library(readxl)

source("02 0 scripts/01 BBGA foutencheck functies.R")

# check kolomnamen

# check Amsterdam 0363 ipv 363 en 'gemeente ipv gemeenten'
# check data Weesp voor 2023 (moet leeg zijn)
# check spatial_data : '20220324'
# check aanlevering csv : moet met puntkomma zijn, niet komma
# check spatial_type : enkelvoud of meervoud ; gebied GGW-Gebieden
# check wel of niet aanleveren met spatial_name? antw: NEE
# check lengte variabelnaam groen antw: aangepast
# check spatial type met hoofdletter; temporal_type met kleine letter 
# ook GGW-gebied ;  Postcode 4 gebied ; Alternatief gebied

kolomnamen <- c("spatial_code", "spatial_type", "spatial_date",
                "temporal_date", "temporal_type", "measure", "value") 


#### inlezen ruwe data ---

data<- list()

## data detailhandel: Rogier --
data$detailhandel <- read.csv2(
  "01 1 ruwe data aangeleverd/detailhandel/20240101_detailhandel_def_cijfers.csv")

### data opleiding: Sjors --
data$opleiding <- read_excel(
  "01 1 ruwe data aangeleverd/20240111_bbga_opleidingsniveau.xlsx")|>
  mutate(spatial_date ='20220324')

### data neet: ook Sjors --
data$neet<- read_excel(
  "01 1 ruwe data aangeleverd/20240215_neet_bbga.xlsx")

### data SvdS/GGD: Ellen --
data$svds <- read.csv2(
  "01 1 ruwe data aangeleverd/Staat vd Stad cijfers/20240101_svds_def_cijfers.csv")

### data groen: Nico en Marleen --
data$groen <- read.csv2(
  "01 1 ruwe data aangeleverd/Groen/20240101_groen_def_cijfers.csv")

### data bevolking: Aafke
data$bev <- read.csv2(
  "01 1 ruwe data aangeleverd/bevolking24tall.csv")

### data hotel en airbnb: Bart
data$hotel <- read.csv2(
  "01 1 ruwe data aangeleverd/20240101_hotel_def_cijfers.csv")

### data locatus: Zakaria
data$locatus <- read.csv2(
  "01 1 ruwe data aangeleverd/statistieken_locatus.csv")

### data lisa vestigingen: ook Zakaria
data$vestigingen <- read.csv2(
  "01 1 ruwe data aangeleverd/statistieken_vestigingen.csv")

### data prognose: Annika
data$prognose <- read.csv2(
  "01 1 ruwe data aangeleverd/20240101_prognose_def_cijfers.csv")

### data parkeren: Merijn
data$parkeerplaatsen <- read.csv2(
  "01 1 ruwe data aangeleverd/20240101_parkeerplaatsen_cijfers.csv")|>

  mutate(spatial_date=case_when(
    spatial_type == 'Winkelgebied' ~ '20240101',
    spatial_type != 'Winkelgebied' ~ '20220324')
    )

### data burgermonitor: Sara R.
data$vertrouwen <- read_excel(
  "01 1 ruwe data aangeleverd/ABM2021tbvBBGA.xlsx")|>
  mutate(spatial_date = '20220324')|>
  mutate(spatial_code = str_replace(spatial_code, "363", "0363"))

### data veiligheid: Anna en Sara
data$veiligheid <- read.csv2(
  "01 1 ruwe data aangeleverd/VMbbga23.csv")

colnames(data$veiligheid)[1] <- "spatial_code"

### data inkomen: Renske
data$inkomen_armoede  <- read.csv("01 1 ruwe data aangeleverd/20240216_Inkomen_Armoede_cijfers.csv")
data$inkomen_giniink  <- read.csv2("01 1 ruwe data aangeleverd/20240216_Inkomen_giniinkomen_cijfers.csv")
data$inkomen_gineverm <- read.csv2("01 1 ruwe data aangeleverd/20240216_Inkomen_ginivermogen_cijfers.csv")


### data arbeid: Feiko (nieuwe dataset geleverd in feb 2024)
data$arbeid <- read.csv2(
  "01 1 ruwe data aangeleverd/20240227_arbeidsmarkt_cijfers.csv")|>
  mutate(spatial_code = str_replace(spatial_code, "363", "0363"))

### data onderwijs:
data$onderwijs <- read.csv2(
  "01 1 ruwe data aangeleverd/20240101_onderwijs_def_cijfers.csv") |>
  mutate(spatial_code=replace_na(spatial_code, "NA"))

### data SES: Mersiha  aanpassing 	
# SK2665_SES234  in 	SK2665_SES234

data$ses <- bind_rows (
  
  read.csv2(
    "01 1 ruwe data aangeleverd/20210101_kwetsbaarheid_ses_secm_cijfers_def.csv"), 
  
  read.csv2(
    "01 1 ruwe data aangeleverd/20220101_ses_secm_cijfers_def.csv")
    
  )|>
  mutate(spatial_code = str_replace(spatial_code, "363", "0363"))


colnames(data$ses)[1] <- "spatial_code"

count(data$ses, measure)
##############
### CHECKS ---
##############



# check kolomnamen
data |>map(\(x) names(x))

# nb: sommige dataframes hebben een x-kolom, die moet weg
data <- data |>
  map(\(x) select(x, all_of(kolomnamen)))|>
  map(\(x) mutate(x, spatial_code  = replace_na  (spatial_code, 'NA')))|>
  map(\(x) mutate(x, spatial_type  = str_to_title(spatial_type)))|>
  map(\(x) mutate(x, temporal_type = str_to_lower(temporal_type)))|>
  map(\(x) mutate(x, measure = str_to_upper(measure)))|>
  map(\(x) mutate(x, spatial_type = str_replace(spatial_type, "Ggw-Gebied", "GGW-gebied")))|>
  map(\(x) mutate(x, spatial_type = str_replace(spatial_type, "Ggwgebied" , "GGW-gebied")))|>
  map(\(x) mutate(x, spatial_type = str_replace(spatial_type, "Postcode 4 Gebied" , "Postcode 4 gebied")))|>
  map(\(x) mutate(x, spatial_type = str_replace(spatial_type, "Alternatief Gebied" , "Alternatief gebied")))


# vastgoed ?
# data renske: % kinderen bijstand is niet gelukt, volgende update
# data Feiko: woonindex
# SES scores: Mersiha


