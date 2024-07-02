

library(tidyverse)
library(openxlsx)
library(readxl)  #nb: readxl leest alle spaties in in de kolomnamen


kolomnamen <- c("spatial_code", "spatial_type", "spatial_date",
                "temporal_date", "temporal_type", "measure", "value") 

### opschonen bestanden

##################
# detailhandel ---
##################

data_detailhandel <- bind_rows(
  
  read.csv2("01 1 ruwe data aangeleverd/detailhandel/20240101_koopkrachtbinding_cijfers.csv"),
  read.csv2("01 1 ruwe data aangeleverd/detailhandel/20240101_rapwingeb_cijfers.csv"),
  read.csv2("01 1 ruwe data aangeleverd/detailhandel/20240101_winkelhuren_cijfers.csv")|>
    mutate(spatial_type = "winkelgebieden"))|>
  
  select(all_of(kolomnamen)) |>
  
  mutate(spatial_code=case_when(
    (spatial_code==363 | spatial_code =='363') ~ '0363',
    TRUE ~ spatial_code))|>
  
  mutate(spatial_type=case_when(
    spatial_type == 'gebieden'   ~ 'ggw-gebied',
    spatial_type == 'stadsdelen' ~ 'stadsdeel',
    spatial_type == 'winkelgebieden' ~ 'winkelgebied',
    TRUE ~ spatial_type)) |>
  
  mutate(measure=str_replace(measure, "E", "BH"))




write.csv2(data_detailhandel, "01 1 ruwe data aangeleverd/detailhandel/20240101_detailhandel_def_cijfers.csv")
  


#############
# data SvdS -
#############

data_svds <- bind_rows(
  
  read_xlsx("01 1 ruwe data aangeleverd/Staat vd Stad cijfers/gegevens BBGA Staat vd Stad 2022_NW.xlsx"),
  read_xlsx("01 1 ruwe data aangeleverd/Staat vd Stad cijfers/GGD-cijfers 18+ 2022 voor BBGA.xlsx"),
  read_xlsx("01 1 ruwe data aangeleverd/Staat vd Stad cijfers/GGD-cijfers 65+ 2022 voor BBGA.xlsx")
  )|>
  
  mutate(
    spatial_code  = str_replace(spatial_code, "363", "0363"),
    temporal_date = '20220101',
    temporal_type = 'jaar')
  
write.csv2(data_svds, "01 1 ruwe data aangeleverd/Staat vd Stad cijfers/20240101_svds_def_cijfers.csv")

#############
# data groen
#############

data_groen<- bind_rows(
  
  # marleen
  read.csv2("01 1 ruwe data aangeleverd/Groen/20240123_afstandgroen_cijfers.csv"),
  
  # nico
  read.csv2("01 1 ruwe data aangeleverd/Groen/bbga_groen_20230101.csv")
  
  # opmerkingen nico:
  #  - toevoegen nieuwe variabelen groen thema openbare ruimte
  #  - verwijderen oude variabelen openbare ruimte, zie metadata
  #  - actualiseren oppervlakte gebieden, oppervlakte land en oppervlakte water.
  
) 


write.csv2(data_groen, "01 1 ruwe data aangeleverd/Groen/20240101_groen_def_cijfers.csv")

#############
# data hotels bart
#############


data_hotels <- read_xlsx("01 1 ruwe data aangeleverd/Hotels BBGA.xlsx") |>
  
  select(-BHHOTBED_1000INW) |>
  
  pivot_longer(cols = c(BHHOT, BHHOTKAM, BHHOTBED), names_to = 'measure', values_to = 'value')


data_hotels_def <-  bind_rows(
  
  y <- data_hotels|>
    add_column(
    
      spatial_type = 'wijk',
      spatial_date = '20220324',
      temporal_date = glue::glue("{data_hotels$Jaar}0101"),
      temporal_type = 'peildatum') |>
  
    rename(spatial_code = Code) |>
    select(all_of(kolomnamen)),
  
  
  y |>
    
    group_by(spatial_date,
             temporal_date, temporal_type, measure) |>
    summarise(value= sum(value)) |>
    
    add_column(spatial_type = 'gemeente',
               spatial_code ='0363')
  )
  

  

data_airbnb <- read_xlsx("01 1 ruwe data aangeleverd/Airbnb BBGA.xlsx")

data_airbnb_def <- data_airbnb |>
  
  mutate(
    Code            = str_replace(Code, "STAD" , "0363" ),
    gebiedsindeling = str_replace(gebiedsindeling, "stad" , "gemeente" ))|>
  
  rename(
    spatial_code = Code,
    spatial_type = gebiedsindeling,
    value        = BHVESTAIRBNB ) |>
  
  add_column(
    spatial_date = '20220324',
    temporal_date = glue::glue("{data_airbnb$Jaar}0101"),
    temporal_type = 'peildatum',
    measure = 'BHVESTAIRBNB') |>
  select(all_of(kolomnamen)) |>
  
  
  mutate(spatial_type=case_when(
    spatial_type == 'wijken'   ~ 'wijk',
    spatial_type == 'stadsdelen' ~ 'stadsdeel',
    TRUE ~ spatial_type))

             

write.csv2(bind_rows(
  data_airbnb_def, data_hotels_def), "01 1 ruwe data aangeleverd/20240101_hotel_def_cijfers.csv")

### Prognose Annika ---

prognose <- read.csv2("01 1 ruwe data aangeleverd/bbgaprog_24.csv")|>
  mutate(gebiedcode22=replace_na(gebiedcode22, "NA"))


prognose_def<- prognose |>
  set_names(c("spatial_code", "temporal_date", "value", "measure"))|>
  add_column(spatial_date  = '20220324',
             temporal_type = 'peildatum')|>
  
  mutate(spatial_code=str_replace(spatial_code, "STAD", "0363"))|>
  
  mutate(spatial_type=case_when(
    
    spatial_code %in% geo_check$wijk[["spatial_code"]]      ~ 'wijk',
    spatial_code %in% geo_check$stadsdeel[["spatial_code"]] ~ 'stadsdeel',
    spatial_code %in% geo_check$buurt[["spatial_code"]]     ~ 'buurt',
    spatial_code %in% geo_check$gebied[["spatial_code"]]    ~ 'gebied',
    spatial_code == '0363' ~ 'gemeente',
    spatial_code == 'Z'    ~ 'stadsdeel',
    spatial_code == 'ZZ'   ~ 'wijk',
    spatial_code == 'ZZ99' ~ 'buurt',
    spatial_code == 'ZX99' ~ 'gebied')
    )


write.csv2(prognose_def, "01 1 ruwe data aangeleverd/20240101_prognose_def_cijfers.csv")


### onderwijs shy ann ---



my_attr<- function (x){
  
  x |>
    mutate(
      spatial_code  = as.character(spatial_code),
      spatial_date  = as.character(spatial_date),
      temporal_date = as.character(temporal_date),
      temporal_type = as.character(temporal_type),
      measure       = as.character(measure),
      value         = as.double(value)
      )
  
}

pad<- "01 1 ruwe data aangeleverd/Onderwijs/"

onderwijs1<- list(
  
  read.csv2(file=glue::glue("{pad}/22_23_onderwijs_leerlingen_VO_cijfers.csv")),
  read.csv2(file=glue::glue("{pad}/22_23_onderwijs_streefniveaus_cijfers.csv")),
  read.csv2(file=glue::glue("{pad}/23_24_onderwijs_leerlingen_PO_cijfers.csv")),
  read.csv2(file=glue::glue("{pad}/2017_2022_onderwijs_leerlingen_PO_cijfers_VSO_SO.csv"))
) |>
  map_df(\(x) my_attr(x))
  # mutate(spatial_date = "20220324")
                  
  
  
onderwijs2<- list(
  
  read.table(file=glue::glue("{pad}/OVVE_20181201BBGA.csv"), sep = "\t", header=TRUE),
  read.table(file=glue::glue("{pad}/OVVE_20191201BBGA.csv"), sep = "\t", header=TRUE), 
  read.table(file=glue::glue("{pad}/OVVE_20201201BBGA.csv"), sep = "\t", header=TRUE),
  read.table(file=glue::glue("{pad}/OVVE_20211201BBGA.csv"), sep = "\t", header=TRUE),
  read.table(file=glue::glue("{pad}/OVVE_20231201BBGA.csv"), sep = "\t", header=TRUE)
) |>
  map(\(x) mutate(x, value= str_replace(value, ",", ".")))|>
  map(\(x) my_attr(x))
  



temp.xlsx = list.files(path = pad, pattern ="*.xlsx", full.names = T)

onderwijs3 <-  map(temp.xlsx, read.xlsx)|>
  map_df(\(x) my_attr(x))

onderwijs <- bind_rows(onderwijs1, onderwijs2, onderwijs3) |>
  
  mutate(spatial_code  = case_when( 
    spatial_code == "363" ~ "0363", 
    TRUE ~ spatial_code)
  )  |>
  
  mutate(
    spatial_code = replace_na(spatial_code, "NA"),
    spatial_type = replace_na(spatial_type, "Buurt")
  )


#test1<- onderwijs|>my_attr()|>functie_duplicates()




write.csv2(onderwijs, "01 1 ruwe data aangeleverd/20240101_onderwijs_def_cijfers.csv")
  
  





