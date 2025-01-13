# note to self: zittende versus nieuwe bewoners tweedeling_sd
# denk aan Oostzanerwerf NA

library(tidyverse)
library(openxlsx)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

## stap 1. inlezen BBGA ---

#pad <- "G:/OIS/Basisbestanden/basisstatistiekbestand/NU op de OIS-site/onderzoek.amsterdam.nl/"

pad <- "../OS 24 BBGA UPDATE 10 2024/data/04 0 publicatie BBGA bestanden/DEF okt 24"

# inlezen BBGA
BBGA_data <- read.csv2(glue::glue("{pad}/bbga_latest_and_greatest.csv")) |>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele)) |>
  rename(
    spatial_code  = gebiedcode15,
    temporal_date = jaar,
    value = waarde)|>
  add_column(
    spatial_date = '20220324') |>
  mutate(spatial_code = replace_na(spatial_code , "NA"))

# inlezen META data
BBGA_meta <- read.csv(glue::glue("{pad}/metadata_latest_and_greatest.csv"))|>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele))|>
  select(variabele, thema, label) |>
  set_names(c("variabele", "thema_bbga", "label_bbga"))


### stap 2 inlezen indicatoren ---

# inlezen basislijst met indicatoren
tabel_ind <- read.xlsx("01 indicatoren/Totaaloverzicht focusgebieden Amsterdam INPUT.xlsx") |>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele))|>
  select(-c(bronhouder, opmerking))
  

# selecteer indicatoren uit basislijst die ook in BBGA staan
tabel_ind_bbga <- tabel_ind |>
  filter(bbga == TRUE)

### stap 3 koppelen BBGA-indicatoren aan BBGA-data ---

BBGA_data_def <- BBGA_data |>
  
  # koppel basislijst (input) aan lijst met alle BBGA-indicatoren
  left_join(tabel_ind_bbga, by = "variabele")|>
  
  # verwijder de BBGA indicatoren die niet in basislijst (input) staan 
  filter(aanpak_noord == TRUE | mpzo == TRUE | nplv == TRUE | basis == TRUE | samen_nw == TRUE)




# koppel de meta-data aan bestand en voeg een kolom tweedeling_def toe 
data_wel_bbga <- BBGA_data_def |>
  left_join(BBGA_meta, "variabele")|>
  mutate(temporal_date = as.character(temporal_date)) |>
  add_column(tweedeling_def = 'totaal')

### stap 5 toevoegen data die niet in BBGA staat ---

# van sommige vars staat het totaal in BBGA en de tweedeling in de map 'niet-bbga'
# gezondheidsindicatoren zijn handmatig voor wijken en buurten aangevuld
#   nb wordt niet meer aangevuld door GGD !!!
# veiligheidsindexcijfers staan in BBGA maar niet op stadsdeelniveau
# in data_bijstand stond tweedeling_sd : dit is handmatig aangepast naar tweedeling
# minimahuishoudens staan tot 2021 in BBGA (handmatig toegevoegd tot 2022)

uitzondering <- c(
  "sruit4_p", "pinzetbrt_p", "pinform_p", "wzbeweeg_p", "wzdepr_p" , "wzzwaar_p", "wzgezond_p",
  "v_onvbeleving_i","v_personovl_i","v_vermijd_i","v_verloed_i","v_slacht_i","v_gercrim_i",
  "iminjong130_p", "iminhh130_p"
  )

tabel_ind_niet_bbga <- tabel_ind |>
  filter(bbga == FALSE | variabele %in% uitzondering)

# inlezen data die niet in BBGA staat
temp <- list.files("00 ruwe data/niet in bbga",full.names = T)


my_mutate <- function(x){
  
  vars <- c("measure", "spatial_type", "spatial_code", 
            "temporal_date", "spatial_date", 
            "value", "tweedeling", "tweedeling_sd")
  
  x |>
    mutate(across(everything(), as.character))|>
    select(any_of(vars)) |>
    rename(variabele = measure)|>
    mutate(
      variabele = str_to_lower(variabele))
}


# omzetten van NA's naar code Oostzaan (Amsterdam Noord)
my_replace_na <-  function(x) {
  
  if ('spatial_code' %in% c(names(x))) {
    
    x |>
      
      mutate(
        spatial_code = case_when(
          spatial_type %in% c('wijk', 'Wijk', 'wijken', 'Wijken') & is.na(spatial_code) ~ 'NA',
          TRUE                                                                          ~ spatial_code)
      )  
    
  } else if ('spatial_name' %in% c(names(x))) {
    
    x |>
      
      mutate(
        spatial_code = case_when(
          spatial_name %in% c('Oostzanerwerf', 'oostzanerwerf') & is.na(spatial_code) ~ 'NA',
          TRUE                                                                        ~ spatial_code)
      )  
    
    
    
    
    
  }
  
  

  
  
  
  
  
  } 

data_niet_bbga <- temp |>
  map(\(x) read.xlsx(x))|>
  map(\(x) my_mutate(x))|>
  map(\(x) left_join(x, tabel_ind_niet_bbga, by = "variabele"))|>
  map_df(\(x) filter(x, !is.na(bbga)))|>
  my_replace_na() |>
  mutate(tweedeling_def = case_when(
    
    # toevoegen totaal aan de tabellen zonder tweedeling
    is.na(tweedeling) ~ "totaal",
    
    # vervangen foute indelingen naar juiste indeling 
    str_detect(tweedeling, pattern = "zittend") ~ "zittende bewoner",
    str_detect(tweedeling, pattern = "lang")    ~ "zittende bewoner",
    str_detect(tweedeling, pattern = "nieuw")   ~ "nieuwe bewoner",
    str_detect(tweedeling, pattern = "kort")    ~ "nieuwe bewoner",
    TRUE                                        ~ tweedeling)
  )

# toevoegen juiste gebiedsindeling -
source("02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R")

# koppelen data bbga en data niet bbga
data_def <- bind_rows(data_niet_bbga, data_wel_bbga) |>
  select(-(c("spatial_type", "spatial_date")))|>
  
  mutate(
    spatial_code = case_when(
      spatial_code %in% c("STAD", "0363", '363', "[code]") ~ '0363',
      TRUE ~ spatial_code)
  )|>
  
  # toevoegen juiste gebiedsindelingen
  left_join(
    geo_df, by ="spatial_code") |>
  
  #NB: thema_zuidoost_label (nieuwe indeling) vervangt thema_zuidoost (oude indeling)
  select(all_of(c(
    
    "indicator_sd", "variabele" ,"value", "temporal_date",   
    "spatial_code","spatial_name","spatial_type", "tweedeling_def", 
    
    # zuidoost_label vervangt thema_zuidoost
    "thema_zuidoost_label",  
   
     # noord_eenmeting vervangt thema_noord
    "thema_noord_eenmeting",
    "thema_nw_kleur","thema_nw_label",
    "thema_bbga","label_bbga", "thema_nplv",  
    
    # booleans
    "mpzo","aanpak_noord", "samen_nw","nplv","bbga", "basis",
    
    #kerninidcator booleans
    "kernindicator_zo", "kernindicator_noord", "kernindicator_nw",
    
    # datum van de gebiedsindeling
    "spatial_date")))|>
  
  # alleen jaartal meenemen
  mutate(
    temporal_date = str_sub(temporal_date, 1, 4),
    value=as.numeric(value)) |>
  
  filter(
    !is.na(spatial_name),
    temporal_date %in% c(2017:2024))

### in dit script worden specieke berekeningen gedaan ---
source("02 scripts/01 scripts bewerking data/script 01 basis extra berekeningen.R")




# extra komt uit script 'extra berekeningen'
data_def2 <- bind_rows(data_def, extra) |>
  
  mutate(
    spatial_type = factor(
      spatial_type, 
      levels = c("winkelgebieden", "buurten", 'wijken', 'gebieden', 'stadsdelen', 'gemeente')),

    tweedeling_def = factor(
      tweedeling_def, levels = c("zittende bewoner", "nieuwe bewoner", "totaal"))
    )|>
  
  group_by(variabele)|>
  
  arrange(temporal_date)|>
  mutate(besch_jaren = paste(unique(temporal_date), collapse = "|"))|>
  
  arrange(spatial_type)|> 
  mutate(besch_aggr_niveaus = paste(unique(spatial_type), collapse = "|"))|>

  arrange(tweedeling_def)|> 
  mutate(besch_tweedeling = paste(unique(tweedeling_def), collapse = "|"))

######################################################################################################
### data_def2 ### wordt gebruikt voor de verwerking naar afzonderlijke datasets voor Zuidoost, Noord en NW
######################################################################################################

