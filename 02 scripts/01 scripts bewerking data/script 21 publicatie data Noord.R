
library(tidyverse)
library(sf)
library(openxlsx)


source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


#inlezen data: let op: 

# NM01 (Blauwe Zand) wordt toegevoegd aan NH (Tuindorp Buiksloot)

data_noord_ruw <- read_rds("03 tussentijds/BBGA_data_noord.rds")|>
  filter(
    (variabele  != 'orpubgroen_p' | temporal_date != '2021')
    )|>
  mutate(
    spatial_code = str_replace_all(spatial_code, 'NM01', 'NH99'),
    ) 

# filter vars uit basis die er wel toe doen
basis_vars <- c(
  "bev0_18_p",  "beveenouderhh_p",  "bevtotaal_groei","wvoorrbag_groei",  'bhvest_groei', 'bhwp_1000inw',
  "bevhhmkind_p","bev66plus_p","bevnstedeling_p","wcorhuur_p", 'bhzzp_p',
  "iwwb_p",  "ihhink_gem",  "wcorhuur_p")

# filter out alle vars met absolute waardes
abs_vars <- c(
  "bevhuishoudenhh","bevtotaal","bhstart","bhvest", "bhvest_1wp",
  "bhlocoppleegstand","bhlocvkpleegstand", "opp_boomkroon_publiek_ha",
  "bhzzp","orland","iwwb","orpubgroen")  

# nieuwe opgeschoonde dataset
data_noord <- bind_rows (
  
  # verplaatsen enkele variabelen naar andere indicatoren
  data_noord_ruw |>
    filter(thema_noord == 'basis')|>
    filter(variabele %in% basis_vars)|>
    mutate(
      thema_noord = case_when(
        variabele %in% c("iwwb_p", "ihhink_gem") ~ 'Inkomen',
        variabele == 'wcorhuur_p' ~ 'Wonen',
        TRUE ~ thema_noord)
        ),
  
  # overige variabelen
  data_noord_ruw |>
    filter(thema_noord != 'basis')
  )|>
  
  # selectie met alleen aanpak noordbuurten en wijken
  filter(str_detect(besch_aggr_niveaus, "wijken"))|>
  filter(aanpak_noord_buurt == TRUE | spatial_type != 'buurten') |>
  filter(!variabele %in% abs_vars)|>
  
  # ranking voor volgorde lijnen in figuur
  mutate(spatial_type_code = case_when(
    spatial_type == 'buurten'    ~ 4,
    spatial_type == 'wijken'     ~ 3,
    spatial_type == 'stadsdelen' ~ 2,
    spatial_type == 'gemeente'   ~ 1)
  )|>
  
  mutate(facet_buurt = case_when(
    spatial_type == 'buurten'  ~ 'buurten',
    TRUE ~  'gemeente, stadsdeel, wijk')
  )





 

# selecteer data op buurtniveau met minimaal 3 jaar 
data_noord_met_buurt_3plus<- data_noord |>
  filter(nchar(besch_jaren) > 10)
  
# selecteer data op buurtniveau met 1 of 2 jaar
data_noord_met_buurt_3min<- data_noord |>
  filter(nchar(besch_jaren) <= 10)

# vector met thema's van noord
loop_var_noord3p   <- unique(data_noord_met_buurt_3plus$variabele)
loop_var_noord3m   <- unique(data_noord_met_buurt_3min$variabele)      

source("02 scripts/01 scripts bewerking data/script 21 publicatie data Noord functies.R")

###########################################
#### dit is een functie om plots te maken---
############################################

my_rdata_plot<- function(wc) {
  
  # Dit is een list met lijnfiguur per variabele
  plot_list <- loop_var_noord3p |>
    map(\(y) my_line_plot(x = data_noord_met_buurt_3plus, var = y, wijkcode = wc))|>
    set_names(loop_var_noord3p)
  
  # Dit is een list met staaf-figuur per variabele
  bar_list <- loop_var_noord3m |>
    map(\(y) my_bar_plot(x = data_noord_met_buurt_3min, var = y, wijkcode = wc))|>
    set_names(loop_var_noord3m)

  # Dit is een illustratief kaartje
  kaart <- my_plot(wc)
  
  # selectie van vars waar een thematische kaart van gemaakt wordt
  kaart_vars <- c(
    "wlabelefg_p",
    "lbuurt_r",
    "wonderhoudwoning_r",
    "iminhh130_p",
    "wzgezond_p",
    "sk017_kwets34_p",
    "skkwets34_p",
    "orpubgroen_inw",
    "wvoorrbag_groei")
  
  # Dit zijn meerdere thematische kaarten
  kaart_thema <- kaart_vars |>
    map(\(y) my_plot_ind(x = data_noord, y, wc))|>
    set_names(kaart_vars)

  # sla alle objecten in een rds-file 
  write_rds(list(
      
    
    plot_list   = plot_list, 
    bar_list    = bar_list, 
    kaart       = kaart,
    kaart_thema = kaart_thema), 
    
    file = glue::glue("03 tussentijds/plot_list_{wc}.rds"))
  
}


wijkcodes <- c("NA", "NC", "NE", "NJ", "NH", "NK", "NL", "NN")

my_rdata_plot(wc='NA')  
my_rdata_plot(wc='NC') 
my_rdata_plot(wc='NE')
my_rdata_plot(wc='NJ')
my_rdata_plot(wc='NH')  # inclusief Blauwe Zand NM (bestaat uit een buurt)
my_rdata_plot(wc='NK')
my_rdata_plot(wc='NL')
my_rdata_plot(wc='NN')


     
