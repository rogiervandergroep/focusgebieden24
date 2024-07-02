
library(readxl)
library(openxlsx)


#### inlezen ruwe meta data ---

# bestaande metadata
meta_basis <- read_excel("G:/OIS/Basisbestanden/basisstatistiekbestand/database/Stuurbestanden/20240226_basis_rogier/metadata.xlsx")

# functie om variabelen naar character om te zetten
my_char_mutate <- function(x){
 
  cols <- c("Thema Kerncijfertabel", "Tussenkopje_kerncijfertabel" ,"Rekeneenheid_tekst", "GROEP")

  x |>  mutate(across(all_of(cols), as.character))
  
  
} 


# nieuw toetevoegen metadata

meta_nieuw <- bind_rows(
  
  # Detailhandel Rogier: monitor detailhandel
  read_excel("01 2 metadata aangeleverd/bbga_meta_detailhandel.xlsx")|> 
    my_char_mutate(),

  # SvdS: Ellen (een variabele, rest staat al in bbga en meta)
  read_excel("01 2 metadata aangeleverd/bbga_meta_vertrouwen_toekomst.xlsx") |>
    my_char_mutate(),
  
  # inkomen: Renske
  read_excel("01 2 metadata aangeleverd/bbga_meta_inkomen.xlsx", sheet = "meta_nieuw")|>
    my_char_mutate()|>
    mutate(Rekeneenheid=as.double(Rekeneenheid)),
  
  # werk: Feiko 
  read_excel("01 2 metadata aangeleverd/bbga_meta_werk.xlsx")|>
    my_char_mutate(),
  
  # groen: Nico en Marleen
  read_excel("01 2 metadata aangeleverd/bbga_meta_afstandgroen.xlsx")  |>
    my_char_mutate(),
  
  read_excel("01 2 metadata aangeleverd/bbga_meta_groen.xlsx", sheet = 1)|>
    my_char_mutate(),
  
  # vertrouwen van Sara R.
  read_excel("01 2 metadata aangeleverd/bbga_meta_ABM.xlsx")|>
    my_char_mutate(),
  
  # locatus: Zakaria
  read_excel("01 2 metadata aangeleverd/bbga_meta_locatus.xlsx")|>
    my_char_mutate(),
  
  # onderwijs: Shy-Ann
  read_excel("01 2 metadata aangeleverd/bbga_meta_onderwijs_nieuw.xlsx")|>
    my_char_mutate(),
  
  # NEET: Sjors
  read_excel("01 2 metadata aangeleverd/bbga_meta_neet.xlsx")|>
    my_char_mutate()
  
  
)
 

# nog toevoegen: 

# rob/zak: vastgoed
# renske: kinderen armoede
# Mershiha: SEC-scores

# Voor de indicatoren OSCHSO en OLLSO mag de data die nu in het BBGA staat verwijderd worden 
# en aangevuld worden vanaf 2017 (zoals data is aangeleverd). 



# samenvoewgen oude en nieuwe meta
META_TUSSEN <- bind_rows(meta_basis, meta_nieuw)


################################################
### data die verwijderd moet worden uit meta_data


groen_weg <- c(
  "ORBEBOUWD", 
  "ORVERHARD",
  "RBEBOUWD_P",
  "ORVERHARD_P") 

det_weg <- c(
  "BHCOFFEE", 
  "BHVEST_BGW",   
  "BHVEST_BGWDG",  
  "BHVEST_BGWNDG", 
  "BHWINK_1000INW",  
  "BHWINKDG_1000INW",  
  "BHVKP_LEEG",  
  "BHVKP_LEEG_P",  
  "BHBGWWVO_M2",  
  "BHBGWFOODWVO_M2",  
  "BHBGWNONFWVO_M2",   
  "BHBGWWVO_1000INW",   
  "BHVKP_WVO_LEEG",   
  "BHVKP_WVO_LEEG_P", 
  "BHBGWFOODWVO_1000INW",  
  "BHBGWNONFWVO_1000INW", 
  "BHWINKALG_R", 
  "BHVEILIG_R", 
  "BHBEREIK_R", 
  "BHPARKEER_R", 
  "BHPARKEERFIETS_R", 
  "BHFOOD_R", 
  "BHNONFOOD_R", 
  "BHUITERL_R", 
  "BHINRICHT_R", 
  "BHSFEER_R", 
  "BHSERVICE_R", 
  "BHPRIJS_R", 
  "BHDAGHORECA_R", 
  "BHVEST_BGWVOEDING", 
  "BHVEST_BGWBAKKER", 
  "BHVEST_BGWSUPERM", 
  "BHVEST_BGWDROGIST", 
  "BHVEST_BGWKLEDING", 
  "BHVEST_BGWINRICHT", 
  "BHVEST_BGWELEKTR", 
  "BHVEST_BGWIJZERW", 
  "BHVEST_BGWBOEK", 
  "BHVEST_BGWTWEEDE", 
  "BHVEST_BGWWAREN", 
  "BHVEST_BGWOVERIG", 
  "BHBINDDGL_P",      
  "BHBINDNIETDGL_P",   
  "BHWP_BGWDG", 
  "BHWP_BGWNDG")         
            

weg_var <- c(groen_weg, det_weg)

# verwijder de variabelen uit de tussenlijst (waar de nieuwe vars al instaan). Dit wordt de nieuwe metadataset
library(tidyverse)

META_DEF<-  META_TUSSEN |>
  filter(!(Variabele %in% weg_var)) |>
  mutate(Variabele      = str_trim(str_to_upper(Variabele), side = "both"))

  
openxlsx:: write.xlsx(META_DEF, "G:/OIS/Basisbestanden/basisstatistiekbestand/database/Stuurbestanden/metadata.xlsx", overwrite = T)




###########################################################################
### bestaande lijst met vervallen variablelen aanvullen met nieuwe vars ---

meta_vervallen_basis <- read_excel("G:/OIS/Basisbestanden/basisstatistiekbestand/database/Stuurbestanden/20240226_basis_rogier/VERVALLENVARIABELEN_metadata.xlsx")|>
  select(Variabele:`Vervallen omdat:`)


#voeg de verwijderde vars toevoegen aan wegvar
META_WEG<-  META_TUSSEN |>
  filter(Variabele %in% weg_var)|>
  select(Variabele,	Label,	Labelkort,	Definitie,	Bron) |>
  add_column(Verval_datum = lubridate::ymd(lubridate::as_date("2024-02-06")),
             `Vervallen omdat:` =  "vervanging door nieuwe variabele")



meta_vervallen_nieuw <- bind_rows(meta_vervallen_basis, META_WEG) |>
  mutate(Verval_datum = lubridate::ymd(lubridate::as_date(Verval_datum)))


write.xlsx(meta_vervallen_nieuw, "G:/OIS/Basisbestanden/basisstatistiekbestand/database/Stuurbestanden/VERVALLENVARIABELEN_metadata.xlsx", overwrite = T)





