
library(tidyverse)
library(openxlsx)
library(sf)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


tabel_sd <- os_get_geom("stadsdelen")|> 
  st_drop_geometry()|>
  select(naam, code)|>
  set_names(c("spatial_name", "spatial_code"))

tabel_geb <- os_get_geom("gebieden")|> 
  st_drop_geometry()|>
  select(naam, code)|>
  set_names(c("spatial_name", "spatial_code"))


bbga_kol <- c(
  "spatial_code",	"spatial_type",	"spatial_date", 
  "temporal_date","temporal_type","measure",	
  "value","tweedeling")

#############################################
### bewerking masterplandata oktober 2024 ---
#############################################

### Simone: aandeel thuiswonend = aanvulling op eerdere jaargangen
df_thuiswonend <- read.csv2("00 ruwe data/ruw input 10 2024/data_thuiswonend.csv")|>
  add_column(tweedeling="totaal")
write.xlsx(df_thuiswonend, "00 ruwe data/niet in bbga/data_thuiswonend_24_def.xlsx")
# variabelen staan in inputlijst -


### Sara: Data Burgermonitor - NB: nog toevoegen totaal ---
df_abm <- openxlsx::read.xlsx("00 ruwe data/ruw input 10 2024/Data ABM.xlsx")|>
  mutate(measure = case_when(
    measure == 'verbondheid met de buurt'        ~ 'LVERBONDENBUURT_P',
    measure == 'verbondenheid met het stadsdeel' ~ 'LVERBONDST_P',
    TRUE                                         ~ measure)
  )

df_abm_totaal <- openxlsx::read.xlsx("00 ruwe data/ruw input 10 2024/Burgermonitor tbv BBGA.xlsx")|>
  add_column(tweedeling = 'totaal')


df_abm_df<- bind_rows(df_abm,df_abm_totaal)|>
  mutate(spatial_code = case_when(
    spatial_code == '363'~  '0363',
    TRUE ~ spatial_code)
  )

write.xlsx(df_abm_df, "00 ruwe data/niet in bbga/data_burgermonitor_24_def.xlsx")
# variabelen staan in inputlijst -

### Isabel
# aandeel flexibel dienstverband van werkenden in loondienst (18 t/m 66) (%), bron: POLIS; laatste jaar 23030101  op precies dezelfde manier bepaald 
# aandeel werkenden werkzaam als ondernemer (18 t/m 66) (%),  bron:  EBBREB: laatste jaar 2021  (met terugwerkende kracht) 2019 t/m 2023. Op basis van enkel de EBB. 

# dit is een toevoeging voor polis_23
df_flex_werkend   <- read.xlsx("00 ruwe data/ruw input 10 2024/20240912_aandeelflex.xlsx")|>
  add_column(tweedeling = 'totaal')|>
  select(spatial_code,	spatial_type,	spatial_date, temporal_date,	temporal_type,	measure,	value,	tweedeling)
write.xlsx(df_flex_werkend, "00 ruwe data/niet in bbga/data_polisadministratie_24_def.xlsx")


# NB: onderstaande data wordt door feiko geleverd aan bbga
# PWERKENDJEUGD_P
# PWERKEND_P
# PJEUGDW_P
# PWERKLBBV_P
# PWERKENDVROUW_P
# PBBV_P
# PBBVJEUGD_P

######################################################################################
# PZOND1866_P op stadsdeelniveau
# NB: deze tabel vervangt data_rebebb (alleen data beschikbaar op SD en Gebiedsniveau)
df_aandeel_ondern_sd <- read.xlsx("00 ruwe data/ruw input 10 2024/20240912_aandeel_zelfstandigen_gewogen.xlsx", sheet = 1 ) |>
  rename(
    'temporal_date'= 'Jaar', 
    'spatial_name' = 'gbd_sdl_naam',
    'value'        = 'aandeel.zelfstandigen')|>
  add_column(
    tweedeling    = 'totaal',
    spatial_type  = 'Stadsdelen',
    temporal_type = 'peildatum',
    spatial_date  = '20220324',
    measure       = 'PZOND1866_P')|>
  mutate(temporal_date = str_glue("{temporal_date}0101"))|>
  left_join(tabel_sd, by = 'spatial_name')

#PZOND1866_P op gemeenteniveau
df_aandeel_ondern_ams <- df_aandeel_ondern_sd |>
  group_by(temporal_date)|>
  summarise(
    Flexibel.dienstverband = sum (Flexibel.dienstverband),
    Vast.dienstverband     = sum(Vast.dienstverband),
    Zelfstandigen          = sum(Zelfstandigen)
  )|>
  mutate(
    value = Zelfstandigen/(Flexibel.dienstverband+Vast.dienstverband+Zelfstandigen)
    )|>
  
  add_column(
    spatial_name = 'Amsterdam',
    spatial_code = '0363',
    tweedeling    = 'totaal',
    spatial_type  = 'Gemeente',
    temporal_type = 'peildatum',
    spatial_date  = '20220324',
    measure       = 'PZOND1866_P')


#PZOND1866_P op gebiedsniveau
#NB: deze tabel vervangt data_rebebb (alleen data beschikbaar op SD en Gebiedsniveau)
df_aandeel_ondern_geb <- read.xlsx("00 ruwe data/ruw input 10 2024/20240912_aandeel_zelfstandigen_gewogen.xlsx", sheet = 2 ) |>
  rename(
    'temporal_date'= 'Jaar', 
    'spatial_name' = 'gbd_ggw_naam',
    'value'= 'aandeel.zelfstandigen')|>
  mutate(
    temporal_date = str_glue("{temporal_date}0101")
  )|>
  add_column(
    tweedeling    = 'totaal',
    spatial_type  = 'Gebieden',
    temporal_type = 'peildatum',
    spatial_date  = '20220324',
    measure       = 'PZOND1866_P')|>
  left_join(tabel_geb, by = 'spatial_name')|>
  
  mutate(spatial_code=case_when(
    spatial_name == 'Oud-West, De Baarsjes' ~ 'GE05',
    TRUE ~ spatial_code)
    )

df_aandeel_ondern <- bind_rows(df_aandeel_ondern_sd,df_aandeel_ondern_geb, df_aandeel_ondern_ams)|>
  select(all_of(bbga_kol))|>
  mutate(value=value*100)

write.xlsx(df_aandeel_ondern, "00 ruwe data/niet in bbga/data_aandeelondernemers_19_23_def.xlsx")

#################################################################################################
    
###########################
# DATA DAAN: jeugdhulp ----
###########################

df_jeugdhulp <- read.xlsx("00 ruwe data/ruw input 10 2024/jeugdhulp_2018_2022 ruw.xlsx") 
 
df_jeugdhulp_def<- df_jeugdhulp |>
  mutate(lft_cat_jgd = str_replace_all(lft_cat_jgd, " t/m ", "tm" ))|>
  mutate(
    measure = str_glue("{measure}_{lft_cat_jgd}_p"),
    measure = str_remove_all(measure, "heeft_"))|>
  filter(heeft_jeugdhulp == 1)|>
  mutate(value = value * 100)|>
  add_column(tweedeling = 'totaal')|>
  select(all_of(bbga_kol))

# toevoeging totaal tot 17 jaar

df_jeugdhulp_17<- df_jeugdhulp |>
  mutate(lft_cat_jgd = str_replace_all(lft_cat_jgd, " t/m ", "tm" ))|>
  mutate(
    measure = str_glue("{measure}_{lft_cat_jgd}_p"),
    measure = str_remove_all(measure, "heeft_"))|>
  filter(
    lft_cat_jgd != '18tm22',
    lft_cat_jgd != '23plus')|>
  group_by(spatial_code, spatial_type, temporal_type,spatial_date, temporal_date, heeft_jeugdhulp)|>
  summarise(n=sum(n))|>
  group_by(spatial_code, spatial_type, temporal_type, spatial_date, temporal_date)|>
  mutate(value=n/sum(n)*100)|>
  filter(heeft_jeugdhulp == 1)|>
  add_column(
    measure = 'jeugdhulp_0tm17_p',
    tweedeling = 'totaal')|>
  select(all_of(bbga_kol))

write.xlsx(bind_rows(df_jeugdhulp_def, df_jeugdhulp_17), "00 ruwe data/niet in bbga/data_jeugdhulp_def.xlsx")
############################################################################################################

###################################
### Sjors: startkwalifactie en secm
###################################


# NEET_JONGEREN_P uit BBGA wordt vervangen door neet_1826_p (inverse van werkopl_1826_p )
# werkopl_1826_p
# werkopl_1866_p

# niet meer geleverd
# werkopl_1866_hbowo_p
# werkopl_1866_max_mbo1_p
# werkopl_1866_mbo2_4_havovwo_p
# werkopl_1866_onbekend_p

# deze worden vervangen door
# WERKEND_OPL_1866_P_HOOG
# WERKEND_OPL_1866_P_LAAG
# WERKEND_OPL_1866_P_MIDDEN


df_secm <- read.xlsx("00 ruwe data/ruw input 10 2024/mpzo_secm.xlsx")|>
  rename(
    value = waarde,
    tweedeling = tweedeling_sd)|>
  select(all_of(bbga_kol))

df_secm_neet <- df_secm |>
  filter(measure == 'WERKOPL_1826_P')|>
  mutate(measure = 'NEET_1826_P',
         value = 100-value)

write.xlsx(bind_rows(df_secm,df_secm_neet), "00 ruwe data/niet in bbga/data_secm_24_def.xlsx")
  
df_startkwal  <- read.xlsx("00 ruwe data/ruw input 10 2024/mpzo_startkwalificaties_ses.xlsx")  |>
  rename(
    value = waarde,
    tweedeling = tweedeling_sd)|>
  select(all_of(bbga_kol))
write.xlsx(df_startkwal, "00 ruwe data/niet in bbga/data_startkwal_ses_24_def.xlsx")
# alle indicatoren staan ook in inputlijst 



### aandeel nederlandse en niet-nederlandse herkomst 

df_buit <- read.xlsx("00 ruwe data/ruw input 10 2024/jongeren_leeftijdsgroep_geboorteplaats_buurt_wijk.xlsx", sheet = 'T5') |>
  group_by(VERSLAGJAAR, WIJK, LEEFTIJDSGROEP)|>
  mutate(
    value = aantal/sum(aantal)*100
    )|>
  mutate(measure = case_when(
    LEEFTIJDSGROEP == '18 t/m 27 jaar' & GEBOORTEPLAATS == 'Amsterdam'  ~ 'geb_ams_18_27_p',
    LEEFTIJDSGROEP == '12 t/m 17 jaar' & GEBOORTEPLAATS == 'Amsterdam'  ~ 'geb_ams_12_17_p',
    
    LEEFTIJDSGROEP == '18 t/m 27 jaar' & GEBOORTEPLAATS == 'Buiten Amsterdam'  ~ 'geb_nl_18_27_p',
    LEEFTIJDSGROEP == '12 t/m 17 jaar' & GEBOORTEPLAATS == 'Buiten Amsterdam'  ~ 'geb_nl_12_17_p',
    
    LEEFTIJDSGROEP == '18 t/m 27 jaar' & GEBOORTEPLAATS == 'Buitenland' ~ 'geb_bui_18_27_p',
    LEEFTIJDSGROEP == '12 t/m 17 jaar' & GEBOORTEPLAATS == 'Buitenland' ~ 'geb_bui_12_17_p')
  )
#wegschrijven in excel
write.xlsx(df_buit ,  "00 ruwe data/niet in bbga/data_geboorteplek_24_def.xlsx")


### HIERONDER ALLEMAAL INDICATOREN VOOR NPLV ---
### SES_WOA afkomstig van Statline ---

SES_WOA <- read.csv2("00 ruwe data/ruw input 10 2024/Sociaal_economische_status_21102024_165251.csv")|>
  purrr::set_names(c("temporal_date", "spatial_name", "huishoudens", "value"))|>
  add_column(measure = 'SES_WOA')|>
  group_by(temporal_date, spatial_name)|>
  filter(huishoudens == max(huishoudens))|>
  distinct(temporal_date, spatial_name, .keep_all = T)

SES_AMS <- SES_WOA |>
  filter(spatial_name == 'Amsterdam')|>
  select(-huishoudens)|>
  add_column(
    spatial_code = '0363', 
    spatial_date = '20220324')
    
  
  
# huishoudens <- read.csv2("00 ruwe data/bbga_latest_and_greatest.csv")|>
#   filter(variabele == 'BEVHUISHOUDENHH')

wijk <- os_get_geom("wijken")|>
  st_drop_geometry()|>
  select(naam, code, stadsdeelNaam, stadsdeelCode)|>
  add_column(spatial_type = 'wijken')

# test_df
SES_WOA_wijk <- SES_WOA |>
  right_join(wijk, by = c('spatial_name'='naam'))|>
  filter(stadsdeelCode != 'B')|>
  group_by(temporal_date, stadsdeelNaam)|>
  mutate(
    gem_ong     = mean(value, na.rm = T),
    weegfactor  = (huishoudens/sum(huishoudens))/(1/n()),
    gem_gew     = sum(value*weegfactor, na.rm = T)/n()
    )

write.xlsx(
  SES_WOA_wijk, "00 ruwe data/ruw input 10 2024/SES_gewogen.xlsx")

# definitieve bestand
SES_WOA_sd <- SES_WOA_wijk |>
  select(
    temporal_date, measure, stadsdeelNaam, stadsdeelCode, gem_gew)|>
  distinct(, .keep_all = T) |>
  set_names(
    c("temporal_date", "measure", "spatial_name", "spatial_code", "value")
    )|>
  add_column(spatial_date = '20220324')


write.xlsx(
  bind_rows(SES_WOA_sd, SES_AMS)|>
    mutate(temporal_date=str_sub(temporal_date, 1, 4)), 
  "00 ruwe data/niet in bbga/data_ses_woa_def.xlsx")





### vastgoedmutaties ### afkomstig van Simone: nog geen 2024

# woningen opvoer minus afvoer
# aantal opgeleverde nieuwbouw woningen
# aantal nieuw te bouwen woningen (in aanbouw genomen) bron: https://maps.amsterdam.nl/woningbouwplannen_monitor/
# nb: mutaties 2024 zijn leverbaar in januari 2024


woningbouw <- openxlsx::read.xlsx(
  "00 ruwe data/ruw input 10 2024/mutaties_naar_stadsdeel.xlsx")|>
  mutate(spatial_name=str_sub(spatial_name, 3))
  

won_ams <- woningbouw |>
  group_by(temporal_date)|>
  summarise(
    wopvoer_nieuw  = sum(wopvoer_nieuw),
    wopvoer_afvoer = sum(wopvoer_afvoer)
    )|>
  add_column(
    spatial_name = 'Amsterdam',
    spatial_code = '0363')
  
woning_tot<-bind_rows(won_ams, woningbouw)|>
  pivot_longer(
    cols = c(wopvoer_nieuw, wopvoer_afvoer),
    values_to = 'value', 
    names_to = 'measure')|>
  add_column(spatial_date = '20220324')

nieuwbouw <- openxlsx::read.xlsx(
  "00 ruwe data/ruw input 10 2024/mutaties_naar_stadsdeel.xlsx", "nieuwbouwplannen")|>
  mutate(spatial_date = as.character(spatial_date))


write.xlsx(bind_rows(woning_tot,nieuwbouw), "00 ruwe data/niet in bbga/data_won_mutaties.xlsx")

### Schuldhulpverlening

### ??? ---

### aandeel met schulden uit REB via Marloes---
schulden <- openxlsx::read.xlsx(
  "00 ruwe data/ruw input 10 2024/0. bbga problematische schulden per stadsdeel 2019-2023.xlsx")


schuld_sd <- schulden |>
  select(stadsdeelnaam:aantal_nee)|>
  mutate(value = round(aantal_ja/(aantal_ja+aantal_nee)*100,1))|>
  add_column(
    measure = 'PROBSCHULD_P',
    spatial_date = '20220324')|>
  rename(
    temporal_date=Jaar,
    spatial_name=stadsdeelnaam)|>
  mutate(spatial_code = case_when(
    spatial_name == 'Amsterdam'   ~ '0363',
    spatial_name == 'Centrum'     ~ 'A',
    spatial_name == 'West'        ~ 'E',
    spatial_name == 'Nieuw-West'  ~ 'F',
    spatial_name == 'Zuid'        ~ 'K',
    spatial_name == 'Oost'        ~ 'M',
    spatial_name == 'Noord'       ~ 'N',
    spatial_name == 'Zuidoost'    ~ 'T')
  )|>
  select(
    spatial_name, spatial_code, spatial_date, temporal_date, measure, value )
  
  

schuld_ams <- schulden |>
  select(stadsdeelnaam:aantal_nee)|>
  group_by(Jaar)|>
  summarise(
    aantal_ja  = sum(aantal_ja),
    aantal_nee = sum(aantal_nee))|>
  mutate(value = round(aantal_ja/(aantal_ja+aantal_nee)*100,1))|>
  add_column(
    measure      = 'PROBSCHULD_P',
    spatial_date = '20220324',
    spatial_name = 'Amsterdam',
    spatial_code = '0363')|>
  rename(
    temporal_date=Jaar)|>
  select(
    spatial_name, spatial_code, spatial_date, temporal_date, measure, value )


  
write.xlsx(bind_rows(schuld_sd,schuld_ams),"00 ruwe data/niet in bbga/data_prob_schulden.xlsx")

# veiligheidsindexen toegevoegd eind november 2024
veiligheidsindex <- openxlsx::read.xlsx(
  "00 ruwe data/ruw input 10 2024/data_veiligheidsindex_stadsdelen.xlsx", sheet = 'index_basis')

veiligheidsindex_long <- veiligheidsindex |>
  pivot_longer(cols= c(v_gercrim_i:v_vermijd_i), names_to = 'measure' , values_to = 'value')|>
  add_column(spatial_date='20220324')

write.xlsx(veiligheidsindex_long,"00 ruwe data/niet in bbga/data_veiligheidsindex.xlsx")


# zittende en nieuwe bewoners toegevoegd begin december 2024

df_zittend <- bind_rows(
  
  openxlsx::read.xlsx(
    "00 ruwe data/ruw input 10 2024/zittende_en_nieuwe_bewoners_in_stadsdeel.xlsx", sheet ='buurt')|>
    add_column(spatial_type = 'buurt')|>
    rename (
      spatial_name = gbd_brt_naam,
      spatial_code = gbd_brt_code)|>
    filter(spatial_code != 'Totaal'),
  
  openxlsx::read.xlsx(
    "00 ruwe data/ruw input 10 2024/zittende_en_nieuwe_bewoners_in_stadsdeel.xlsx", sheet ='wijk')|>
    add_column(spatial_type = 'wijk')|>
    rename (
      spatial_name = gbd_wijk_naam,
      spatial_code = gbd_wijk_code)|>
    mutate(spatial_name = replace_na(spatial_name, "NA"))|>
    filter(spatial_code != 'Totaal'),
  
  openxlsx::read.xlsx(
    "00 ruwe data/ruw input 10 2024/zittende_en_nieuwe_bewoners_in_stadsdeel.xlsx", sheet ='ggw')|>
    add_column(spatial_type = 'ggw')|>
    rename (
      spatial_name = gbd_ggw_naam,
      spatial_code = gbd_ggw_code)|>
    filter(spatial_code != 'Totaal'),
  
  openxlsx::read.xlsx(
    "00 ruwe data/ruw input 10 2024/zittende_en_nieuwe_bewoners_in_stadsdeel.xlsx", sheet ='stadsdeel')|>
    add_column(spatial_type = 'stadsdeel')|>
    rename (
      spatial_name = gbd_sdl_naam,
      spatial_code = gbd_sdl_code)
    ) |>
  
  mutate(spatial_code = str_replace_all(spatial_code, "Totaal", "0363"))|>
  mutate(spatial_type = case_when(
    spatial_code == '0363' ~ 'gemeente',
    TRUE ~ spatial_type)
  ) |>
  add_column(
    temporal_date = '20240101',
    spatial_date  = '20220324')|>
  mutate(
    bew_nieuw = (aantal_nieuwe_bewoners_in_stadsdeel / totaal_18_plussers)*100,
    bew_oud  = (aantal_zittende_bewoners_in_stadsdeel / totaal_18_plussers)*100
  ) |>
  
  pivot_longer(cols = c(
    aantal_nieuwe_bewoners_in_stadsdeel,
    aantal_zittende_bewoners_in_stadsdeel, 
    totaal_18_plussers,
    bew_nieuw, bew_oud), names_to = 'measure')
           
  
write.xlsx(df_zittend,"00 ruwe data/niet in bbga/data_bew_zittend_nieuw.xlsx")
  
  
### toevoeging 2021 Armoedemonitor data bminreg_p en bminregjong_p

# inlezen gebieden met gebiedscodes
source("02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R")


geo_list <- geo_list|>
  map(\(x) select(x, spatial_code, spatial_name))

library(openxlsx)

data_reg_23 <- read.xlsx(
  "00 ruwe data/ruw input 10 2024/Stadsdeelrapportage Amsterdamse Armoedemonitor 2023.xlsx",
  sheet = "hh bereik", startRow = 2)|>
  select(spatial_type, spatial_name, BMINREG_P, BMINREGJONG_P)


df_reg_23 <- bind_rows(

  # op wijkniveau
  data_reg_23 |>

    filter(spatial_type == 'wijken')|>
    left_join(geo_list$wijken_nieuw, by= "spatial_name")|>
    pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure'),

  # op gebiedniveau
  data_reg_23 |>
  
    filter(spatial_type == 'gebieden')|>
    left_join(geo_list$gebieden_nieuw, by= "spatial_name")|>
    pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure' ),

  # op stadsdeel niveau en Amsterdam
  data_reg_23 |>
    
    filter(spatial_type %in% c('stadsdelen', 'gemeente'  ))|>
    left_join(geo_list$sd_nieuw, by= "spatial_name")|>
    pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure' )
) |>

  mutate(

    spatial_code = case_when(
      spatial_name == 'Oud-West, De Baarsjes' ~ 'GE05',
      spatial_name == 'Amsterdam' ~ '0363',
      TRUE ~ spatial_code)
    )|>
  add_column(
    spatial_date  = '20220324',
    temporal_date = '20230101')



write.xlsx(df_reg_23, "00 ruwe data/niet in bbga/data_regelingen_23.xlsx")
  
  
    

