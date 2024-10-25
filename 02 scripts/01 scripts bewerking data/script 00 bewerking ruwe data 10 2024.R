
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


#PZOND1866_P op stadsdeelniveau
#NB: deze tabel vervangt data_rebebb (alleen data beschikbaar op SD en Gebiedsniveau)
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
    value = Zelfstandigen/(Flexibel.dienstverband+Vast.dienstverband+Zelfstandigen)*100)|>
  
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
  select(all_of(bbga_kol))
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

SES_DEF <- 

write.xlsx(
  bind_rows(SES_WOA_sd, SES_AMS)|>
    mutate(temporal_date=str_sub(temporal_date, 1, 4)), 
  "00 ruwe data/niet in bbga/data_ses_woa_def.xlsx")





### vastgoedmutaties ### afkomstig van Simone: nog geen 2024

# woningen opvoer minus afvoer
# aantal opgeleverde nieuwbouw woningen
# aantal nieuw te bouwen woningen (in aanbouw genomen) bron: https://maps.amsterdam.nl/woningbouwplannen_monitor/

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

write.xlsx(woning_tot, "00 ruwe data/niet in bbga/data_won_mutaties.xlsx")



### Schuldhulpverlening

schuld <- openxlsx::read.xlsx(
  "00 ruwe data/ruw input 10 2024/schuldhulpverlening.xlsx")|>
  
  pivot_longer(
    cols= c(Amsterdam:Zuidoost), 
    values_to = 'value', 
    names_to = 'spatial_name')|>
  
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
  add_column(spatial_date = '20220324')|>
  rename(temporal_date = jaar)
  
write.xlsx(schuld, "00 ruwe data/niet in bbga/data_schuldhulpverlening.xlsx")


