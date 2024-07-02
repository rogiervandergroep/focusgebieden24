# note to self: zittende versus nieuwe bewoners tweedeling_sd
# denk aan Oostzanerwerf NA


library(tidyverse)
library(openxlsx)


source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

# ### uit de referentiedatabase zonder geo ---
# bbga_data<- dbGetQuery(
#   conn = os_db_con(
#     db_name = 'refdb_az',
#     path = pad_thuis_nieuw),
#   statement = "
#   SELECT jaar, gebiedcode_15, waarde, indicator_definitie_id
#   FROM public.bbga_kerncijfers
#   WHERE jaar BETWEEN 2014 AND 2024;
#   ")
# 
# ### metadata uit de referentiedatabase zonder geo ---
# bbga_meta<- dbGetQuery(
#   conn = os_db_con(
#     db_name = 'refdb_az',
#     path = pad_thuis_nieuw),
#   statement = "
#   SELECT variabele, thema, label, label_kort, definitie
#   FROM public.bbga_indicatoren_definities
#   WHERE variabele IN ('BEVTOTAAL', 'BHWP', 'BHVEST', 'WVOORRBAG', 'WDICHT');
#   ")

## stap 1. inlezen BBGA ---

#pad <- "G:/OIS/Basisbestanden/basisstatistiekbestand/NU op de OIS-site/onderzoek.amsterdam.nl/"


BBGA_data <- read.csv2("00 ruwe data/bbga_latest_and_greatest.csv") |>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele)) |>
  rename(
    spatial_code  = gebiedcode15,
    temporal_date = jaar,
    value = waarde)|>
  add_column(
    spatial_date = '20220324') |>
  mutate(spatial_code = replace_na(spatial_code , "NA"))

BBGA_meta <- read.csv("00 ruwe data/metadata_latest_and_greatest.csv")|>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele))|>
  select(variabele, thema, label) |>
  set_names(c("variabele", "thema_bbga", "label_bbga"))


### stap 2 inlezen indicatoren ---

tabel_ind <- read.xlsx("01 indicatoren/Totaaloverzicht focusgebieden Amsterdam INPUT.xlsx") |>
  janitor::clean_names()|>
  mutate(variabele = str_to_lower(variabele))

# selecteer indicatoren die in BBGA staan
tabel_ind_bbga <- tabel_ind |>
  filter(bbga == TRUE)

### stap 3 koppelen BBGA-indicatoren aan BBGA-data ---

BBGA_data_def <- BBGA_data |>
  left_join(tabel_ind_bbga, by = "variabele")|>
  filter(aanpak_noord == TRUE | mpzo == TRUE | nplv == TRUE | basis == TRUE | samen_nw == TRUE)




# koppel de meta-data aan bestand en voeg een kolom tweedeling_def toe 
data_wel_bbga <- BBGA_data_def |>
  left_join(BBGA_meta, "variabele")|>
  mutate(temporal_date = as.character(temporal_date)) |>
  add_column(tweedeling_def= 'totaal')

### stap 5 toevoegen data die niet in BBGA staat ---

# van sommige vars staat het totaal in BBGA en de tweedeling in de map 'niet-bbga'
# gezondheidsindicatoren zijn handmatig voor wijken en buurten aangevuld
uitzondering <- c("sruit4_p", "pinzetbrt_p", "pinform_p", "wzbeweeg_p", "wzdepr_p" , "wzzwaar_p", "wzgezond_p")




tabel_ind_niet_bbga <- tabel_ind |>
  filter(bbga == FALSE | variabele %in% uitzondering)

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


data_niet_bbga <- temp |>
  map(\(x) read.xlsx(x))|>
  map(\(x) my_mutate(x))|>
  map_df(\(x) left_join(x, tabel_ind_niet_bbga, by = "variabele"))|>
  filter(!is.na(bbga)) |>
  mutate(spatial_code = replace_na(spatial_code , "NA"))|>
  
  mutate(tweedeling_def = case_when(
    !is.na(tweedeling)    ~ tweedeling,
    !is.na(tweedeling_sd) ~ tweedeling_sd,
    TRUE ~ "totaal")) |>
  mutate(tweedeling_def = case_when(
    tweedeling_def %in% c("zittende_bewoner", "zittende bewoner", "langdwonend") ~ "zittende bewoner",
    tweedeling_def %in% c("nieuwe_bewoner", "nieuwe bewoner", "kortwonend")      ~ "nieuwe bewoner",
    tweedeling_def == 'totaal'    ~ "totaal")) |>
  select(-(c("tweedeling", "tweedeling_sd")))


# toevoegen juiste gebiedsindeling -
source("02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R")

# koppelen data bbga en data niet bbga
data_def <- bind_rows(data_niet_bbga, data_wel_bbga) |>
  select(-(c("spatial_type", "spatial_date")))|>
  
  mutate(
    spatial_code = case_when(
      spatial_code %in% c("STAD", "0363", '363', "[code]") ~ '0363',
      TRUE ~ spatial_code)
  ) |>
  
  # toevoegen juisten gebiedsindelingen
  left_join(
    geo_df, by ="spatial_code") |>
  
  select(
    "ambitie_zuidoost","thema_noord","indicator_sd","thema_bbga","label_bbga" ,  
    "spatial_code","spatial_name","spatial_type","spatial_date",
    "temporal_date", "variabele" ,"value" ,  
    "mpzo","aanpak_noord","nplv","bbga",everything())|>
  
  # alleen jaartal meenenem
  mutate(
    temporal_date = str_sub(temporal_date, 1, 4),
    value=as.numeric(value))|>
  
  filter(
    !is.na(spatial_name),
    temporal_date %in% c(2017:2024))

### in dit script worden specieke berekeningen gedaan ---
source("02 scripts/01 scripts bewerking data/script 00 basis extra berekeningen.R")

# extra komt uit script 'extra berekeningen'
data_def2 <- bind_rows(data_def, extra) |>
  
  mutate(
    spatial_type = factor(
      spatial_type, 
      levels = c("buurten", 'wijken', 'gebieden', 'stadsdelen', 'gemeente')),
    
    tweedeling_def = factor(
      tweedeling_def, levels = c("zittende bewoner", "nieuwe bewoner", "totaal"))
    )|>
  
  group_by(variabele)|>
  arrange(temporal_date, spatial_type)|>
  
  # toevoegen jaar en gebiedsrange
  mutate(
    besch_tweedeling   = paste(unique(tweedeling_def), collapse = "|"),
    besch_jaren        = paste(unique(temporal_date),  collapse = "|"),
    besch_aggr_niveaus = paste(unique(spatial_type),   collapse = "|"))

range <- data_def2 |>
  select(variabele,besch_jaren, starts_with("besch_")) |>
  distinct(variabele, .keep_all = T) 

tabel_ind_def <- tabel_ind |>
  left_join(range, by = "variabele")


### ranking indicatoren
onderdeel_van_levels <- c(
  '1. MPZO, Aanpak Noord, Samen Nieuw-West',
  '2. MPZO, Aanpak Noord',
  '3. MPZO, Samen Nieuw-West',
  '4. Aanpak Noord, Samen Nieuw-West',
  '5. MPZO',
  '6. Aanpak Noord',
  '7. Samen Nieuw-West',
  '8. basis'
  )


tabel_ind_def <- tabel_ind_def |>
  mutate(ind_onderdeel_van = case_when(
    (mpzo == TRUE  & aanpak_noord == TRUE   & samen_nw == TRUE)   ~ '1. MPZO, Aanpak Noord, Samen Nieuw-West',
    
    (mpzo == TRUE  & aanpak_noord == TRUE   & samen_nw == FALSE)  ~ '2. MPZO, Aanpak Noord',
    (mpzo == TRUE  & aanpak_noord == FALSE  & samen_nw == TRUE)   ~ '3. MPZO, Samen Nieuw-West',
    
    (mpzo == FALSE & aanpak_noord == TRUE   & samen_nw == TRUE)   ~ '4. Aanpak Noord, Samen Nieuw-West',
    
    (mpzo == TRUE  & aanpak_noord == FALSE  & samen_nw == FALSE)  ~ '5. MPZO',
    (mpzo == FALSE & aanpak_noord == TRUE   & samen_nw == FALSE)  ~ '6. Aanpak Noord',
    (mpzo == FALSE & aanpak_noord == FALSE  & samen_nw == TRUE)   ~ '7. Samen Nieuw-West',
    (mpzo == FALSE & aanpak_noord == FALSE  & samen_nw == FALSE)  ~ '8. basis')
    ) |>
  
  mutate(ind_onderdeel_van = factor(ind_onderdeel_van, levels = onderdeel_van_levels))



write.xlsx(tabel_ind_def,  "01 indicatoren/Totaaloverzicht focusgebieden Amsterdam DEF.xlsx", withFilter=T, overwrite = T, widths = 30)

tabel_ind_def_sel <- tabel_ind_def |>
  select(indicator_sd,ind_onderdeel_van,ambitie_zuidoost,thema_noord,thema_nw,besch_jaren,besch_tweedeling,besch_aggr_niveaus )|>
  arrange(ind_onderdeel_van)

## Create a new workbook
wb <- createWorkbook()

## Add a worksheet
addWorksheet(wb, "indicatoren")

writeData(wb, 1, x = tabel_ind_def_sel, withFilter = T)
setColWidths(wb, 1, cols = c(1,2,3,4,5, 6,7,8), widths = c(50, 40, 30, 30, 30, 40, 40, 40))

## Save workbook
saveWorkbook(wb, "01 indicatoren/totaaloverzicht indicatoren focusgebieden website.xlsx", overwrite = TRUE)

