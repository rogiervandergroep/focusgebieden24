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
  mutate(variabele = str_to_lower(variabele))

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
# nb wordt niet meer aangevuld door GGD !!!
# veiligheidsindexcijfers staan in BBGA maar niet op stadsdeelniveau

uitzondering <- c("sruit4_p", "pinzetbrt_p", "pinform_p", "wzbeweeg_p", "wzdepr_p" , "wzzwaar_p", "wzgezond_p",
                  "v_onvbeleving_i","v_personovl_i","v_vermijd_i","v_verloed_i","v_slacht_i","v_gercrim_i")




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
  
  #NB: thema_zuidoost_label (nieuwe indeling) vervangt thema_zuidoost (oude indeling)
  select(
    "thema_zuidoost_label",  "thema_zuidoost_nulmeting", "thema_noord", "samen_nw", "indicator_sd","thema_bbga","label_bbga" ,  
    "spatial_code","spatial_name","spatial_type","spatial_date",
    "temporal_date", "variabele" ,"value" ,  
    "mpzo","aanpak_noord","nplv","bbga",everything())|>
  
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
  arrange(temporal_date, spatial_type)|>
  
  # toevoegen jaar en gebiedsrange
  mutate(
    besch_tweedeling   = paste(unique(tweedeling_def), collapse = "|"),
    besch_jaren        = paste(unique(temporal_date),  collapse = "|"),
    besch_aggr_niveaus = paste(unique(spatial_type),   collapse = "|"))

range <- data_def2 |>
  select(variabele, starts_with("besch_")) |>
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

tabelvoorupdates<- tabel_ind_def|>
  select(indicator_sd, variabele, bron, bbga, starts_with("besch"))

write.xlsx(tabel_ind_def,  "01 indicatoren/Totaaloverzicht focusgebieden Amsterdam DEF.xlsx", withFilter=T, overwrite = T, widths = 30)
write.xlsx(tabelvoorupdates,  "overzicht inidcatoren tbv data_uitvraag.xlsx", withFilter=T, overwrite = T)


tabel_ind_def_sel <- tabel_ind_def |>
  select(indicator_sd,ind_onderdeel_van, thema_zuidoost_label,thema_noord,thema_nw_label,besch_jaren,besch_tweedeling,besch_aggr_niveaus )|>
  arrange(ind_onderdeel_van)

tabel_okt_24 <- list(
  
  tab_mazo = tabel_ind_def_sel |>
    filter(!is.na(thema_zuidoost_label))|>
    select(indicator_sd, thema_zuidoost_label, besch_jaren,besch_aggr_niveaus),
  
  tab_snw = tabel_ind_def_sel |>
    filter(!is.na(thema_nw_label))|>
    select(indicator_sd, thema_nw_label, besch_jaren,besch_aggr_niveaus),
  
  tab_noord = tabel_ind_def_sel |>
    filter(!is.na(thema_noord))|>
    select(indicator_sd, thema_noord, besch_jaren,besch_aggr_niveaus)
  
)


# Define a style with blue background for the header row
header_style <- createStyle(
  fontColour = "#FFFFFF",      
  fgFill = "#004699",
  textDecoration = "bold",
  fontSize = 11
)



## Create a new workbook
wb <- createWorkbook()

# Loop through the list and add each element to a new sheet
for (i in seq_along(tabel_okt_24)) {
  
  sheet_name <- names(tabel_okt_24)[i]  
  
  addWorksheet(wb, sheet_name)   
  
  writeData(wb, sheet_name, tabel_okt_24[[i]], headerStyle = header_style, withFilter = T) 
  setColWidths(wb, sheet_name, cols = c(1,2,3,4), widths = c(50, 40, 40, 40))
  
}


## Save workbook
saveWorkbook(wb, "01 indicatoren/totaaloverzicht indicatoren focusgebieden okt 24.xlsx", overwrite = TRUE)


## Create a new workbook
wb <- createWorkbook()

## Add a worksheet
addWorksheet(wb, "indicatoren")

writeData(wb, 1, x = tabel_ind_def_sel, withFilter = T)
setColWidths(wb, 1, cols = c(1,2,3,4,5, 6,7,8), widths = c(50, 40, 30, 30, 30, 40, 40, 40))

## Save workbook
saveWorkbook(wb, "01 indicatoren/totaaloverzicht indicatoren focusgebieden website.xlsx", overwrite = TRUE)










