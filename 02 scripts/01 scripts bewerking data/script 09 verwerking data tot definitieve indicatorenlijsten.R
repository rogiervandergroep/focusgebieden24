
# hieronder worden verschillende overzichten gemaakt van de indicatoren:

data_def2 <- read_rds("03 tussentijds/data_def2.rds")

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

# tabelvoorupdates<- tabel_ind_def|>
#   select(indicator_sd, variabele, bron, bbga, starts_with("besch"))

write.xlsx(tabel_ind_def,  "01 indicatoren/Totaaloverzicht focusgebieden Amsterdam DEF.xlsx", withFilter=T, overwrite = T, widths = 30)
# write.xlsx(tabelvoorupdates,  "overzicht inidcatoren tbv data_uitvraag.xlsx", withFilter=T, overwrite = T)


tabel_ind_def_sel <- tabel_ind_def |>
  select(indicator_sd, ind_onderdeel_van, 
         thema_zuidoost_label, kernindicator_zo,
         thema_noord_eenmeting , kernindicator_noord,
         thema_nw_label, kernindicator_nw,
         thema_nplv, 
         besch_jaren, besch_tweedeling, besch_aggr_niveaus )|>
  arrange(ind_onderdeel_van)


tabel_okt_24 <- list(
  
  ind_MPZO = tabel_ind_def_sel |>
    filter(!is.na(thema_zuidoost_label))|>
    select(indicator_sd, thema_zuidoost_label, kernindicator_zo, 
           besch_jaren,besch_aggr_niveaus)|>
    arrange(thema_zuidoost_label, desc(kernindicator_zo)),
  
  ind_SNW = tabel_ind_def_sel |>
    filter(!is.na(thema_nw_label))|>
    select(indicator_sd, thema_nw_label, kernindicator_nw,  
           besch_jaren,besch_aggr_niveaus)|>
    arrange(thema_nw_label, kernindicator_nw),
  
  ind_NOORD = tabel_ind_def_sel |>
    filter(!is.na(thema_noord_eenmeting))|>
    select(indicator_sd, thema_noord_eenmeting, kernindicator_noord,
           besch_jaren,besch_aggr_niveaus)|>
    arrange(thema_noord_eenmeting,desc(kernindicator_noord)),
  
  ind_NPLV = tabel_ind_def_sel |>
    filter(!is.na(thema_nplv))|>
    select(indicator_sd, thema_nplv, besch_jaren,besch_aggr_niveaus)|>
    arrange(thema_nplv)
)|>
  map(\(x)   rename(
    x,
    indicator = indicator_sd,
    `beschikbare peiljaren` = besch_jaren,
    `beschikbare buurtniveaus` = besch_aggr_niveaus)
  )


############################################

# inhoud colofon indicatorenoverzicht

Indicatorenoverzicht <- tibble("Indicatorenoverzicht" = c(
  "",
  "ter ondersteuning van de outcome monitoring van",
  "het Masterplan Zuidoost, het Nationaal Programma Samen Nieuw-West en Aanpak Noord",
  "Projectnummer: 24015",
  "",
  "Eva Karacay",
  "Marloes de Hoon",
  "Rogier van der Groep",
  "Ralph Rusconi",
  "",
  "onderzoek.amsterdam.nl",
  "rogier.van.der.groep@amsterdam.nl",
  "Amsterdam, november 2024",
  "",
  "In samenwerking met de allianties van Masterplan Zuidoost, Samen Nieuw-West en Aanpak Noord zijn verschillende indicatoren geselecteerd ter ondersteuning van de outcome monitoring in de drie focusgebieden in Zuidoost, Nieuw-West en Noord.",
  "Vanuit het Nationaal Programma Leefbaarheid en Veiligheid (NPLV) is een lijst met indicatoren voorgesteld en deze is door de allianties aangevuld.", 
  
  "Deze geselecteerde indicatoren liggen in het verlengde van de verschillende ambities en strategische doelen waarmee de allianties de komende 20 tot 25 jaar aan de slag gaan in de drie focusgebieden.",
  "",
  "",
  "In dit Excelbestand wordt per tabblad en per focusgebied een overzicht gegeven van de geselecteerde indicatoren en de daarbij horende ambities en strategische doelen.",
  "Ook is de lijst met indicatoren toegevoegd die is samengesteld door het NPLV.",
  "",
  "Inhoudsopgave",
  ""
))


inhoud <- tibble(Indicatorenoverzicht = names(tabel_okt_24))|>
  
  mutate(Indicatorenoverzicht = case_when(
    Indicatorenoverzicht == 'ind_MPZO'  ~ 'ind_MPZO lijst met indicatoren voor Masterplan Zuidoost',
    Indicatorenoverzicht == 'ind_SNW'   ~ 'ind_SNW lijst met indicatoren voor Samen Nieuw-West',
    Indicatorenoverzicht == 'ind_NOORD' ~ 'ind_NOORD lijst met indicatoren voor Aanpak Noord',
    Indicatorenoverzicht == 'ind_NPLV'  ~ 'ind_NPLV lijst met indicatoren voor NPLV')
  )

df_colofon <- bind_rows(
  
  Indicatorenoverzicht ,
  inhoud)

##########################################

source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")
## Create a new workbook
wb <- createWorkbook()

sheet_nr       <- c(1:length(tabel_okt_24))
sheet_nr_plus1 <- length(sheet_nr)+1

tabel <- tabel_okt_24

# Loop through the list and add each element to a new sheet
for (i in seq_along(tabel)) {
  
  sheet_name <- names(tabel)[i]  
  
  addWorksheet(wb, sheet_name)   
  
  writeData   (wb, sheet_name, tabel[[i]], headerStyle = style$header_left, withFilter = T) 
  addStyle    (wb, sheet_name, style$data_base,  rows = 1:(nrow(tabel[[i]])+1), cols = 1:ncol(tabel[[i]]), gridExpand = T)
  addStyle    (wb, sheet_name, style$dark_blue,  rows = 2:(nrow(tabel[[i]])+ 1), cols = c(1,3), gridExpand = T)
  addStyle    (wb, sheet_name, style$light_blue, rows = 2:(nrow(tabel[[i]])+ 1), cols = c(2,4), gridExpand = T)
  setColWidths(wb, sheet_name, cols = c(1,2,3,4,5), widths = "auto")
  
}

addWorksheet(wb, "Colofon")
writeData(wb, "Colofon", x = df_colofon)
addStyle(wb, "Colofon", style$data_colofon, rows = 1:nrow(df_colofon)+1, cols = 1,gridExpand = TRUE)
addStyle(wb, "Colofon", style$header_colofon, rows = 1:3, cols = 1,gridExpand = TRUE)

worksheetOrder(wb) <- c(sheet_nr_plus1, sheet_nr)
## Save workbook voor lijst indicatoren 
saveWorkbook(wb, "04 tabellen/05 tabellen website focusgebieden/tabel overzicht indicatoren focusgebieden jan 25.xlsx", overwrite = TRUE)


#### BRON VERMELDING Zuidoost ---


tabel_ind_def_bron <- tabel_ind_def |>
  select(
    indicator_sd, 
    bron, mpzo, aanpak_noord, 
    thema_zuidoost_label, thema_noord_eenmeting, thema_nw_label)

zo_label <- tabel_ind_def |>
  filter(!is.na(thema_zuidoost_label))|>
  select(thema_zuidoost_label)|>
  distinct()|>
  arrange(thema_zuidoost_label)|>
  mutate(thema_zuidoost_label= str_sub(thema_zuidoost_label, 1,3))|>
  pull()

no_label <- tabel_ind_def |>
  filter(!is.na(thema_noord_eenmeting))|>
  select(thema_noord_eenmeting)|>
  distinct()|>
  arrange(thema_noord_eenmeting)|>
  mutate(thema_noord_eenmeting = str_sub(thema_noord_eenmeting, 1,2))|>
  pull()
  
nw_label <- tabel_ind_def |>
  filter(!is.na(thema_nw_kleur))|>
  select(thema_nw_kleur,thema_nw_label)|>
  distinct()|>
  arrange(thema_nw_label)|>
  select(thema_nw_kleur)|>
  pull()

  


tabel_zo_bron <- tabel_ind_def_bron |>
  filter(!is.na(thema_zuidoost_label))|>
  select(thema_zuidoost_label, indicator_sd, bron)|>
  arrange(thema_zuidoost_label)|>
  group_by(thema_zuidoost_label)|>
  group_split()|>
  set_names(zo_label)

tabel_no_bron <- tabel_ind_def_bron |>
  filter(!is.na(thema_noord_eenmeting))|>
  select(thema_noord_eenmeting, indicator_sd, bron)|>
  arrange(thema_noord_eenmeting)|>
  group_by(thema_noord_eenmeting)|>
  group_split()|>
  set_names(no_label)

tabel_nw_bron <- tabel_ind_def_bron |>
  filter(!is.na(thema_nw_label))|>
  select(thema_nw_label, indicator_sd, bron)|>
  arrange(thema_nw_label)|>
  group_by(thema_nw_label)|>
  group_split()|>
  set_names(nw_label)
  
## Create a new workbook

### bronvermelding Zuidoost ---
wb <- createWorkbook()
tabel <- tabel_zo_bron

# Loop through the list and add each element to a new sheet
for (i in seq_along(tabel)) {
  
  sheet_name <- names(tabel)[i]  
  
  addWorksheet(wb, sheet_name)   
  
  writeData (wb, sheet_name, tabel[[i]], headerStyle = style$header_left) 
}

## Save workbook voor lijst indicatoren 
saveWorkbook(wb, "10 rapporten/02 rapporten Zuidoost/mazo_bron.xlsx", overwrite = T)




## Create a new workbook



### bronnen noord
wb <- createWorkbook()
tabel <- tabel_no_bron

# Loop through the list and add each element to a new sheet
for (i in seq_along(tabel)) {
  
  sheet_name <- names(tabel)[i]  
  
  addWorksheet(wb, sheet_name)   
  
  writeData (wb, sheet_name, tabel[[i]], headerStyle = style$header_left) 
}


## Save workbook voor lijst indicatoren 
saveWorkbook(wb, "10 rapporten/03 rapporten Noord/aanpak_noord_bron.xlsx", overwrite = T)



### bronnen Nieuw-West 
wb <- createWorkbook()
tabel <- tabel_nw_bron

# Loop through the list and add each element to a new sheet
for (i in seq_along(tabel)) {
  
  sheet_name <- names(tabel)[i]  
  
  addWorksheet(wb, sheet_name)   
  
  writeData (wb, sheet_name, tabel[[i]], headerStyle = style$header_left) 
}

## Save workbook voor lijst indicatoren 
saveWorkbook(wb, "10 rapporten/04 rapporten Nieuw-West/samen_nw_bron.xlsx", overwrite = T)


