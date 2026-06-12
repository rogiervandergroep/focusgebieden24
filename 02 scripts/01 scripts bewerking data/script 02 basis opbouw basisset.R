# note to self: zittende versus nieuwe bewoners tweedeling_sd
# denk aan Oostzanerwerf NA

library(tidyverse)
library(openxlsx)

# source(
#   "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
# )

############################
### stap 1. inlezen Statistiekhub ---
############################

# inlezen Statistiekhub: gedownload van
# https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#cijfers

# 1. Download de zip
download.file(
  url = "https://api.data.amsterdam.nl/bulk-data/csv/statistieken_v2_all_openbaar.csv.zip",
  destfile = "statistieken_v2_all_openbaar.csv.zip",
  mode = "wb" # belangrijk voor binaire bestanden op Windows
)

# 2. Bekijk wat erin zit
unzip("statistieken_v2_all_openbaar.csv.zip", list = TRUE)

# 3. Alles uitpakken naar een map
unzip(
  "statistieken_v2_all_openbaar.csv.zip",
  exdir = "00 ruwe data/statistiekhub ruw"
)


# # https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#cijfers
Statistiekhub_raw <- readr::read_csv(
  "00 ruwe data/statistiekhub ruw/statistieken_v2_cijfers_openbaar.csv"
)

# # https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#indicatoren
Statistiekhub_meta <- readr::read_csv(
  "00 ruwe data/statistiekhub ruw/statistieken_v2_indicatoren_openbaar.csv"
)

# # https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#kengetallen
# Statistiekhub_kengetallen <- readr::read_csv(
#   "https://acc.api.data.amsterdam.nl/v1/statistieken/v2/kengetallen?_format=csv"
# )

Statistiekhub_data <- Statistiekhub_raw |>
  select(
    Gebiedtype,
    Gebiedcode,
    Begindatum,
    Indicatorid,
    Waarde
  )

Statistiekhub_meta <- Statistiekhub_meta |>
  select(Id, Naam)


# opschonen data
Statistiekhub_data_clean <- Statistiekhub_data |>
  left_join(Statistiekhub_meta, by = c("Indicatorid" = "Id")) |>
  rename(
    measure = Naam,
    value = Waarde,
    spatial_type = Gebiedtype,
    spatial_code = Gebiedcode,
    temporal_date = Begindatum
  ) |>
  mutate(
    spatial_code = replace_na(spatial_code, "NA")
  ) |>
  mutate(temporal_date = format(lubridate::ymd(temporal_date), "%Y-%m-%d")) |>
  filter(temporal_date > lubridate::ymd("2018-01-01")) |>
  mutate(measure = str_to_lower(measure))

#### tijdelijke oplossing WIA DATA toevoegen ----

WIA_data <- openxlsx::read.xlsx(
  "00 ruwe data/leefbaarheidscijfers_bbga_26.xlsx"
) |>
  select(-temporal_type) |>
  mutate(temporal_date = format(lubridate::ymd(temporal_date), "%Y-%m-%d")) |>
  mutate(measure = str_to_lower(measure))


### data onveiligheidsindex toevoegen ---

veiligheid_v_i <- bind_rows(
  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:3, 8)
  ),

  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:2, 9, 14)
  ),

  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:2, 15, 20)
  ),

  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:2, 21, 26)
  ),

  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:2, 27, 32)
  ),

  openxlsx::read.xlsx(
    "00 ruwe data/veiligheidsindex_2026_1_84ebd334de.xlsx",
    cols = c(1:2, 33, 38)
  )
) |>
  rename(value = `2025`) |>
  add_column(temporal_date = "2025-01-01")

### samenvoegen nieuwe wia, update veiligheidsindex en

Statistiekhub_data_clean_wia <- bind_rows(
  Statistiekhub_data_clean,
  veiligheid_v_i,
  WIA_data
) |>
  select(spatial_type:measure)

test <- Statistiekhub_data_clean_wia |>
  filter(measure == "wzdepr_p")


# opschonen meta data
# Statistiekhub_meta_clean <- Statistiekhub_meta |>
#   janitor::clean_names() |>
#   mutate(measure = str_to_lower(name)) |>
#   select(measure, theme, label) |>
#   set_names(c("measure", "thema_Statistiekhub", "label_Statistiekhub"))

#################################################
### stap 2 inlezen indicatoren uit input xlsx ---
#################################################

# inlezen basislijst met indicatoren
tabel_ind <- read.xlsx(
  "01 indicatoren/Totaaloverzicht focusgebieden Amsterdam INPUT.xlsx",
  sheet = "ind_26"
) |>
  janitor::clean_names() |>
  mutate(measure = str_to_lower(variabele)) |>
  select(
    -c(
      variabele,
      bronhouder,
      update_2026_mail_verstuurd,
      update_2026_data_aangeleverd,
      opmerking
    )
  ) |>
  select(indicator_sd, measure, everything())

# selecteer indicatoren uit basislijst die ook in Statistiekhub staan
tabel_ind_Statistiekhub <- tabel_ind |>
  filter(bbga == TRUE)


######################################################
### stap 3 koppelen Statistiekhub-indicatoren aan Statistiekhub-data ---
######################################################

Statistiekhub_data_def <- Statistiekhub_data_clean_wia |>

  # koppel basislijst (input) aan lijst met alle Statistiekhub-indicatoren
  left_join(tabel_ind_Statistiekhub, by = "measure") |>

  # verwijder de Statistiekhub indicatoren die niet in basislijst (input) staan

  # aanpassing 2026: omdat er een nieuwe nplv dataset is, worden de oude indicatoren die alleen NPLV zijn verwijderd

  filter(
    aanpak_noord == TRUE |
      mpzo == TRUE |
      # nplv == TRUE | bestaat niet meer
      basis == TRUE |
      samen_nw == TRUE
  )

# koppel de meta-data aan bestand en voeg een kolom tweedeling_def toe
data_wel_Statistiekhub <- Statistiekhub_data_def |>
  # left_join(Statistiekhub_meta_clean, "measure") |>
  mutate(temporal_date = as.character(temporal_date)) |>
  add_column(tweedeling_def = 'totaal') # als tweedeling_def ontbreekt dan wordt de waarde 'totaal'


####################################################
### stap 4 toevoegen data die NIET in Statistiekhub staat ---
####################################################

# van sommige vars staat het totaal in Statistiekhub en de tweedeling in de map 'niet-Statistiekhub'
# gezondheidsindicatoren zijn handmatig voor wijken en buurten aangevuld; aanvulling gaat moeizaam
# veiligheidsindexcijfers staan in Statistiekhub maar niet op stadsdeelniveau

# deze variabelen staan ook in Statistiekhub, maar worden uit andere csv's gehaald want opnieuw berekend met tweedeling
# in 2026 zijn geen data aangeleverd op tweedeling: voor deze tussenmeting wordt dus wel gekeken naar BBGA data
uitzondering <- c(
  # "sruit4_p",
  # "pinzetbrt_p",
  # "pinform_p",
  # "wzbeweeg_p",
  # "wzdepr_p",
  # "wzzwaar_p",
  # "wzgezond_p"
)

# selecteer indicatoren uit basislijst die NIET in Statistiekhub staan
tabel_ind_niet_Statistiekhub <- tabel_ind |>
  filter(bbga == FALSE) #| measure %in% uitzondering)


# inlezen datasets die niet in Statistiekhub staat
temp <- list.files("00 ruwe data/niet in bbga", full.names = T)

# herschrijf alle kolomnamen naar het juiste statistiek_hub format
my_mutate <- function(x) {
  vars <- c(
    "measure",
    "spatial_type",
    "spatial_code",
    "temporal_date",
    "spatial_date",
    "value",
    "tweedeling",
    "tweedeling_sd"
  )

  x |>
    mutate(across(everything(), as.character)) |>
    select(any_of(vars)) |>
    mutate(
      value = as.double(value),
      measure = str_to_lower(measure)
    )
}


# omzetten van NA's naar code Oostzaan (Amsterdam Noord)
my_replace_na <- function(x) {
  if ('spatial_code' %in% c(names(x))) {
    x |>

      mutate(
        spatial_code = case_when(
          spatial_type %in%
            c('wijk', 'Wijk', 'wijken', 'Wijken') &
            is.na(spatial_code) ~ 'NA',
          TRUE ~ spatial_code
        )
      )
  } else if ('spatial_name' %in% c(names(x))) {
    x |>

      mutate(
        spatial_code = case_when(
          spatial_name %in%
            c('Oostzanerwerf', 'oostzanerwerf') &
            is.na(spatial_code) ~ 'NA',
          TRUE ~ spatial_code
        )
      )
  }
}

# purrr treintje
data_niet_Statistiekhub <- temp |>
  map(\(x) read.xlsx(x)) |>
  map(\(x) my_mutate(x)) |>
  map(\(x) left_join(x, tabel_ind_niet_Statistiekhub, by = "measure")) |>
  map_df(\(x) filter(x, !is.na(bbga))) |>
  my_replace_na() |>
  mutate(
    tweedeling_def = case_when(
      # toevoegen totaal aan de tabellen zonder tweedeling
      is.na(tweedeling) ~ "totaal",

      # vervangen foute indelingen naar juiste indeling
      str_detect(tweedeling, pattern = "zittend") ~ "zittende bewoner",
      str_detect(tweedeling, pattern = "lang") ~ "zittende bewoner",
      str_detect(tweedeling, pattern = "nieuw") ~ "nieuwe bewoner",
      str_detect(tweedeling, pattern = "kort") ~ "nieuwe bewoner",
      TRUE ~ tweedeling
    )
  ) |>
  mutate(
    temporal_date = case_when(
      temporal_date == '46023' ~ '20260101',
      temporal_date == '45658' ~ '20250101',
      temporal_date == '45292' ~ '20240101',
      TRUE ~ temporal_date
    )
  )


# script met dat alle gebiedsindelingen van Bas in een list plaatst
source(
  "02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R"
)

# koppelen data Statistiekhub en data niet Statistiekhub
data_def <- bind_rows(data_niet_Statistiekhub, data_wel_Statistiekhub) |>
  select(-(c("spatial_type", "spatial_date"))) |>

  mutate(
    spatial_code = case_when(
      spatial_code %in% c("STAD", "0363", '363', "[code]") ~ '0363',
      TRUE ~ spatial_code
    )
  ) |>

  # toevoegen juiste gebiedsindelingen
  left_join(
    geo_df,
    by = "spatial_code"
  ) |>

  #NB: thema_zuidoost_label (nieuwe indeling) vervangt thema_zuidoost (oude indeling)
  select(all_of(c(
    "indicator_sd",
    "measure",
    "bron",
    "value",
    "temporal_date",
    "spatial_code",
    "spatial_name",
    "spatial_type",
    "tweedeling_def",

    # zuidoost_label vervangt thema_zuidoost
    "thema_zuidoost_label",

    # noord_eenmeting vervangt thema_noord
    "thema_noord_eenmeting",
    "thema_nw_kleur",
    "thema_nw_label",
    # "thema_Statistiekhub", (bestaat niet meer)
    # "label_Statistiekhub", (bestaat niet meer)
    # "thema_nplv", (bestaat niet meer sinds 2026)

    # booleans
    "mpzo",
    "aanpak_noord",
    "samen_nw",
    # "nplv", (bestaat niet meer)
    "bbga",
    "basis",

    #kerninidcator booleans
    "kernindicator_zo",
    "kernindicator_noord",
    "kernindicator_nw",

    # datum van de gebiedsindeling
    "spatial_date"
  ))) |>

  # alleen jaartal meenemen
  mutate(
    temporal_date = str_sub(temporal_date, 1, 4),
    value = as.numeric(value)
  ) |>

  filter(
    !is.na(spatial_name)
  )

### in dit script worden specieke berekeningen gedaan ---
source(
  "02 scripts/01 scripts bewerking data/script 01 basis extra berekeningen.R"
)


# extra komt uit script 'extra berekeningen'
data_def2 <- bind_rows(
  data_def,
  extra
) |>

  mutate(
    spatial_type = factor(
      spatial_type,
      levels = c(
        "winkelgebieden",
        "buurten",
        'wijken',
        'gebieden',
        'stadsdelen',
        'gemeente'
      )
    ),

    tweedeling_def = factor(
      tweedeling_def,
      levels = c("zittende bewoner", "nieuwe bewoner", "totaal")
    )
  ) |>

  group_by(measure) |>

  arrange(temporal_date) |>
  mutate(besch_jaren = paste(unique(temporal_date), collapse = "|")) |>
  mutate(
    jaar_min = min(temporal_date),
    jaar_max = max(temporal_date)
  ) |>

  arrange(spatial_type) |>
  mutate(besch_aggr_niveaus = paste(unique(spatial_type), collapse = "|")) |>

  arrange(tweedeling_def) |>
  mutate(besch_tweedeling = paste(unique(tweedeling_def), collapse = "|"))

######################################################################################################
### data_def2 ### wordt gebruikt voor de verwerking naar afzonderlijke datasets voor Zuidoost, Noord en NW
######################################################################################################

### toevoegen winkelgebieden

write_rds(data_def2, "03 tussentijds/data_def2.rds")
write_rds(tabel_ind, "03 tussentijds/tabel_ind.rds")
