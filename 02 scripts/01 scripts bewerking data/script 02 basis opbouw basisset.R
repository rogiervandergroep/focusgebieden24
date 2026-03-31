# note to self: zittende versus nieuwe bewoners tweedeling_sd
# denk aan Oostzanerwerf NA

library(tidyverse)
library(openxlsx)

source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
)

############################
### stap 1. inlezen BBGA ---
############################

# pad <- "../OS 24 BBGA UPDATE 10 2024/data/04 0 publicatie BBGA bestanden/DEF okt 24"

# inlezen BBGA
BBGA_data <- read.csv("00 ruwe data/bbga_data.csv")

# inlezen META data
BBGA_meta <- read.csv("00 ruwe data/bbga_meta.csv")

# opschonen data
BBGA_data_clean <- BBGA_data |>
  janitor::clean_names() |>
  mutate(
    measure = str_to_lower(measure),
    spatial_code = replace_na(spatialdimensioncode, "NA")
  ) |>
  rename(
    spatial_type = spatialdimensiontype,
    spatial_date = spatialdimensiondate,
    temporal_type = temporaldimensiontype,
    temporal_date = temporaldimensionstartdate
  ) |>
  select(
    measure,
    spatial_code,
    spatial_type,
    temporal_type,
    temporal_date,
    value
  ) |>
  mutate(temporal_date = lubridate::as_date(temporal_date)) |>
  filter(temporal_date > lubridate::ymd("20180101"))


# opschonen meta data
BBGA_meta_clean <- BBGA_meta |>
  janitor::clean_names() |>
  mutate(measure = str_to_lower(name)) |>
  select(measure, theme, label) |>
  set_names(c("measure", "thema_bbga", "label_bbga"))

#################################################
### stap 2 inlezen indicatoren uit input xlsx ---
#################################################

# inlezen basislijst met indicatoren
tabel_ind <- read.xlsx(
  "01 indicatoren/Totaaloverzicht focusgebieden Amsterdam INPUT.xlsx"
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

# selecteer indicatoren uit basislijst die ook in BBGA staan
tabel_ind_bbga <- tabel_ind |>
  filter(bbga == TRUE)

######################################################
### stap 3 koppelen BBGA-indicatoren aan BBGA-data ---
######################################################

BBGA_data_def <- BBGA_data_clean |>

  # koppel basislijst (input) aan lijst met alle BBGA-indicatoren
  left_join(tabel_ind_bbga, by = "measure") |>

  # verwijder de BBGA indicatoren die niet in basislijst (input) staan
  filter(
    aanpak_noord == TRUE |
      mpzo == TRUE |
      nplv == TRUE |
      basis == TRUE |
      samen_nw == TRUE
  )

# koppel de meta-data aan bestand en voeg een kolom tweedeling_def toe
data_wel_bbga <- BBGA_data_def |>
  left_join(BBGA_meta_clean, "measure") |>
  mutate(temporal_date = as.character(temporal_date)) |>
  add_column(tweedeling_def = 'totaal') # als tweedeling_def ontbreekt dan wordt de waarde 'totaal'

####################################################
### stap 4 toevoegen data die NIET in BBGA staat ---
####################################################

# van sommige vars staat het totaal in BBGA en de tweedeling in de map 'niet-bbga'
# gezondheidsindicatoren zijn handmatig voor wijken en buurten aangevuld; aanvulling gaat moeizaam
# veiligheidsindexcijfers staan in BBGA maar niet op stadsdeelniveau

# deze variabelen staan ook in BBGA, maar verkeerd en worden uit andere csv's gehaald
uitzondering <- c(
  "sruit4_p",
  "pinzetbrt_p",
  "pinform_p",
  "wzbeweeg_p",
  "wzdepr_p",
  "wzzwaar_p",
  "wzgezond_p",
  "v_onvbeleving_i",
  "v_personovl_i",
  "v_vermijd_i",
  "v_verloed_i",
  "v_slacht_i",
  "v_gercrim_i",
  "iminjong130_p",
  "iminhh130_p"
)

# selecteer indicatoren uit basislijst die NIET in BBGA staan
tabel_ind_niet_bbga <- tabel_ind |>
  filter(bbga == FALSE | measure %in% uitzondering)

# inlezen datasets die niet in BBGA staat
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
data_niet_bbga <- temp |>
  map(\(x) read.xlsx(x)) |>
  map(\(x) my_mutate(x)) |>
  map(\(x) left_join(x, tabel_ind_niet_bbga, by = "measure")) |>
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
  )

# script met dat alle gebiedsindelingen van Bas in een list plaatst
source(
  "02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R"
)

# koppelen data bbga en data niet bbga
data_def <- bind_rows(data_niet_bbga, data_wel_bbga) |>
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
    "thema_bbga",
    "label_bbga",
    "thema_nplv",

    # booleans
    "mpzo",
    "aanpak_noord",
    "samen_nw",
    "nplv",
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
data_def2 <- bind_rows(data_def, extra) |>

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
