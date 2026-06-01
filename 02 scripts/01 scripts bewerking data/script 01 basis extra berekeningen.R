##########################
### extra berekeningen ---
##########################

# met dit script worden extra indicatoren toegevoegd. Vaak gaat het om aandelen op basis van bestaande indicatoren.
# In de functie geef je aan welke twee measuren er toegevoegd worden en hoe de nieuwe measure heet
# er zijn groeicijfers; percentages; dichtheden

# my_berekening<- function (x, berekening, teller, noemer, newvar) {
#
#
#   y <- x |>
#     filter(
#       measure %in% c('bhvest'))|>
#     select(all_of(var_sel))|>
#     group_by(
#       spatial_code, spatial_name)|>
#     arrange(temporal_date )
#
#   if (berekening = 'groeicijfer') {
#
#     mutate(
#       value = value/first(value)*100)|>
#
#   }
#
#     select(-measure)|>
#     add_column(measure = 'bhvest_groei')|>
#     left_join(tabel_ind, by="measure")
#
#
# }

var_sel <- c(
  "spatial_code",
  "spatial_name",
  "spatial_type",
  "spatial_date",
  "temporal_date",
  "measure",
  "value"
)

extra <- bind_rows(
  # berekening groei vestigingen
  data_def |>
    filter(
      measure %in% c('bhvest')
    ) |>
    select(all_of(var_sel)) |>
    group_by(
      spatial_code,
      spatial_name
    ) |>
    arrange(temporal_date) |>
    mutate(
      value = value / first(value) * 100
    ) |>
    select(-measure) |>
    add_column(measure = 'bhvest_groei') |>
    left_join(tabel_ind, by = "measure"),

  # data_def |>
  #   filter(
  #     measure %in% c('orntrgroen', 'orpubland')
  #   ) |>
  #   select(all_of(var_sel)) |>
  #   pivot_wider(
  #     values_from = value,
  #     names_from = measure
  #   ) |>
  #   mutate(
  #     value = (orntrgroen / orpubland) * 100
  #   ) |>
  #   add_column(measure = 'orntrgroen_p') |>
  #   left_join(tabel_ind, by = "measure") |>
  #   select(-(c("orntrgroen", "orpubland"))),

  # berekening publiekelijk groen per 1000 inwoners
  # data_def |>
  #   filter(
  #     measure %in% c('orpubgroen', 'bevtotaal')
  #   ) |>
  #   select(all_of(var_sel)) |>
  #   pivot_wider(
  #     values_from = value,
  #     names_from = measure
  #   ) |>
  #   mutate(
  #     value = (orpubgroen / bevtotaal) * 1000
  #   ) |>
  #   add_column(measure = 'orpubgroen_inw') |>
  #   left_join(tabel_ind, by = "measure") |>
  #   select(-(c("orpubgroen", "bevtotaal"))),

  # bevolkingsgroei
  data_def |>
    filter(
      measure %in% c('bevtotaal')
    ) |>
    select(all_of(var_sel)) |>
    group_by(
      spatial_code,
      spatial_name
    ) |>
    arrange(temporal_date) |>
    mutate(
      value = value / first(value) * 100
    ) |>
    select(-measure) |>
    add_column(measure = 'bevtotaal_groei') |>
    left_join(tabel_ind, by = "measure"),

  # woningvoorraadontwikkeling
  data_def |>
    filter(
      measure %in% c('wvoorrbag')
    ) |>
    select(all_of(var_sel)) |>
    group_by(
      spatial_code,
      spatial_name
    ) |>
    arrange(temporal_date) |>
    mutate(
      value = value / first(value) * 100
    ) |>
    select(-measure) |>
    add_column(measure = 'wvoorrbag_groei') |>
    left_join(tabel_ind, by = "measure"),

  # aandeel starters gedeeld door aandeel opheffingen   NB: nieuwe measure: werkt pas na BBGA update
  data_def |>
    filter(
      measure %in% c('bhstart_vest', 'bhophef_vest', 'bhvest')
    ) |>
    select(all_of(var_sel)) |>
    pivot_wider(
      values_from = value,
      names_from = measure
    ) |>
    mutate(
      value = ((bhstart_vest / bhvest) / (bhophef_vest / bhvest))
    ) |>
    add_column(measure = 'bhstart_oph') |>
    left_join(tabel_ind, by = "measure") |>
    select(-(c("bhstart_vest", "bhophef_vest", "bhvest"))),

  # berekening aandeel STARTERS
  data_def |>
    filter(
      measure %in% c('bhstart_vest', 'bhvest')
    ) |>
    select(all_of(var_sel)) |>
    pivot_wider(
      values_from = value,
      names_from = measure
    ) |>
    mutate(
      value = (bhstart_vest / bhvest) * 100
    ) |>
    add_column(measure = 'bhstart_tot_p') |>
    left_join(tabel_ind, by = "measure") |>
    select(-(c("bhstart_vest", "bhvest"))),

  # berekening aandeel zonder startkwalificatie
  data_def |>
    filter(
      measure %in% c('startkwalificatie_p'),
      tweedeling_def == 'totaal'
    ) |>
    select(all_of(var_sel)) |>
    pivot_wider(
      values_from = value,
      names_from = measure
    ) |>
    mutate(
      value = (100 - startkwalificatie_p)
    ) |>
    add_column(measure = 'startkwal_zonder_p') |>
    left_join(tabel_ind, by = "measure") |>
    select(-(c("startkwalificatie_p")))

  # berekening aandeel werkzame personen in CI
  # data_def |>
  #   filter(
  #     measure %in% c('bhwp_ci', 'bhwp')
  #   ) |>
  #   select(all_of(var_sel)) |>
  #   pivot_wider(
  #     values_from = value,
  #     names_from = measure
  #   ) |>
  #   mutate(
  #     value = (bhwp_ci / bhwp) * 100
  #   ) |>
  #   add_column(measure = 'bhwp_ci_p') |>
  #   left_join(tabel_ind, by = "measure") |>
  #   select(-(c("bhwp_ci", "bhwp")))
) |>
  add_column(tweedeling_def = 'totaal') |>
  select(-c(thema_zuidoost, thema_noord, thema_zuidoost_nulmeting)) # deze zijn vervangen door nieuwe namen

## zuidoost_label vervangt thema_zuidoost
## noord_eenmeting vervangt thema_noord
