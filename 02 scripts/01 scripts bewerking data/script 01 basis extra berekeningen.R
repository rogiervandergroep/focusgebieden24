
##########################
### extra berekeningen ---
##########################

var_sel<- c("spatial_code", "spatial_name", "spatial_type", "temporal_date", "variabele", "value")

extra  <- bind_rows(
  
  # berekening boomkronen oppervlakte
  data_def|>
    filter(
      variabele %in% c('orland', 'opp_boomkroon_publiek_ha'),
      temporal_date == '2023')|>
    select(all_of(var_sel))|>
    pivot_wider(
      values_from = value, 
      names_from = variabele)|>
    mutate(
      value = (opp_boomkroon_publiek_ha/orland)*100)|>
    add_column(variabele = 'opp_boomkroon_publiek_p') |>
    left_join(tabel_ind, by="variabele") |>
    select(-(c("opp_boomkroon_publiek_ha", "orland"))),
  
 
  # berekening groei vestigingen
  data_def|>
    filter(
      variabele %in% c('bhvest'))|>
    select(all_of(var_sel))|>
    group_by(
      spatial_code, spatial_name)|>
    arrange(temporal_date )|>
    mutate(
      value = value/first(value)*100)|>
    select(-variabele)|>
    add_column(variabele = 'bhvest_groei')|>
    left_join(tabel_ind, by="variabele"),
  

  # berekening publiekelijk groen per 1000 inwoners
  data_def|>
    filter(
      variabele %in% c('orpubgroen', 'bevtotaal'),
      temporal_date == '2023')|>
    select(all_of(var_sel))|>
    pivot_wider(
      values_from = value, 
      names_from = variabele)|>
    mutate(
      value = (orpubgroen/bevtotaal)*1000)|>
    add_column(variabele = 'orpubgroen_inw')|>
    left_join(tabel_ind, by="variabele")  |>
    select(-(c("orpubgroen", "bevtotaal"))),
  
 # bevolkingsgroei
 data_def|>
    filter(
      variabele %in% c('bevtotaal'))|>
    select(all_of(var_sel))|>
    group_by(
      spatial_code, spatial_name)|>
   arrange(temporal_date )|>
    mutate(
      value = value/first(value)*100)|>
   select(-variabele)|>
    add_column(variabele = 'bevtotaal_groei')|>
    left_join(tabel_ind, by="variabele"),
 
 # woningvoorraadontwikkeling
 data_def|>
   filter(
     variabele %in% c('wvoorrbag'))|>
   select(all_of(var_sel))|>
   group_by(
     spatial_code, spatial_name)|>
   arrange(temporal_date )|>
   mutate(
     value = value/first(value)*100)|>
   select(-variabele)|>
   add_column(variabele = 'wvoorrbag_groei')|>
   left_join(tabel_ind, by="variabele"),
  
 # aandeel starters gedeeld door aandeel opheffingen   NB: nieuwe variabele: werkt pas na BBGA update
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhophef_tot', 'bhvest'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = ((bhstart_tot/bhvest)/(bhophef_tot/bhvest))
     )|>
   add_column(variabele = 'bhstart_oph')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhophef_tot"))),
 
 # berekening aandeel STARTERS
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhvest'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhstart_tot/bhvest)*100)|>
   add_column(variabele = 'bhstart_tot_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhvest"))),
 
  # berekening aandeel zonder startkwalificatie
 data_def|>
   filter(
     variabele %in% c('startkwalificatie_p'),
     tweedeling_def=='totaal')|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (100-startkwalificatie_p))|>
   add_column(variabele = 'startkwal_zonder_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("startkwalificatie_p"))),
 
 # berekening aandeel werkzame personen in CI 
 data_def|>
   filter(
     variabele %in% c('bhwp_ci', 'bhwp'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhwp_ci/bhwp)*100)|>
   add_column(variabele = 'bhwp_ci_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhwp_ci", "bhwp")))
  
  )  |>
  add_column(tweedeling_def= 'totaal')

