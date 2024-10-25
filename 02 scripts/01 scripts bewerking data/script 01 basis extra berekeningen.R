
##########################
### extra berekeningen ---
##########################

extra  <- bind_rows(
  
  # berekening boomkronen oppervlakte
  data_def|>
    
    filter(
      variabele %in% c('orland', 'opp_boomkroon_publiek_ha'),
      temporal_date == '2023')|>
    select(spatial_code:value)|>
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
    select(spatial_code:value)|>
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
    select(spatial_code:value)|>
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
    select(spatial_code:value)|>
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
   select(spatial_code:value)|>
   group_by(
     spatial_code, spatial_name)|>
   arrange(temporal_date )|>
   mutate(
     value = value/first(value)*100)|>
   select(-variabele)|>
   add_column(variabele = 'wvoorrbag_groei')|>
   left_join(tabel_ind, by="variabele"),
  

 # starters gedeeld door opheffingen index  NB: nieuwe variabele: werkt pas na BBGA update
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhophef_tot'))|>
   select(spatial_code:value)|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhstart_tot/bhophef_tot)*1000)|>
   add_column(variabele = 'bhstart_oph')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhophef_tot"))),
 
 # berekening aandeel STARTERS
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhvest'))|>
   select(spatial_code:value)|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhstart_tot/bhvest)*100)|>
   add_column(variabele = 'bhstart_tot_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhvest"))),
 

 
 # berekening aandeel werkzame personen in CI 
 data_def|>
   filter(
     variabele %in% c('bhwp_ci', 'bhwp'))|>
   select(spatial_code:value)|>
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

