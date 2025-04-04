


# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/04 scripts publicaties Nieuw-West/script 22 publicatie NW factsheets algemeen data.R")
source("02 scripts/01 scripts bewerking data/script 50 functies voor kaarten.R")


breed = 10
hoog = 6
aantal_kolommen = 1
legenda_pos = 'right'
my_kaart("Nieuw-West")

focusbuurten <- c(
  "FB03",  "FB08",  "FC02",  "FC03",  "FE01",  "FE02",  "FJ01",  "FK01",  "FL03",  "FM06"
)

breed = 10
hoog = 6

# algemeen kaartje met wijknamen Nieuw-West
legenda_pos = 'right'
aantal_kolommen = 1
my_focus_buurt_kaart("Nieuw-West")



data_nw_def <- data_nw_ruw

### eigenaarschap ---

# discriminatie: ldiscri_p
# invloed op de stad:  binvloed_p (alleen beschikbaar op SD-niveau)

hoog = 4
breed = 12
stadsdeel = 'Nieuw-West'
# invloed: alleen op gebiedsniveau voor 2019
data_nw_def |> 
  filter(
    spatial_type %in% c( 'stadsdelen', 'gemeente'))|>
  my_bar_plot(
    fill_var = temporal_date,
    var = c("binvloed_p"),
    jaar = NULL, 
    facet_var = tweedeling_def)


hoog = 4
breed = 12
# discriminatie (data uit veiligheidsmonitor)
data_nw_def |> 
  filter(
    spatial_code != 'GF06',
    tweedeling_def == 'totaal',
    spatial_type %in% c('gebieden','stadsdelen','gemeente'))|>
  my_bar_plot(
    var = c("ldiscri_p"), 
    jaar = c("2020"), 
    facet_var = temporal_date)

legenda_pos = 'bottom'
hoog  = 6
breed = 12
data_nw_def |>
  my_kaart_plot( 
    var = c("ldiscri_p"),
    geo = "wijken",
    afr = 0,
    jaar = c("2020"), 
    facet_var = temporal_date)

### Veiligehid ---

# zwerfafval grof (%) 
# voelt zich wel eens onveilig in eigen buurt (%) 
# geregistreerde criminatiliteitsindex 
# onveiligheidsbelevingsindex 
# gerapporteerd slachtofferschapindex 
# vermijdingsindex (onderdeel veiligheidsindex) 


# zwerfvuil : nu alleen op gebiedsniveau: merijn gaat data leveren op wijkniveau
hoog = 4
data_nw_def |> 
  filter(
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("or_grof_p"), c("2020"), temporal_date)

hoog = 6
breed = 12
data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "vveiligavond_r"  ~ "veiligheidsgevoel avond (1-10)",
    variabele == "vveiligvoelen_p" ~ "onveilig in buurt (%)")
    )|>
  my_kaart_plot( 
    var = c("vveiligavond_r","vveiligvoelen_p" ),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)

# objectieve veiligheid

# geregistreerde criminatiliteitsindex 
# gerapporteerd slachtofferschapindex 

breed = 10
hoog =5
legenda_pos = 'bottom'
data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "v_gercrim_i" ~ "criminaliteit (index)",
    variabele == "v_slacht_i"  ~ "slachtofferschap (index)")
  )|>
  my_kaart_plot( 
    var = c("v_gercrim_i", "v_slacht_i"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)

# subjectieve veiligheid

legenda_pos = 'bottom'
data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "v_vermijd_i"     ~ "vermijding (index)",
    variabele == "v_onvbeleving_i" ~ "onveiligheidsbeleving (index)")
  )|>
  my_kaart_plot( 
    var = c("v_onvbeleving_i", "v_vermijd_i"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)


# veiligheid winkelgebieden -

data_nw_def |>
  filter(
    variabele %in% c("bhwin_veil_dag_r" , "bhwin_veil_avond_r")
    )|>
  select(variabele, spatial_code, spatial_name, value)|>
  pivot_wider(values_from = value, names_from = variabele)|>
  write.xlsx("10 rapporten/04 rapporten Nieuw-West/figuren/tabel_winkelgebieden.xlsx")       


### GRIJS: Woning en woonomgevin

# ontwikkeling woningvoorraad (index, 2017 =  100) 
# - met eigen woning (1-10)
# - tevredenheid eigen buurt (1-10)
# - betrokkenheid bew. buurt (1-10)
# - omgang tussen groepen (1-10)
# - verbondenheid met de buurt (%)
# - aanbod winkels food (1-10) 


### waardering woning


hoog = 6
breed = 12
data_nw_def |>
  mutate(indicator_sd = case_when(
    
    variabele == "wwoning_r"     ~ "tevredenheid woning",
    variabele == "wwoningcorp_r" ~ "tevredenheid corporatiewoning",
    variabele == "lbuurt_r"      ~ "tevredenheid met buurt")
    
  )|>
  my_kaart_plot( 

    var = c("wwoning_r",  "wwoningcorp_r", "lbuurt_r"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = fct_relevel(
      indicator_sd, 
      "tevredenheid woning", "tevredenheid corporatiewoning", "tevredenheid met buurt"))


data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "lbetrokken_r"       ~ "betrokkenheid van bewoners",
    variabele == "lomganggroepenb_r"  ~ "omgang tussen groepen")
    )|>
  my_kaart_plot( 

    var = c("lbetrokken_r", "lomganggroepenb_r"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)

data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "lverbondenbuurt_p" ~ "verbondenheid met buurt",
    variabele == "bhwinkelaanbod_r"  ~ "winkelaanbod food")
    )|>
  my_kaart_plot( 
  
    var = c("lverbondenbuurt_p", "bhwinkelaanbod_r"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)



### GEEL: Kansen voor de jeugd ---

data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "w_krap_kind_p"  ~ "jongeren in krapwonende gezinnen",
    variabele == "iminjong130_p"  ~ "jongeren in minimahuishoudens")
  )|>
  my_kaart_plot( 
    var =  c("w_krap_kind_p", "iminjong130_p"), 
    geo = "wijken",
    afr = 0,
    jaar =   NULL, 
    facet_var = indicator_sd)

# bar met NEET
legenda_pos = 'bottom'
breed = 10
hoog = 6
data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "startkwal_zonder_p"  ~ "geen startkwalificatie (%)",
    variabele == "neet_1826_p"         ~ "geen werk of opleiding (%)")
  )|>
  my_kaart_plot( 
    var =  c("startkwal_zonder_p", "neet_1826_p"), 
    geo = "wijken",
    afr = 0,
    jaar =   NULL,
    facet_var = indicator_sd)


school <- c("ostrflez_p",  "ostrfrek_p",  "ostrftaal_p")

hoog = 4 
data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "ostrflez_p"   ~ "streefniveau lezen (%)",
    variabele == "ostrfrek_p"   ~ "streefniveau rekenen (%)",
    variabele == "ostrftaal_p"  ~ "streefniveau taal (%)")
    )|>
  filter(
    spatial_type %in% c( 'stadsdelen', 'gemeente'))|>
  my_line_plot(
    var =  school,
    facet_var = indicator_sd)


hoog = 5
data_nw_def |> 
  mutate(indicator_sd = case_when(
    variabele == "ostrflez_p"   ~ "streefniveau lezen (%)",
    variabele == "ostrfrek_p"   ~ "streefniveau rekenen (%)",
    variabele == "ostrftaal_p"  ~ "streefniveau taal (%)")
  )|>
  filter(
    spatial_name != 'Sloterdijk Nieuw-West',
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente')
    )|>
  my_bar_plot(
    var = school,
    jaar = c("2020"),
    fill_var = temporal_date,
    facet_var = indicator_sd)


# aantal starters	                                          bhstart
# aandeel starters (%)                                  	  bhstart_p
# aanbod winkels dagelijkse boodschappen (1-10)	            bhwinkelaanbod_r
# 
# aandeel inwoners met laag inkomen en weinig vermogen (%)	iminhh130_p
# aandeel inwoners 27 - 65 meest kwetsbare positie (%)	    sk2765_kwets34_p
# aandeel inwoners 65 + meest kwetsbare positie (%)	        sk66plus_kwets34_p
# aandeel inwoners meest kwetsbare positie (%)	            skkwets34_p


# bar met minimahuishoudens
data_nw_def |> 
  mutate(
    value= round(value))|>
  filter(
    spatial_type %in% c('gebieden', 'stadsdelen', 'gemeente'))|>
  my_bar_plot(var = c("iminhh130_p"), 
              jaar = c("2020"),
              fill_var = variabele,
              facet_var = temporal_date)


kwetsb <- c(
  #'kwetsbaar 0-17 (%)'  = "sk017_kwets34_p", 
  #'kwetsbaar 18-26 (%)' = "sk1826_kwets34_p" , 
  'kwetsbaar 27-65 (%)' = "sk2765_kwets34_p", 
  'kwetsbaar 66+ (%)'   = "sk66plus_kwets34_p",
  'kwetsbaar (%)'       = "skkwets34_p")


data_nw_def |>
  mutate(indicator_sd=case_when(
    
    variabele == 'skkwets34_p'        ~ 'kwetsbaar (%)',
    variabele == 'sk017_kwets34_p'    ~ 'kwetsbaar 0-17 (%)',
    variabele == 'sk1826_kwets34_p'   ~ 'kwetsbaar 18-26 (%)',
    variabele == 'sk2765_kwets34_p'   ~ 'kwetsbaar 27-65 (%)',
    variabele == 'sk66plus_kwets34_p' ~ 'kwetsbaar 65+ (%)')
    
    )|>
  filter(
    tweedeling_def == 'totaal',
    spatial_type %in% c('gebieden', 'stadsdelen', 'gemeente'))|>
  my_bar_plot(
    jaar = NULL,
    var = kwetsb,
    facet_var =  fct_relevel(indicator_sd, kwetsb ))

breed  = 14
hoog = 6

data_nw_def |>
  mutate(indicator_sd=case_when(
    variabele == 'skkwets34_p'        ~ 'kwetsbaar (%)',
    variabele == 'sk017_kwets34_p'    ~ 'kwetsbaar 0-17 (%)',
    variabele == 'sk1826_kwets34_p'   ~ 'kwetsbaar 18-26 (%)',
    variabele == 'sk2765_kwets34_p'   ~ 'kwetsbaar 27-65 (%)',
    variabele == 'sk66plus_kwets34_p' ~ 'kwetsbaar 65+ (%)')
  )|>
  my_kaart_plot(
    var  = kwetsb,
    geo  = "wijken", 
    afr  = 1,  
    jaar = NULL,
    facet_var = fct_relevel(indicator_sd, kwetsb )
    )




