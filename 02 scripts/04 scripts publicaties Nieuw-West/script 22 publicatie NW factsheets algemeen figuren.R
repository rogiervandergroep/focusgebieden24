


# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/04 scripts publicaties Nieuw-West/script 22 publicatie NW factsheets algemeen data.R")
source("02 scripts/04 scripts publicaties Nieuw-West/script 20 publicatie NW functies.R")


data_nw_def <- data_nw_ruw

# FA           Sloterdijk Nieuw-West
# FB FC FD FE  Geuzenveld Slotermeer
# FF FH FJ FK  Osdorp
# FL FM FN FP  Slotervaart
# FG FQ        De Aker SLoten Nieuw-Sloten

geb_levels <- c(
  'Sloterdijk Nieuw-West',
  'Geuzenveld, Slotermeer',
  'Osdorp',
  'De Aker, Sloten, Nieuw-Sloten',
  'Slotervaart')





### eigenaarschap ---

# discriminatie: ldiscri_p
# invloed op de stad:  binvloed_p (alleen beschikbaar op SD-niveau)

hoog = 4
breed = 12
# invloed: alleen op gebiedsniveau voor 2019
data_nw_def |> 
  filter(
    spatial_type %in% c( 'stadsdelen', 'gemeente'))|>
  my_bar_plot(
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


data_nw_def |>
  mutate(indicator_sd = case_when(
    variabele == "v_vermijd_i"     ~ "vermijding (index)",
    variabele == "v_onvbeleving_i" ~ "onveiligeidsbeleving (index)")
  )|>
  my_kaart_plot( 
    var = c("v_onvbeleving_i", "v_vermijd_i"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)




breed = 12
data_nw_def |>
  my_kaart_plot( 
    var = c("v_gercrim_i", "v_slacht_i"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = indicator_sd)








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
    variabele == "wwoning_r" ~ "tevredenheid over woning",
    variabele == "lbuurt_r"  ~ "tevredenheid met buurt")
  )|>
  my_kaart_plot( 
    var = c("wwoning_r", "lbuurt_r"),
    geo = "wijken",
    afr = 1,
    jaar =  NULL,
    facet_var = fct_rev(indicator_sd))


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
# vooralsnog niet gebruikt - staat als wel primair
# aandeel dat enige vorm van jeugdhulp gebruikt (leeftijd 0 t/m 5) (%)	  jeugdhulp_0tm5_p
# aandeel dat enige vorm van jeugdhulp gebruikt (leeftijd 12 t/m 17) (%)	jeugdhulp_12tm17_p
# aandeel dat enige vorm van jeugdhulp gebruikt (leeftijd 18 t/m 22) (%)	jeugdhulp_18tm22_p
# aandeel dat enige vorm van jeugdhulp gebruikt (leeftijd 6 t/m 11) (%)	  jeugdhulp_6tm11_p
# toevoegen aandeel dat ooit gebruik heeft gemaakt van jeugdhulp?


# percentage jongeren (16 t/m/ 26) zonder werk of opleiding (NEET)	neet_jongeren_p

# aandeel leerlingen van groep 8 dat streefniveau lezen haalt (%)	  ostrflez_p
# aandeel leerlingen van groep 8 dat streefniveau rekenen haalt (%)	ostrfrek_p
# aandeel leerlingen van groep 8 dat streefniveau taal haalt (%)	  ostrftaal_p

# aandeel jongeren in gezin dat krap woont (%)	w_krap_kind_p

# onderadvisering leerlingen (%)	otoetsh_p
# overadvisering leerlingen (%) 	otoetsl_p

# aandeel jongeren (18-26) met startkwalificatie, totaal (%)	startkwalificatie_p

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



data_nw_def |>
  my_kaart_plot( 
    var =  c("neet_jongeren_p"), 
    geo = "buurten",
    jaar =   c("2018", "2020") , 
    facet_var = temporal_date)



jeugdhulp <- c(
  "jeugdhulp_0tm5_p",
  "jeugdhulp_6tm11_p",
  "jeugdhulp_12tm17_p")

hoog = 7

data_nw_def |>
  filter(
    spatial_code != 'FA', 
    spatial_code != 'FF',
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(jeugdhulp, NULL, variabele)

hoog = 5
# line met NEET
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente")
    )|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "werkgebieden",
    TRUE                            ~ "stadsdeel en stad")
    )|>
  my_line_plot("neet_jongeren_p", facet)


# bar met NEET
data_nw_def |>
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("neet_jongeren_p"), c("2020"), temporal_date)



# bar met startkwalificatie : alleen 2019 en 2021
data_nw_def |> 
  filter(
  spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("startkwalificatie_p"), c("2019"), temporal_date)

data_nw_def |>
  my_kaart_plot( 
    var =  c("startkwalificatie_p", "neet_jongeren_p"), 
    geo = "buurten",
    jaar =   NULL,
    facet_var = indicator_sd)

breed = 7
hoog = 8

data_nw_def |>
  my_kaart_plot( 
    var =  jeugdhulp,
    geo = "wijken",
    jaar =  NULL,
    facet_var = variabele)





school <- c("ostrflez_p",  "ostrfrek_p",  "ostrftaal_p")

data_nw_def |>
  filter(value!=0)|>
  my_kaart_plot( 
    var =  school,
    geo = "wijken",
    jaar =  NULL,
    facet_var = variabele)

hoog = 5 
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("ostrflez_p", fct_rev(facet))

data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("ostrftaal_p", fct_rev(facet))

data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("ostrfrek_p", fct_rev(facet))

toets <- c("otoetsh_p","otoetsl_p")

data_nw_def |>
  filter(value!=0)|>
  my_kaart_plot( 
    var =  toets,
    geo = "wijken",
    jaar =  NULL,
    facet_var = indicator_sd)

# 
# aantal starters	                                          bhstart
# aandeel starters (%)                                  	  bhstart_p
# aanbod winkels dagelijkse boodschappen (1-10)	            bhwinkelaanbod_r
# 
# aandeel inwoners met laag inkomen en weinig vermogen (%)	iminhh130_p
# aandeel inwoners 27 - 65 meest kwetsbare positie (%)	    sk2765_kwets34_p
# aandeel inwoners 65 + meest kwetsbare positie (%)	        sk66plus_kwets34_p
# aandeel inwoners meest kwetsbare positie (%)	            skkwets34_p
# 



data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("ihhink_gem", fct_rev(facet))

	
# lijn met uitkeringen 
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("iwwb_p", fct_rev(facet))



# lijn met uitkeringen 
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("pwerklbbv", fct_rev(facet))


data_nw_def |>
  filter(value!=0)|>
  mutate(value = case_when(
    variabele == 'ihhink_gem' ~ value/1000,
    TRUE ~ value))|>
  my_kaart_plot( 
    var = c("ihhink_gem","iwwb_p"),
    geo = "buurten",
    jaar =  NULL,
    facet_var = fct_rev(indicator_sd))




# bar met laag inkomen 
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("iminhh130_p"), c("2020"), temporal_date)

# bar met kwetsbare minderjarigen
data_nw_def |> 
  filter(
    spatial_name != 'Lutkemeer/Ookmeer',
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("sk017_kwets34_p"), c("2020"), temporal_date)


data_nw_def |>
  my_kaart_plot( 
    var = c("bhstart","bhstart_p"),
    geo = "buurten",
    jaar =  NULL,
    facet_var = fct_rev(indicator_sd))



# bar rapportcijfer winkelaanbod
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("bhwinkelaanbod_r"), c("2019", "2021"), temporal_date)

# bar met kwetsbare minderjarigen
data_nw_def |> 
  filter(
    spatial_name != 'Lutkemeer/Ookmeer',
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("sk017_kwets34_p"), c("2020"), temporal_date)






hoog = 6
# lijn met uitkeringen 
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("vveiligvoelen_p", fct_rev(facet))

