


# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/04 scripts publicaties Nieuw-West/script 22 publicatie NW factsheets algemeen data.R")

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



### kaarten demografie

# bar ontwikkeling bevolking en woningvoorraad
data_nw_def |>
  filter(
    spatial_code != 'FA',
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
    my_bar_plot( 
      var =  c("bevtotaal_groei", "wvoorrbag_groei"), 
      facet_var = indicator_sd)


# gestapelde staaf bevolking
data_nw_def |>
  filter(
  spatial_type %in% c('gebieden'))|>
  my_stack_plot("bevtotaal")

# kaart bevolking
data_nw_def |>
  my_kaart_plot(c("bevtotaal"), "wijken", c("2019", "2021"), temporal_date)

# kaart kinderen en 65_plus
data_nw_def |>
  my_kaart_plot(c("bevhhmkind_p", "bev66plus_p"), "buurten", NULL, facet_var = indicator_sd)


# bar corporatiehuur
data_nw_def |>
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("wcorhuur_p"), c("2019", "2021"), temporal_date)

### GRIJS: Woning en woonomgevin

# sociale cohesie (schaal obv 4 items) 	lsoccoh_r
# verbondheid met de buurt (%)	        lverbondenbuurt_p
# tevredenheid met eigen woning (1-10)	wwoning_r
# mening te geven over ontwikkelingen in mijn buurt (%)	menontwbrt_p
# oordeel aanbod ov onvoldoende-voldoende (1-10)	vkov_r
# duur wonen whuurquote
# aandeel woningen met een energielabel 	wlabelefg_p
# onderhoud eigen corporatiewoning (1-10)	wonderhoudwoning_r




### waardering woning
data_nw_def |>
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("wwoning_r"), c("2019", "2021"), temporal_date)

# kaart krap wonen totaal en jogneren
data_nw_def |>
  filter(value>0)|>
  my_kaart_plot(c("w_krap_p", "w_krap_kind_p"), "buurten", NULL, facet_var = indicator_sd)





w_krap_kind_p

### onderhoud corporatiewoning
data_nw_def |>
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("wonderhoudwoning_r"), c("2019", "2021"), temporal_date)


# kaart energielabel
data_nw_def |>
  my_kaart_plot(c("wlabelefg_p"), "wijken", c("2019", "2021"), temporal_date)


# kaart energielabel
data_nw_def |>
  filter(!is.na(value),
         value!= 0)|>
  my_kaart_plot(c("whuurquote"), "wijken", NULL, NULL)

data_nw_def |>
  my_bar_plot(c("whuurquote"),  c("2019", "2021"), temporal_date)

# bar sociale cohesie
data_nw_def |>
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("lsoccoh_r"), c("2019", "2021"), temporal_date)

### "lverbondenbuurt_p" en "wzeenzj_p" (eenzaam) alleen op sd-niveau --


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
# bar sociale cohesie
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

breed = 14
hoog = 7

data_nw_def |>
  my_kaart_plot( 
    var =  c("w_krap_kind_p"), 
    geo = "buurten",
    jaar =   c("2021") , 
    facet_var = temporal_date)

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


### eigenaarschap ---






# invloed: alleen op gebiedsniveau voor 2019
data_nw_def |> 
  filter(
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("binvloed_p"), c("2018", "2019","2020", "2021"), temporal_date)


# inzet buurt (uit SvdS) alleen gebieden
data_nw_def |> 
  filter(
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("pinzetbrt_p"), c("2018", "2019","2020", "2021"), temporal_date)



# omgang: alleen op gebiedsniveau voor 2019
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("lomganggroepenb_r"), c("2018", "2019","2020", "2021"), temporal_date)



# discriminatie (data uit veiligheidsmonitor)
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("ldiscri_p"), c( "2021"), temporal_date)

data_nw_def |>
  my_kaart_plot( 
    var = c("ldiscri_p" , "lomganggroepenb_r"),
    geo = "wijken",
    jaar =  NULL,
    facet_var = indicator_sd)


# Veiligehid

# zwerfvuil : nu alleen op gebiedsniveau: merijn gaat data leveren op wijkniveau
data_nw_def |> 
  filter(
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("or_grof_p"), c("2019"), temporal_date)


# veilig voelen (data uit veiligheidsmonitor)
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("vveiligvoelen_p"), c( "2021"), temporal_date)




hoog = 6
# lijn met uitkeringen 
data_nw_def |>
  filter(
    spatial_type %in% c('gebieden', "stadsdelen", "gemeente"))|>
  mutate(facet = case_when(
    spatial_type %in% c("gebieden") ~ "gebieden",
    TRUE                            ~ "stadsdeel en stad"))|>
  my_line_plot("vveiligvoelen_p", fct_rev(facet))


# jeugdige verdachten
data_nw_def |> 
  filter(
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("vjeugdige_p"), c( "2021"), temporal_date)


# jeugdige verdachten
data_nw_def |> 
  filter(
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(c("vveiligavond_r"), c( "2021"), temporal_date)


data_nw_def |>
  my_kaart_plot( 
    var = c("vveiligavond_r"),
    geo = "buurten",
    afr = 1,
    jaar =  c("2019"),
    facet_var = temporal_date)

