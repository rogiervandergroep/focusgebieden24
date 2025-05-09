
# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/02 scripts publicaties Zuidoost/script 22 publicatie ZO eenmeting basis.R")
source("02 scripts/01 scripts bewerking data/script 50 functies voor kaarten.R")

# overzichtskaart Zuidoost -

breed = 10
hoog = 8
aantal_kolommen = 1
legenda_pos = 'right'
stadsdeel = 'Zuidoost'
# my_kaart("Zuidoost")

# overzicht oude versus nieuwe bewoners
# bewoners van 18 jaar en ouder die niet op 1-1-2021 in het stadsdeel wonen (nieuwe bewoners) (%)
hoog = 6
breed = 12

data_zo_def2 <- data_zo_def |>
  filter(
    temporal_date %in% c('2020', '2024'),
    variabele == 'wvoorrbag')|>
  group_by(spatial_code, spatial_name, spatial_type)|>
  mutate(value     = (value-lag(value))/lag(value)*100,
         variabele = 'wvoorrbag_groei_p',
         indicator_sd = 'woningvoorraad (20 - 24) (%)')|>
  filter(temporal_date == '2024',
         value != 'Inf',
         !is.na(value))
  


# kaart oude vs nieuwe bewoners en ontwikkeling woningvoorraad
legenda_pos= 'bottom'
bind_rows (data_zo_def,data_zo_def2) |>
  mutate(indicator_sd = case_when(
    variabele == 'bew_nieuw' ~ 'nieuwe bewoners (%)',
    TRUE                     ~ indicator_sd)
    )|>
  my_kaart_plot(
    var = c("bew_nieuw", 'wvoorrbag_groei_p'), 
    geo = "wijken", 
    afr = 1,  
    jaar = NULL, 
    facet_var = indicator_sd) 


##################################
### SD 1: Inclusie en participatie
##################################

# voel me geaccepteerd in Zuidoost zoals ik ben (%) acczo_p
# Heb genoeg mogelijkheden om mee te doen in maatschappij (%) meemij_p
# Heb voldoende mogelijkheden om mening te geven over ontwikkelingen in mijn buurt (%) menontwbrt_p

hoog = 4
breed = 12

# staafdiagran
data_zo_def |>
  
  mutate(indicator_sd = case_when(
    variabele == "acczo_p"      ~ "geaccepteerd (%)",
    variabele == "meemij_p"     ~ "meedoen maatschappij (%)",
    variabele == "menontwbrt_p" ~ "mening ontw. buurt (%)")
    )|>
      
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  filter(
    spatial_type %in% c( 'gebieden', 'stadsdelen'))|>
  my_bar_plot(
    var  = c("acczo_p", "meemij_p", "menontwbrt_p"),
    fill = temporal_date,
    jaar = NULL,
    facet_var = indicator_sd)

# De mensen van het stadsdeel 
# - zijn eerlijk (%) mwzoeerl_p
# - behandelen mij goed (%) mwzogobeh_p
# - nemen mj serieus, er wordt naar mij geluisterd (%) mwzoluist_p
# - lossen problemen in mijn buurt goed op (%) mwzoprbrt_p

hoog = 6

data_zo_def |>
  
  mutate(indicator_sd = case_when(
    variabele == "mwzoeerl_p"      ~ "... zijn eerlijk (%)",
    variabele == "mwzogobeh_p"     ~ "... behandelen mij goed (%)",
    variabele == "mwzoluist_p"     ~ "... luisteren naar mij (%)",
    variabele == "mwzoprbrt_p"     ~ "... lossen problemen op (%)")
    )|>
  
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  filter(
    spatial_type %in% c( 'gebieden', 'stadsdelen'))|>
  my_bar_plot(
    var  = c("mwzoeerl_p", "mwzogobeh_p", "mwzoluist_p", "mwzoprbrt_p"),
    jaar = NULL,
    fill = temporal_date,
    facet_var = indicator_sd)

# Het stadsdeel houdt rekening met:
# - mensen met fysieke beperkingen  zoinwfb_p
# - mensen die niet goed Nederlands kunnen lezen  zoinwngnl_p
# - mensen die niet goed Nederlands kunnen spreken zoinwngns_p
# - verschillende culturen zoinwvc_p
# - verschillende seksuele voorkeuren zoinwvsv_p

data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "zoinwfb_p"    ~ "... met fysieke beperkingen (%)",
    variabele == "zoinwngnl_p"  ~ "... die niet nederlands lezen (%)",
    variabele == "zoinwngns_p"  ~ "... die niet nederlands spreken (%)",
    variabele == "zoinwvc_p"    ~ "... van verschillende culturen (%)",
    variabele == "zoinwvsv_p"   ~ "... seksuele voorkeuren (%)")
  )|>
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  filter(
    spatial_type %in% c( 'gebieden', 'stadsdelen'))|>
  my_bar_plot(
    var  = c("zoinwfb_p", "zoinwngnl_p", "zoinwngns_p", "zoinwvc_p", "zoinwvsv_p"),
    jaar = NULL,
    fill = temporal_date,
    facet_var = indicator_sd)


# kaarten
data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "acczo_p"      ~ "geaccepteerd (%)",
    variabele == "meemij_p"     ~ "meedoen maatschappij (%)",
    variabele == "menontwbrt_p" ~ "mening ontw. buurt (%)")|>
  my_kaart_plot(
    var = c("acczo_p", 'meemij_p', 'menontwbrt_p' ), 
    geo = "wijken", 
    afr = 0,  
    jaar = NULL,
    facet_var = indicator_sd)



# kernindicatoren 

# betrokkenheid buurtbewoners met de buurt (1-10) (kernindicator)
# omgang tussen groepen bewoners (1-10) (kernindicator)

# staafdiagran

hoog = 5
breed = 12
data_zo_def |>
  mutate(
    variabele2 = str_replace_all(variabele, "lbetrokken_r", "betrokkenheid (%)"),
    variabele2 = str_replace_all(variabele2, "lomganggroepenb_r", "omgang groepen(%)")
  )|>
  mutate(value = case_when(
    is.na(value) ~ 0,
    TRUE ~ value))|>
  filter(
    spatial_type %in% c( 'gebieden', 'stadsdelen'))|>
  my_bar_plot(
    var  = c("lbetrokken_r", "lomganggroepenb_r"),
    jaar = c("2021"),
    fill_var = temporal_date,
    facet_var = variabele2)

hoog = 6
breed = 12
data_zo_def |>
  mutate(
    variabele2 = str_replace_all(variabele, "lbetrokken_r", "betrokkenheid (%)"),
    variabele2 = str_replace_all(variabele2, "lomganggroepenb_r", "omgang groepen(%)")
  )|>
  my_kaart_plot(
    var = c("lbetrokken_r", "lomganggroepenb_r"), 
    geo = "buurten", 
    afr = 1,  
    jaar = NULL, 
    facet_var = variabele2) 

# verbondenheid buurt en stadsdeel -
aantal_kolommen = 2
hoog = 5
breed = 14


data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "lverbondst_p"      ~ "verbond. stadsdeel (%)",
    variabele == "lverbondenbuurt_p" ~ "verbond. buurt (%)")
  )|>
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  filter(
    (spatial_type == 'gemeente' & tweedeling_def == 'totaal') | 
      spatial_type %in% c('gebieden', 'stadsdelen'))|>
  mutate(indicator_sd = factor(
    indicator_sd, levels = c(
      "verbond. stadsdeel (%)", 
      "verbond. buurt (%)"))
    )|>
  my_bar_plot(
    var  = c("lverbondst_p", "lverbondenbuurt_p"),
    jaar = NULL,
    fill_var = fct_rev(temporal_date),
    facet_var = indicator_sd)



###############################
### SD2 Veilig en prettig wonen
###############################


data_zo_def |>
  filter(spatial_type %in% c( 'stadsdelen', 'gebieden',  'gemeente'))|>
  my_bar_plot(
    var  = c("wwoning_r"), 
    jaar = c("2021"), 
    facet_var =  temporal_date )

# nb: indexen kunnen niet vergeleken worden met stadsdelen: zijn andere cijfers 
hoog = 4
data_zo_def |>
  filter(
    #spatial_name != 'Bijlmer-West',
    spatial_type %in% c('stadsdelen', 'gebieden',  'gemeente'))|>
  my_bar_plot(
    var  = c("vveiligvoelen_p" , "lbuurt_r"),
    jaar = c("2021"),
    fill_var = fct_rev(temporal_date),
    facet_var =  indicator_sd, schaal = 'free_x')

hoog = 5
breed = 12

legenda_pos = 'bottom'

data_zo_def |>
  filter(
    spatial_name != 'Bijlmer-West')|>
  my_kaart_plot(
    var = c("lbuurt_r"), 
    geo = "wijken", 
    afr = 1,  
    jaar = 2021,
    facet_var = temporal_date)

data_zo_def |>
  filter(
    spatial_name != 'Bijlmer-West')|>
  my_kaart_plot(
    var = c("vveiligvoelen_p"), 
    geo = "wijken", 
    afr = 1,  
    jaar = 2021,
    facet_var = temporal_date)



# SD 3:In Zuidoost opgroeien en wonen betekent dat je je eenvoudig fysiek kunt verplaatsen
# in een schoon, heel en veilige openbare ruimte

# zwerfafval grof (% geen of weinig) (kernindicator)
# oordeel aanbod speelvoorzieningen (0-10) (kernindicator)
# oordeel aanbod parkeervoorzieningen (1-10) (kernindicator)


hoog = 6
breed = 12
data_zo_def |>
  filter(spatial_type %in% c('stadsdelen', 'gebieden',  'gemeente'))|>
  mutate(indicator_sd = case_when(
    variabele == "or_grof_p"        ~ 'weinig zwerfvuil (%)',
    variabele == "oraanbodspelen_r" ~ 'aanbod speelvoorz. (0-10)',
    variabele == "vkparkeren_r"     ~ 'aanbod parkeervoorz. (0-10)')
    )|>
  my_bar_plot(
    var  = c("or_grof_p", "oraanbodspelen_r", "vkparkeren_r"),
    jaar = c("2021"), 
    fill_var = fct_rev(temporal_date),
    facet_var =  indicator_sd,
    schaal = 'free_x')



hoog = 6
breed = 8

data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "or_grof_p"        ~ 'weinig zwerfvuil (%)',
    variabele == "oraanbodspelen_r" ~ 'aanbod speelvoorz. (0-10)',
    variabele == "vkparkeren_r"     ~ 'aanbod parkeervoorz. (0-10)')
  )|>
  my_kaart_plot(
    var = c("oraanbodspelen_r"), 
    geo = "buurten", 
    afr = 1,  
    jaar = NULL,
    facet_var = indicator_sd)

hoog = 5
breed = 10

legenda_pos = 'bottom'

# kaart op buurtniv voor spelen  en parkeren 
data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "or_grof_p"        ~ 'weinig zwerfvuil (%)',
    variabele == "oraanbodspelen_r" ~ 'aanbod speelvoorz. (0-10)',
    variabele == "vkparkeren_r"     ~ 'aanbod parkeervoorz. (0-10)')
  )|>
  my_kaart_plot(
    var = c("oraanbodspelen_r", "vkparkeren_r"), 
    geo = "buurten", 
    afr = 1,  
    jaar = NULL,
    facet_var = indicator_sd)

# kaart op wijkniveau grofvuil
data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == "or_grof_p"        ~ 'weinig zwerfvuil (%)',
    variabele == "oraanbodspelen_r" ~ 'aanbod speelvoorz. (0-10)',
    variabele == "vkparkeren_r"     ~ 'aanbod parkeervoorz. (0-10)')
  )|>
  my_kaart_plot(
    var = c("or_grof_p"), 
    geo = "wijken", 
    afr = 1,  
    jaar = c("2021"),
    facet_var = temporal_date)


# SD4 Sociale basis
# Inwoner zijn van Zuidoost betekent dat je zelfredzaam bent en mee doet, gefaciliteerd via een laagdrempelige sociale basis in de wijk.

# Inwoners met problematische schulden (zelfrapportage) (%) : problem_schuld_p : geen update
# Inwoners met laag inkomen en weinig vermogen (%) : iminhh130_p: alleen 2021: geen update
# Minderjarigen met laag inkomen en weinig vermogen (%) : IMINJONG130_P: alleen 2021 - geen update

hoog = 4
breed = 9

data_zo_def |>
  mutate(indicator_sd=case_when(
    variabele == 'skkwets34_p'      ~ 'kwetsbaar (%)',
    variabele == 'sk2765_kwets34_p' ~ 'kwetsbaar 27-65 (%)',
    TRUE ~ indicator_sd))|>
  filter(
    spatial_type %in% c('stadsdelen', 'gemeente'))|>
  my_line_plot(
    var = c("skkwets34_p", "sk2765_kwets34_p"),
    facet_var =  fct_rev(indicator_sd ))



data_zo_def |>
  mutate(indicator_sd=case_when(
    variabele == 'skkwets34_p'      ~ 'kwetsbaar (%)',
    variabele == 'sk2765_kwets34_p' ~ 'kwetsbaar 27-65 (%)',
    TRUE ~ indicator_sd)
  )|>  

  my_kaart_plot(
    var  =  c(  "skkwets34_p", "sk2765_kwets34_p"),
    geo  = "wijken", 
    afr  = 1,  
    jaar = NULL,
    facet_var = indicator_sd)

# Meest kwetsbaar (%)
# Meest kwetsbaar (% 0-17)
# Meest kwetsbaar (% 18-26)
# Meest kwetsbaar (% 27-65)

kwetsb <- c(
  'kwetsbaar (%)'       = "skkwets34_p", 
  'kwetsbaar 0-17 (%)'  = "sk017_kwets34_p", 
 # 'kwetsbaar 18-26 (%)' = "sk1826_kwets34_p" , 
  'kwetsbaar 27-65 (%)' = "sk2765_kwets34_p", 
  'kwetsbaar 66+ (%)'   = "sk66plus_kwets34_p")
hoog = 5
data_zo_def |>
  mutate(indicator_sd=case_when(
    
    variabele == 'skkwets34_p'        ~ 'kwetsbaar totaal (%)',
    variabele == 'sk017_kwets34_p'    ~ 'kwetsbaar 0-17 (%)',
    # variabele == 'sk1826_kwets34_p'   ~ 'kwetsbaar 18-26 (%)',
    variabele == 'sk2765_kwets34_p'   ~ 'kwetsbaar 27-65 (%)',
    variabele == 'sk66plus_kwets34_p' ~ 'kwetsbaar 66+ (%)',
    TRUE ~ indicator_sd)
    )|>
  filter(
    spatial_name != 'Bijlmer-West', 
    spatial_type %in% c('stadsdelen', 'gebieden', 'gemeente')
    )|>
  my_bar_plot(
    var = kwetsb, 
    fill_var = temporal_date,
    facet_var = indicator_sd)

# Weinig regie over eigen leven (%) : geen update meer
# Hoge veerkracht (%) : geen update

# waar terecht bij problemen

# Weet waar respondent terecht kan (%) (kernindicator)
# - bij burenruzie
# - met financiële problemen 
# - met problemen gezondheid
# - onveiligheid op straat
# - overlast op straat
# - met juridische problemen 
# - met problemen woning

# terecht kunnen bij burenruzie (%)	          tpburen_p
# terecht kunnen met financiele problemen (%)	tprfinan_p
# terecht kunnen met juridische problemen (%)	tprpjurid_p
# terecht kunnen met problemen gezondheid (%)	tprgezond_p
# terecht kunnen met problemen woning (%)	    tprwoning_p
# terecht kunnen onveiligheid op straat (%)	  tpronvstr_p
# terecht kunnen overlast op straat (%)	      tprovstr_p

# 'tpronvstr_p',

problemen <- c(
  'tprburen_p',
  'tprfinan_p',
  'tprpjurid_p',
  'tprgezond_p',
  'tprwoning_p',

  'tprovstr_p')

# staafdiagran
data_zo_def |>
  
  mutate(variabele2 = case_when(
    variabele == 'tprburen_p'  ~ 'problemen met buren',
    variabele == 'tprfinan_p'  ~ 'financiële problemen',
    variabele == 'tprpjurid_p' ~ 'juridische problemen',
    variabele == 'tprgezond_p' ~ 'gezondheidsproblemen',
    variabele == 'tprwoning_p' ~ 'problemen met de woning',
    variabele == 'tpronvstr_p' ~ 'onveiligheid op straat',
    variabele == 'tprovstr_p'  ~ 'overlast op straat')
  )|>
  
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  filter(
    spatial_type %in% c( 'gebieden', 'stadsdelen'))|>
  my_bar_plot(
    var  = problemen,
    jaar = NULL,
    fill_var = temporal_date,
    facet_var = variabele2)

#### Strategisch doel 5: 
# In Zuidoost opgroeien en wonen betekent dat je alle kansen en 
# mogelijkheden hebt om je te ontwikkelen, te groeien en talenten te ontplooien

# schooladvies: Praktijkonderwijs, VMBO-B of VMBO-K (%) (kernindicator)
# leerlingen van groep 8 dat streefniveau taal haalt (%) (kernindicator)
# jongeren (18 t/m 26) met werk of opleiding (%) (kernindicator)

# o_vmbot_p
# ostrftaal_p
# "werkopl_1826_p"

# barplots op gebiedsniveau


hoog = 5
breed = 12

data_zo_def |>
  filter(
    # spatial_name != 'Bijlmer-West',
    spatial_type %in% c('stadsdelen', 'gebieden',  'gemeente'))|>
  mutate(indicator_sd=case_when(
    variabele == "o_vmbot_p"   ~ "Praktijkonderwijs, VMBO-B of VMBO-K (%)",
    variabele == "ostrftaal_p" ~ "streefniveau taal (%)")
  )|>
  my_bar_plot(
    var  = c("o_vmbot_p","ostrftaal_p"),
    jaar = c("2020"), 
    fill_var  = fct_rev(temporal_date),
    facet_var = fct_rev(indicator_sd))


### oud vs nieuw ###
hoog = 5
data_zo_def |>
  filter(
    # spatial_name != 'Bijlmer-West',
    spatial_type %in% c('stadsdelen', 'gebieden'))|>
  
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  
  my_bar_plot(
    var  = c("werkopl_1826_p", "startkwalificatie_p" ),
    jaar = NULL,
    fill_var =  temporal_date,
    facet_var =  indicator_sd )










# Streefniveau Rekenen (%)
# Streefniveau Taal (%)
# Onderadvies (%)
# Afstroom aandeel 3e jaar VO (%)
# Jongeren met werk of opleiding (%)

data_zo_def |>
  my_kaart_plot(
    var =  c("werkopl_1826_p", ), 
    geo = "wijken", 
    afr = 1,  
    jaar = c( "2020", "2021"),
    facet_var = temporal_date)



# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES laag) startkwalificatie_seslaag_p
# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES midden) startkwalificatie_sesmidden_p
# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES hoog) startkwalificatie_seshoog_p

breed = 12
hoog = 4
data_zo_def |>
  filter(
    (spatial_type == 'gemeente' & tweedeling_def == 'totaal') | 
      spatial_type %in% c('gebieden', 'stadsdelen'))|>
  mutate(indicator_sd = case_when(
    variabele == "startkwalificatie_seslaag_p"   ~ 'ses laag (%)',
    variabele == "startkwalificatie_sesmidden_p" ~ 'ses midden (%)',
    variabele == "startkwalificatie_seshoog_p"   ~ 'ses hoog (%)',
    variabele == "startkwalificatie_p"           ~ 'totaal (%)')
    )|>
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  mutate(indicator_sd = factor(indicator_sd, levels = c(
    'ses laag (%)', 'ses midden (%)', 'ses hoog (%)', 'totaal (%)'))
  )|>
  my_bar_plot(
    var =  c(
      "startkwalificatie_seslaag_p",  
      "startkwalificatie_sesmidden_p", 
      "startkwalificatie_seshoog_p",
      "startkwalificatie_p"), 
    facet_var = indicator_sd,
    jaar = c("2021"),
    fill_var = fct_rev(temporal_date))
    

# variabele == "werkopl_1826_p"                ~ '18-26 met werk of opl. (%)',


#### Werk & economie Strategisch doel: 
# In Zuidoost zijn we trots op onze positie als economische toplocatie, 
# op ons ondernemerschap en creativiteit en investeren we wederkerig daarin en in werk voor onze inwoners.

# kernindicatoren
# 15 tot en met 65-jarigen dat WWB ontvangt (%) (kernindicator)
# werkloosheid (actief op zoek) (18 t/m 66) (%) (kernindicator)
# werkend of in opleiding (%) (kernindicator)

# iwwb_p
# pwerklbbv_p
# werkopl_1866_p

# kernindicatoren op gebiedsniveau

hoog = 5
data_zo_def |>
  filter(tweedeling_def == 'totaal') |>
  filter(
    spatial_type %in% c('stadsdelen', 'gebieden', 'gemeente')
    )|>
  my_bar_plot(
    var  = c("iwwb_p", "terugkeer_bijstand_p"),
    jaar = c("2020"), 
    fill_var = fct_rev(temporal_date),
    facet_var =  indicator_sd)




# Werkend of in opleiding (%)
breed = 10
hoog = 6
data_zo_def |>
  mutate(indicator_sd = case_when(
    variabele == 'iwwb_p' ~ 'ontvangt WWB (%)',
    TRUE ~ indicator_sd)
  )|>
  my_kaart_plot(
    var =  c("werkopl_1866_p", "iwwb_p"), 
    geo = "wijken", 
    afr = 1,  
    jaar = NULL,
    facet_var = indicator_sd)



#### SD8: Kunst & cultuur 
# Strategisch doel: In Zuidoost professionaliseren we de infrastructuur van kunst en cultuur 
# met als doel het verbreden en verdiepen van het aanbod

# Culturele participatie (%) 

data_zo_def |>
  filter(spatial_type %in% c( 'stadsdelen', 'gebieden')
  )|>
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  my_bar_plot(
    var  = c("sruit4_p"),
    jaar = NULL,
    facet_var = fct_rev(tweedeling_def))

# Banen creatieve industrie (%)
# Vestigingen creatieve industrie (%)

# Kunst en cultuur verbindt verschillend groepen bewoners
data_zo_def |>
  filter(
    (spatial_type == 'gebieden' & tweedeling_def == 'totaal') | spatial_type == 'stadsdelen')|>
  mutate(spatial_name = case_when(
    spatial_type == 'stadsdelen' ~ glue::glue("{spatial_name} ({tweedeling_def})"),
    TRUE ~ spatial_name)
  )|>
  mutate(indicator_sd = case_when(
    variabele == 'kcverbbew_p' ~ 'kunst verbindt bewoners (%)',
    variabele == 'kctonten_p'  ~ 'tevredenheid kunst Zuidoost (%)',
    TRUE ~ variabele))|>
  my_bar_plot(
    var  = c("kcverbbew_p", "kctonten_p"),
    jaar = NULL,
    facet_var = fct_rev(indicator_sd))









#### 