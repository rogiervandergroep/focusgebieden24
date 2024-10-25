
# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/02 scripts publicaties Zuidoost/script 22 publicatie ZO rapport eenmeting basis.R")


# wijken en het gebied waar ze toe behoren 
tabel_wijk <- os_get_geom("wijken")|>
  filter(stadsdeelCode == 'T') |>
  sf::st_drop_geometry()|>
  select(naam, code, gebiedNaam, gebiedCode)

# volgorde van de gebieden 
geb_levels <- tabel_wijk |> 
  arrange(gebiedCode)|>
  select(gebiedNaam)|>
  distinct()|>
  pull()
 
### kaarten demografie

# bar ontwikkeling bevolking en woningvoorraad

# vanwege enorme groei (index inwoners: 581 ; index woningvoorraad: 717) in Amstel III/Bullenwijk (TA) uit de data gehaald -
data_zo_def |>
  filter(
    spatial_code != 'TA',
    spatial_type %in% c('wijken',  'stadsdelen', 'gemeente'))|>
    my_bar_plot( 
      var =  c("bevtotaal_groei", "wvoorrbag_groei"), 
      facet_var = indicator_sd)


# gestapelde staaf bevolking
data_zo_def |>
  filter(
  spatial_type %in% c('gebieden'))|>
  my_stack_plot("bevtotaal")

# kaart bevolking
data_zo_def |>
  my_kaart_plot(
    var = c("bevtotaal"), 
    geo = "wijken", 
    afr = 0,  
    jaar = c("2019", "2021"),  
    facet_var = temporal_date)

# kaart kinderen en 65_plus
data_zo_def |>
  my_kaart_plot(
    var = c("bevhhmkind_p", "bev66plus_p"), 
    geo = "buurten", 
    afr = 0,  
    jaar = NULL, 
    facet_var = indicator_sd)


#lijn inkomensontwikkeling

data_zo_def |>
  filter(spatial_type %in% c("gemeente", 'stadsdelen', 'gebieden'))|>
  my_bar_plot(
    var = c("ihhink_gem"),
    jaar = 2020,
    facet_var = temporal_date)
      




### SD 1: Inclusie en participatie

# voel me geaccepteerd in Zuidoost zoals ik ben (%) acczo_p
# Heb genoeg mogelijkheden om mee te doen in maatschappij (%) meemij_p
# Heb voldoende mogelijkheden om mening te geven over ontwikkelingen in mijn buurt (%) menontwbrt_p

hoog = 4
breed = 12

data_zo_def |>
  filter(spatial_type %in% c( 'stadsdelen', 'gebieden'))|>
  my_bar_plot(
    var  = c("acczo_p"),
    jaar = NULL,
    facet_var = fct_rev(tweedeling_def))

data_zo_def |>
  filter(spatial_type %in% c( 'stadsdelen', 'gebieden'))|>
  my_bar_plot(
    var  = c("meemij_p"),
    jaar = NULL,
    facet_var = fct_rev(tweedeling_def))

data_zo_def |>
  filter(spatial_type %in% c( 'stadsdelen', 'gebieden'))|>
  my_bar_plot(
    var  = c("menontwbrt_p"),
    jaar = NULL,
    facet_var = fct_rev(tweedeling_def))

hoog = 6
breed = 14
data_zo_def |>
  mutate(
    variabele2 = str_replace_all(variabele, "acczo_p", "geaccepteerd (%)"),
    variabele2 = str_replace_all(variabele2, "meemij_p", "meedoen (%)"),
    variabele2 = str_replace_all(variabele2, "menontwbrt_p", "mening buurt (%)")
  )|>
  my_kaart_plot(
    var = c("acczo_p", 'meemij_p', 'menontwbrt_p' ), 
    geo = "wijken", 
    afr = 0,  
    jaar = NULL,
    facet_var = variabele2)


### SD2 Veilig en prettig wonen

# Tevredenheid met eigen woning (rapportcijfer)
hoog=5
data_zo_def |>
  filter(
    tweedeling_def == 'totaal',
    spatial_type %in% c('gebieden',  'stadsdelen', 'gemeente'))|>
  my_bar_plot(
    c("lbuurt_r"), 
    jaar = c("2019", "2021"), 
    facet_var =  temporal_date )

# Onveiligheidsbeleving (index): geen amsterdam en ZO?
hoog = 6
breed = 12
data_zo_def |>
  my_kaart_plot(
    var = c( "lbuurt_r" ), 
    geo = "wijken", 
    afr = 1,  
    jaar = c("2019", "2021", "2023"),
    facet_var = temporal_date)

## check amsterdam en zuidoost ontbreekt!
hoog = 6
breed = 12
data_zo_def |>
  my_kaart_plot(
    var = c( "v_onvbeleving_i" ), 
    geo = "wijken", 
    afr = 0,  
    jaar = c("2021", "2023"),
    facet_var = temporal_date)

# SD 3:In Zuidoost opgroeien en wonen betekent dat je je eenvoudig fysiek kunt verplaatsen
# in een schoon, heel en veilige openbare ruimte

data_zo_def |>
  my_kaart_plot(
    var =  c("v_verloed_i"), 
    geo = "wijken", 
    afr = 1,  
    jaar = "2021", 
    facet_var = temporal_date)


data_zo_def |>
  my_kaart_plot(
    var =  c("vkov_r"), 
    geo = "wijken", 
    afr = 1,  
    jaar = c( "2019","2021"),
    facet_var = temporal_date)

# SD4 Sociale basis
# Inwoner zijn van Zuidoost betekent dat je zelfredzaam bent en mee doet, gefaciliteerd via een laagdrempelige sociale basis in de wijk.

# Inwoners met problematische schulden (zelfrapportage) (%) : problem_schuld_p : geen update
# Inwoners met laag inkomen en weinig vermogen (%) : iminhh130_p: alleen 2021: geen update
# Minderjarigen met laag inkomen en weinig vermogen (%) : IMINJONG130_P: alleen 2021 - geen update


data_zo_def |>
  filter(
    tweedeling_def == 'totaal',
    spatial_type %in% c('stadsdelen', 'gemeente'))|>
  my_line_plot(
    var = c("skkwets34_p", "sk2765_kwets34_p"),
    facet_var =  indicator_sd )







data_zo_def |>
  my_kaart_plot(
    var =  c( "iminhh130_p", "skkwets34_p"),
    geo = "buurten", 
    afr = 1,  
    jaar = NULL,
    facet_var = indicator_sd)

data_zo_def |>
  filter(spatial_type %in% c('stadsdelen', 'gemeente', 'gebieden'))|>
  my_line_plot("skkwets34_p", spatial_type)


# Meest kwetsbaar (%)
# Meest kwetsbaar (% 0-17)
# Meest kwetsbaar (% 18-26)
# Meest kwetsbaar (% 27-65)

kwetsb <- c(
  'kwetsbaar (%)'       = "skkwets34_p", 
  'kwetsbaar 0-17 (%)'  = "sk017_kwets34_p", 
  'kwetsbaar 18-26 (%)' = "sk1826_kwets34_p" , 
  'kwetsbaar 27-65 (%)' = "sk2765_kwets34_p", 
  'kwetsbaar 66+ (%)'   = "sk66plus_kwets34_p")

data_zo_def |>
  filter(spatial_type %in% c('stadsdelen', 'gemeente'))|>
  my_line_plot(kwetsb, variabele)


# Weinig regie over eigen leven (%) : geen update meer
# Hoge veerkracht (%) : geen update






# Informele hulp (%): iminhh130_p (uitr SvdS)
# Bereik minimaregelingen (%): bminreg_p : armoedemonitor
# Bereik minimaregelingen minderjarigen (%): bminregjong_p : armoedemonitor


# Aanbod buurthuizen /wijkcentra (rapportcijfer) wzbuurthuizen_r
# Aanbod zorgvoorzieningen (rapportcijfer) wzzorgvoorzieningen_r



# Strategisch doel: 
# In Zuidoost opgroeien en wonen betekent dat je alle kansen en 
# mogelijkheden hebt om je te ontwikkelen, te groeien en talenten te ontplooien

# Deelname voorschool (%)
# Streefniveau Lezen (%)
# Streefniveau Rekenen (%)
# Streefniveau Taal (%)
# Onderadvies (%)
# Afstroom aandeel 3e jaar VO (%)
# Jongeren met werk of opleiding (%)
# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES laag)
# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES midden)
# Startkwalificatie (18 t/m 26 jaar, (% met ouders SES hoog)
# Jeugdhulp 0 t/m 5-jarigen (%)
# Jeugdhulp 6 t/m 11-jarigen (%)
# Jeugdhulp 12 t/m 17-jarigen (%)
# Jeugdhulp 18 t/m 22-jarigen (%)

# Werk & economie Strategisch doel: 
# In Zuidoost zijn we trots op onze positie als economische toplocatie, 
# op ons ondernemerschap en creativiteit en investeren we wederkerig daarin en in werk voor onze inwoners.


# Werkend of in opleiding (%)
# Werkenden met maximaal een mbo 1 opleiding (%)
# Werkenden met mbo 2 t/m 4, havo, vwo-opleiding (%)
# Werkenden met hbo-/wo opleiding (%)
# Werkloosheid (actief op zoek) (18 t/m 66) (%)
# Uitkeringsgerechtigden (%)
# Terugkeer in de bijstand (%)
# Flexibel dienstverband (%)
# Werkenden zuidoost, woonachtig in Zuidoost (%)
# Starters gedeeld door opheffingen
# Werkenden werkzaam als ondernemer (18 t/m 66) (%)


# Kunst & cultuur 
# Strategisch doel: In Zuidoost professionaliseren we de infrastructuur van kunst en cultuur 
# met als doel het verbreden en verdiepen van het aanbod

# Culturele participatie (%)
# Banen creatieve industrie (%)
# Vestigingen creatieve industrie (%)