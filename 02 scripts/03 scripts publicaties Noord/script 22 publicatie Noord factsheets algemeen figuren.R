

# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/03 scripts publicaties Noord/script 22 publicatie Noord factsheets algemeen data.R")
source("02 scripts/01 scripts bewerking data/script 50 functies voor kaarten.R")

breed = 12
hoog  = 8

# algemeen kaartje met wijknamen
legenda_pos = 'bottom'
aantal_kolommen = 4  # aantal kolommen in legenda
stadsdeel = 'Noord'
my_kaart("Noord")

# focusbuurten in Noord
focusbuurten <- c(
  "NA07",  "NC01",  "NC02",  "NC03",  "NE01",  "NE02",  "NE03",  "NE04",
  "NH02",  "NH05",  "NJ02",  "NJ03",  "NJ05",  "NJ06",  "NK01",  "NK02",
  "NK03",  "NL01",  "NL03",  "NL04",  "NM01",  "NN01",  "NN02"
  )


# algemeen kaartje met focusbuurten
aantal_kolommen = 4
# legenda_pos = 'bottom'
# my_focus_buurt_kaart( "Noord")


###################################
data_noord_def <- data_noord_def |>
  ungroup()
###################################

################################################
### h1 kansen voor de jeugd: kernindicatoren ---
################################################

# jongeren in minimahuishoudens (130% sm en weinig vermogen) (%) (kernindicator)
# jongeren (18 t/m 26) zonder werk of opleiding (%) (kernindicator)
# leerlingen van groep 8 dat streefniveau taal haalt (%) (kernindicator)
# bewoners van 30 - 47 maanden met een VVE indicatie (voor en vroegschoolse educatie) (%) (kernindicator)
# jongeren (18-26) zonder startkwalificatie, totaal (%) (kernindicator)

h1_kernind <- c(
  "startkwal_zonder_p","ostrftaal_p","ovveindi_p",
  "neet_1826_p","iminjong130_p")

breed = 12
hoog = 4

#lijndiagrammen
data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "startkwal_zonder_p" ~ 'jongeren zonder startkwal. (%)',
    variabele == "neet_1826_p"        ~ 'jongeren zonder werk of opl. (%)',
    variabele == "iminjong130_p"      ~ 'jongeren in minimahuishoudens (%)',
    TRUE                              ~ indicator_sd))|>
  my_line_plot_noord(c("startkwal_zonder_p","neet_1826_p","iminjong130_p"))


test <- data_noord_def |>
  filter(variabele == "ostrftaal_p") |>
  select(-c("temporal_date", "value"))|>
  distinct(.keep_all = T)|>
  add_column(
    temporal_date = '2019',
    value = NA)
            
bind_rows(data_noord_def,test) |>
  filter(temporal_date %in% c("201": "2023"  ))|>
  mutate(indicator_sd = case_when(
    variabele == "ostrftaal_p" ~ 'streefniveau taal (%)',
    variabele == "ovveindi_p"  ~ 'VVE-indicatie (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot_noord(c("ovveindi_p", "ostrftaal_p") )





#kaarten op buurtniveau

h1_kernind |>
  map(\(x) my_kaart_plot_noord(data_noord_def, x))

##########################################
### HOOFDSTUK 2: kansrijke sociale buurten
##########################################

# oordeel tevredenheid met eigen buurt (1-10) (kernindicator)



# inwoners met (zeer) goede gezondheid (%)
# inwoners met ernstige psychische klachten (%) (kernindicator)
# minimahuishoudens (130% sm en weinig vermogen) (%) (kernindicator)
# inwoners meest kwetsbare positie (%) (kernindicator)

h2_kansrijk<- c(
  "lbuurt_r","wzgezond_p", "wzdepr_p",
  "iminhh130_p", "skkwets34_p")

breed = 12
hoog = 5
data_noord_def |>
  filter(temporal_date %in% c("2017": "2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "skkwets34_p"  ~ 'meest kwetsbaar (%)',
    variabele == "iminhh130_p"  ~ 'minimahuishoudens (%)',
    TRUE ~ indicator_sd))|>
  mutate(indicator_sd = factor(indicator_sd, levels = c("minimahuishoudens (%)", "meest kwetsbaar (%)")))|>
  my_line_plot_noord(c("skkwets34_p", "iminhh130_p"))

### buurtindicatoren
# lbetrokken_r
# lomganggroepenb_r
# lbuurt_r

# oordeel betrokkenheid buurtbewoners met de buurt (1-10)
# oordeel omgang tussen groepen bewoners (1-10)
# oordeel tevredenheid met eigen buurt (1-10)

data_noord_def |>
  filter(temporal_date %in% c("2017": "2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "lbetrokken_r"      ~ 'betrokkenheid buurtbewoners buurt (1-10)',
    variabele == "lomganggroepenb_r" ~ 'omgang tussen groepen bewoners (1-10)',
    variabele == "lbuurt_r"          ~ 'tevredenheid met eigen buurt (1-10)',
    TRUE ~ indicator_sd)
    )|>
  my_line_plot_noord(c("lbetrokken_r", "lomganggroepenb_r", "lbuurt_r"), ymin = 4, ymax = 10)

data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "wzgezond_p" ~ 'goede gezondheid (%)',
    variabele == "wzdepr_p"   ~ 'psychische klachten (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot_noord(c("wzgezond_p","wzdepr_p"))

h2_kansrijk_p <- c(
  "wzgezond_p",  "wzdepr_p",
  "iminhh130_p", "skkwets34_p")

#kaarten op buurtniveau

h2_kansrijk_p |>
  map(\(x) my_kaart_plot_noord(data_noord_def, x))

data_noord_def |>
  my_kaart_plot_noord(c("lbuurt_r"), afr = 1)

data_noord_def |>
  my_kaart_plot_noord(c("lomganggroepenb_r"), afr = 1)

data_noord_def |>
  my_kaart_plot_noord(c("lbetrokken_r"), afr = 1)







##########################
### Hoofdstuk 3: Wonen ---
##########################

# oordeel tevredenheid met eigen woning (1-10) (kernindicator)
# oordeel onderhoud eigen corporatiewoning (1-10) (kernindicator)
# woningen met een energielabel van E , F of G (%) (kernindicator)
# woningen ongeïsoleerd (%) (kernindicator)

h2_wonen <- c(
  "wwoning_r",
  "wonderhoudwoning_r",
  "wlabelefg_p",
  "dnietiso_p"
)

breed = 12
hoog = 4
data_noord_def |>
  filter(temporal_date %in% c("2017": "2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "wwoning_r"          ~ 'tevredenheid met eigen woning (1-10)',
    variabele == "wonderhoudwoning_r" ~ 'onderhoud eigen corporatiewoning (1-10)',
    variabele == "wlabelefg_p"        ~ 'woningen energielabel E,F of G (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot_noord(c("wwoning_r","wonderhoudwoning_r"), ymin = 4, ymax=10)

# nb : woningen niet geisoleerd is geen data beschikbaar op buurtniv.

data_noord_def |>
  filter(temporal_date %in% c("2017": "2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "wwoning_r"          ~ 'tevredenheid met eigen woning (1-10)',
    variabele == "wonderhoudwoning_r" ~ 'onderhoud eigen corporatiewoning (1-10)',
    variabele == "wlabelefg_p"        ~ 'woningen energielabel E,F of G (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot_noord(c("wlabelefg_p"), ymax=25)



breed = 9
hoog = 5

data_noord_def |>
  my_kaart_plot_noord(c("wwoning_r"), afr = 1)

data_noord_def |>
  my_kaart_plot_noord(c("wonderhoudwoning_r"), afr = 1)

data_noord_def |>
  my_kaart_plot_noord(c("wlabelefg_p"), afr = 0)


### ontwikkeling huur, particullier en koop

data_noord_ruw |>
  filter(
    spatial_type == 'buurten',
    spatial_code != 'NFOCUS',
    spatial_code != 'NOVERIG',
    variabele %in% c("wkoop", "wcorhuur", "wparthuur")) |>
  group_by(temporal_date,indicator_sd, focusbuurt)|>
  summarise(totaal= sum(value, na.rm = T))|>
  ggplot(aes(
    y = totaal, 
    x = as.character(temporal_date), 
    label = totaal,
    fill = indicator_sd)) +
  geom_col(aes(
    fill = indicator_sd), position = 'dodge') +
  labs(title = NULL , x = NULL, y = NULL) +
  theme_os2(
    orientation = "vertical",
    legend_position = 'bottom') + 
  scale_fill_manual(name = NULL, values = wild_pal) +
  guides(color = guide_legend( reverse = F, nrow = 3))+
  facet_wrap(~ focusbuurt)
ggsave(filename = glue::glue(
  "10 rapporten/03 rapporten Noord/figuren/bar_eigendom.svg"),
  device = "svg", width = 12, height = 5)



###############################
### Hoofdstuk 3: veiligheid ---
###############################

# oordeel veiligheidsgevoel 's avonds (1-10) (kernindicator)
# verloederingsscore (index) (kernindicator)
# oordeel onderhoud straten en stoepen (1-10) (kernindicator)

h3_wonen <- c(
  "vveiligavond_r",
  "v_verloed_i",
  "oronderhoudstraten_r"
)


breed = 12
hoog = 4
data_noord_def |>
  filter(temporal_date %in% c("2017": "2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "vveiligavond_r"        ~ "veiligheidsgevoel 's avonds (1-10)",
    #variabele == "v_verloed_i"           ~ 'verloederingsscore (index)',
    variabele == "oronderhoudstraten_r"  ~ 'onderhoud straten en stoepen (1-10)',
    TRUE ~ indicator_sd))|>
  my_line_plot_noord(c("vveiligavond_r", "oronderhoudstraten_r" ), ymin = 4, ymax = 10)


breed = 9
hoog = 5
data_noord_def |>
  my_kaart_plot_noord(c("vveiligavond_r"), afr = 1)

data_noord_def |>
  my_kaart_plot_noord(c("oronderhoudstraten_r"), afr = 1)
 
###############################
### Hoofdstuk 4: groen ---
###############################

# publiek groen (%) (kernindicator)
# boomkroon publieke ruimte (%) (kernindicator)
# fiets- of loopafstand tot parkachtig groen (m) (kernindicator)
# natuurlijk groen (%) (kernindicator)

h5_groen <- c(
  "orpubgroen_p",
  "opp_boomkroon_publiek_p",
  "orgafspark_m",
  "orntrgroen_p"
)

# nb: slechts een observatie dus geen zin om trendlijnen te laten zien
breed = 8
hoog = 4
data_noord_def |>
  my_kaart_plot_noord(c("orpubgroen_p"))

data_noord_def |>
  my_kaart_plot_noord(c("opp_boomkroon_publiek_p"))

data_noord_def |>
  my_kaart_plot_noord(c("orbuurtgroen_p"))

data_noord_def |>
  my_kaart_plot_noord(c("orntrgroen_p"))

data_noord_def |>
  my_kaart_plot_noord(c("org_afsgroen_m"))
###############################
### Hoofdstuk 5: 
###############################

# vestigingen 
# zzp'ers (%) 
# oordeel aanbod winkels dagelijkse boodschappen (1-10) 
# oordeel horeca-aanbod (1-10) 

# vestigingen zzp en starters opnieuw uitrekenen


breed = 12
hoog = 4
data_noord_def |>
  filter(temporal_date %in% c("2017":"2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "bhvest"            ~ "vestigingen",
    variabele == "bhzzp_p"           ~ "zzp'ers (%)",
    TRUE ~ indicator_sd)
    )|>
  my_line_plot_noord(c("bhvest","bhzzp_p"))

data_noord_def |>
  filter(temporal_date %in% c("2017":"2023"))|>
  mutate(indicator_sd = case_when(
    variabele == "bhwinkelaanbod_r"  ~ 'oordeel winkels DG (1-10)',
    variabele == "bhhorecaaanbod_r"  ~ 'oordeel horeca-aanbod (1-10)',
    TRUE ~ indicator_sd)
  )|>
  my_line_plot_noord(c("bhwinkelaanbod_r", "bhhorecaaanbod_r"), ymin = 2, ymax = 10)



data_noord_def |>
  my_kaart_plot_noord(c("bhvest"))

data_noord_def |>
  my_kaart_plot_noord(c("bhzzp_p"))

data_noord_def |>
  my_kaart_plot_noord(c("bhwinkelaanbod_r"), 1)

data_noord_def |>
  my_kaart_plot_noord(c("bhhorecaaanbod_r"), 1)


