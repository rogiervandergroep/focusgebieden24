


# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/03 scripts publicaties Noord/script 22 publicatie Noord factsheets algemeen data.R")
source("02 scripts/03 scripts publicaties Noord/script 20 publicatie Noord functies.R")

data_noord_def <- data_noord_def |>
  ungroup()

# h1 kansen voor de jeugd: kernindicatoen

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
    TRUE ~ indicator_sd))|>
  my_line_plot(c("startkwal_zonder_p","neet_1826_p","iminjong130_p"))

data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "ostrftaal_p" ~ 'streefniveau taal (%)',
    variabele == "ovveindi_p"  ~ 'bew. 30-47 mnd. met VVE-ind.',
    TRUE ~ indicator_sd))|>
  my_line_plot(c("ostrftaal_p","ovveindi_p"))

#kaarten op buurtniveau
breed = 9
hoog = 5
h1_kernind |>
  map(\(x) my_plot_ind_alg(data_noord_def, x))

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
hoog = 4
data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "lbuurt_r"     ~ 'tevredenheid eigen buurt (0-10)',
    variabele == "skkwets34_p"  ~ 'meest kwetsbaar (%)',
    variabele == "iminhh130_p"  ~ 'minimahuishoudens (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot(c("lbuurt_r","skkwets34_p", "iminhh130_p"))

data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "wzgezond_p" ~ 'goede gezondheid (%)',
    variabele == "wzdepr_p"   ~ 'psychische klachten (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot(c("wzgezond_p","wzdepr_p"))

h2_kansrijk_p <- c(
  "wzgezond_p",  "wzdepr_p",
  "iminhh130_p", "skkwets34_p")

#kaarten op buurtniveau
breed = 9
hoog = 5
h2_kansrijk_p |>
  map(\(x) my_plot_ind_alg(data_noord_def, x))

data_noord_def |>
  my_plot_ind_alg(c("lbuurt_r"), afr = 1)

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
  mutate(indicator_sd = case_when(
    variabele == "wwoning_r"          ~ 'tevredenheid met eigen woning (1-10)',
    variabele == "wonderhoudwoning_r" ~ 'onderhoud eigen corporatiewoning (1-10)',
    variabele == "wlabelefg_p"        ~ 'woningen energielabel E,F of G (%)',
    TRUE ~ indicator_sd))|>
  my_line_plot(c("wwoning_r","wonderhoudwoning_r", "wlabelefg_p" ))

# nb : woningen niet geisoleerd is geen data beschikbaar op buurtniv.

breed = 9
hoog = 5
data_noord_def |>
  my_plot_ind_alg(c("wwoning_r"), afr = 1)

data_noord_def |>
  my_plot_ind_alg(c("wonderhoudwoning_r"), afr = 1)

data_noord_def |>
  my_plot_ind_alg(c("wlabelefg_p"), afr = 0)


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
  mutate(indicator_sd = case_when(
    variabele == "vveiligavond_r"        ~ "veiligheidsgevoel 's avonds (1-10)",
    variabele == "v_verloed_i"           ~ 'verloederingsscore (index)',
    variabele == "oronderhoudstraten_r"  ~ 'onderhoud straten en stoepen (1-10)',
    TRUE ~ indicator_sd))|>
  my_line_plot(c("vveiligavond_r","v_verloed_i", "oronderhoudstraten_r" ))


breed = 9
hoog = 5
data_noord_def |>
  my_plot_ind_alg(c("vveiligavond_r"), afr = 1)

data_noord_def |>
  my_plot_ind_alg(c("oronderhoudstraten_r"), afr = 1)
 
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

breed = 9
hoog = 5
data_noord_def |>
  my_plot_ind_alg(c("orpubgroen_p"))

data_noord_def |>
  my_plot_ind_alg(c("opp_boomkroon_publiek_p"))

data_noord_def |>
  my_plot_ind_alg(c("orgafspark_m"))

data_noord_def |>
  my_plot_ind_alg(c("orntrgroen_p"))


###############################
### Hoofdstuk 5: 
###############################

# vestigingen 
# zzp'ers (%) 
# oordeel aanbod winkels dagelijkse boodschappen (1-10) 
# oordeel horeca-aanbod (1-10) 


breed = 12
hoog = 4
data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "bhvest"            ~ "vestigingen",
    variabele == "bhzzp_p"           ~ "zzp'ers (%)",
    TRUE ~ indicator_sd)
    )|>
  my_line_plot(c("bhvest","bhzzp_p"))

data_noord_def |>
  mutate(indicator_sd = case_when(
    variabele == "bhwinkelaanbod_r"  ~ 'oordeel winkels DG (1-10)',
    variabele == "bhhorecaaanbod_r"  ~ 'oordeel horeca-aanbod (1-10)',
    TRUE ~ indicator_sd)
  )|>
  my_line_plot(c("bhwinkelaanbod_r", "bhhorecaaanbod_r"  ))


breed = 9
hoog = 5
data_noord_def |>
  my_plot_ind_alg(c("bhvest"))

data_noord_def |>
  my_plot_ind_alg(c("bhzzp_p"))

data_noord_def |>
  my_plot_ind_alg(c("bhwinkelaanbod_r"), 1)

data_noord_def |>
  my_plot_ind_alg(c("bhhorecaaanbod_r"), 1)


