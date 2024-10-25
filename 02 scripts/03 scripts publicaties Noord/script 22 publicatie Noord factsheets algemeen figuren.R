
# "bevtotaal_groei"     "bhlocoppleegstand"   "bhlocoppleegstand_p" "bhlocvkpleegstand"   "bhlocvkpleegstand_p"
# "bhstart"             "bhstart_p"           "bhwinkelaanbod_r"    "ihhbstkind_p"        "iminhh130_p"        
# "lsoccoh_r"           "olsv_p"              "ostrftaal_p"        
# "ovveindi_p"          "pwerklbbv_p"         "v_verloed_i"        
# "werkopl_1826_p"      "werkopl_1866_p"      "wzgezond_p"    "wzzwaar_p"   

# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/03 scripts publicaties Noord/script 22 publicatie Noord factsheets algemeen data.R")

### kaarten demografie

# barplot ontwikkeling bevolking en woningvoorraad
data_noord_def |>
  my_bar_plot_alg(c("bevtotaal_groei", "wvoorrbag_groei"), NULL)+
  facet_wrap(~ indicator_sd)
ggsave("20 figuren rapport noord/bar_demo.svg", width = 12, height = 7)

# barplot corporatiehuur
data_noord_def |>
  my_bar_plot_alg(c("wcorhuur_p"),c("2019", "2021"))+
  facet_wrap(~ temporal_date, nrow=1)
ggsave("20 figuren rapport noord/fig_wcorhuur_p.svg", width = 12, height = 7)

# kaart huishoudens en kinderen
data_noord_def |>
  my_plot_ind_alg(c("bevhhmkind_p"), "2020")+
  facet_wrap(~ temporal_date, ncol=1)
ggsave("20 figuren rapport noord/fig_bevhhmkind_p.svg", width = 14, height = 7)


### waardering woning

# waardering eigen woning
data_noord_def |>
  my_bar_plot_alg( c( "wwoning_r"), c("2019", "2021"))+
  facet_wrap(~ fct_rev(indicator_sd))+
  facet_wrap(~ temporal_date, nrow=1)
ggsave("20 figuren rapport noord/fig_rap_woning.svg", width = 12, height = 7)    




# waardering woning en onderhoud corp. woning
data_noord_def |>
  my_bar_plot_alg( c( "wonderhoudwoning_r"), c("2019", "2021"))+
  facet_wrap(~ fct_rev(indicator_sd))+
  facet_wrap(~ temporal_date, nrow=1)
ggsave("20 figuren rapport noord/fig_rap_woning_ond.svg", width = 12, height = 7)    

# kaart energielabel
data_noord_def |>
  my_plot_ind_alg(c("wlabelefg_p"))
ggsave("20 figuren rapport noord/fig_wlabelefg_p.svg", width = 14, height = 7)


# waardering buurt  
data_noord_def |>
  my_bar_plot_alg( c("lbuurt_r"), c("2019", "2021"))+
  facet_wrap(~ temporal_date, nrow=1)
ggsave("20 figuren rapport noord/fig_rap_buurt.svg", width = 12, height = 7)    

# kaart veiligheid buurt
data_noord_def|>
  my_plot_ind_alg(c("vveiligavond_r")) 
ggsave("20 figuren rapport noord/fig_noord_veiligavond_r.svg", width = 14, height = 7) 

# kaart verloedering
### alleen data beschikbaar op wijkniveau


# waardering groen
fig_groen <- data_noord_def |>
  my_bar_plot_alg(c("orpubgroen_p", "orrecgroen_p"))+
  facet_wrap(~ indicator_sd)
ggsave("20 figuren rapport noord/fig_rap_groen.svg", width = 12, height = 7)    

# gemiddeld besteedbaar inkomen 
data_noord_def|>
  my_plot_ind_alg(c("ihhink_gem"), NULL) 
ggsave("20 figuren rapport noord/fig_noord_ihhink_gem.svg", width = 12, height = 7) 


# aandeel laag inkomen
data_noord_def |>
  my_bar_plot_alg(c("iminhh130_p"), c("2020"))+
  facet_wrap(~ temporal_date, nrow=1)
ggsave("20 figuren rapport noord/fig_noord_iminhh130_p.svg", width = 12, height = 7)

data_noord_def |>
  my_plot_ind_alg(c("skkwets34_p", "sk017_kwets34_p"), NULL)+
  facet_wrap(~ indicator_sd, nrow = 2)
ggsave("20 figuren rapport noord/fig_noord_skkwets34_p.svg", width= 12, height = 10)

### gezondheid

data_noord_def |>
  my_bar_plot_alg(c("wzgezond_p", "wzdepr_p"), NULL)+
  facet_wrap(~ fct_rev(indicator_sd))
ggsave("20 figuren rapport noord/fig_noord_wzgezond_p.svg", width = 12, height = 7)


### school 
data_noord_def |>
  my_bar_plot_alg(c("ostrftaal_p"), c("2018", '2020'))+
  facet_wrap(~ temporal_date)
ggsave("20 figuren rapport noord/fig_noord_ostrftaal_p.svg", width = 12, height = 7)

# vve indicatie
data_noord_def |>
  mutate(indicator_sd=case_when(
    indicator_sd ==
      'aandeel bewoners van 30 - 47 maanden met een VVE indicatie (voor en vroegschoolse educatie) (%)' ~ 
      'aandeel met VVE indicatie (%)',
    indicator_sd ==
    'aandeel bewoners van 30 - 47 maanden met een VVE indicatie dat deelneemt aan VVE (%)' ~
    'aandeel met VVE indicatie dat deelneemt aan VVE (%)',
    TRUE ~ indicator_sd)
  )|>
  my_bar_plot_alg(c("ovvedeeln_p", "ovveindi_p"), NULL)+
  facet_wrap(~ indicator_sd)
ggsave("20 figuren rapport noord/fig_noord_ovvedeeln_p.svg", width = 12, height = 7)

# startkwalificatie lage startkwallificatie
data_noord_def |>
  my_bar_plot_alg(c("startkwalificatie_p", "startkwalificatie_seslaag_p"), NULL)+
  facet_wrap(~ fct_rev(indicator_sd))
ggsave("20 figuren rapport noord/fig_noord_startkwalificatie_p.svg", width = 12, height = 7)


# vestiginggroei en starteer
data_noord_def |>
  my_bar_plot_alg(c( "bhzzp_p", "bhvest_groei"), NULL)+
  facet_wrap(~ fct_rev(indicator_sd), scales = 'free_x')
ggsave("20 figuren rapport noord/fig_noord_bhvest_groei.svg", width = 12, height = 7)

data_noord_def |>
  my_bar_plot_alg(c("werkopl_1826_p", "werkopl_1866_p"), NULL)+
  facet_wrap(~ fct_rev(indicator_sd))
ggsave("20 figuren rapport noord/fig_noord_werkopl_1866_p.svg", width = 12, height = 7)

data_noord_def |>
  my_bar_plot_alg(c("bhhorecaaanbod_r", "bhwinkelaanbod_r"), NULL)+
  facet_wrap(~ indicator_sd)
ggsave("20 figuren rapport noord/fig_noord_bhhorecaaanbod_r.svg", width = 12, height = 7)







