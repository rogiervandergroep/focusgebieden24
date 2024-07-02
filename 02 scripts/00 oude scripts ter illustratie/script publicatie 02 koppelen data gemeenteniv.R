
library(tidyverse)
library(openxlsx)

load("05 tussentijds/mra_data_gemcijfers.RData")




### Row bind data
meta_totaal <- bind_rows(files_meta)

###koppelen thema en onderwerp aan data
meta_thema<- meta_totaal%>%
  select(thema, 'ambitie/strategisch.doel', variabele, label, rekeneenheid_tekst, bron)

function_thema <- function(x) {
  
   x<- x %>%
    left_join(meta_thema, by= "variabele")
  
}

files_data<- map(files_data, function_thema)

### netjes ordenen kolommen
functie_kol_netjes <- function(x) {
  
  x<- x %>%
    relocate("thema", "ambitie/strategisch.doel", "gebied_niveau", 
             "gebied_code", "gebied_naam",  "peiljaar_gebiedsindeling",  
              "variabele","label","jaar", "waarde", 'rekeneenheid_tekst')
  
  }

files_data<- map(files_data, functie_kol_netjes)


data_totaal <- bind_rows(files_data)

# pivot wider data
data_wide_jaar<-data_totaal%>%
  arrange(jaar)%>%
  select(-peiljaar_gebiedsindeling)%>%
  pivot_wider(names_from = jaar, values_from = waarde, values_fill = NA)

# kolommen schrappen
# @Marloes: label in pivot wider! variabele eruit halen
# pivot_wider(...,-var)
data_wide_var<-data_totaal%>%
  arrange(jaar)%>%
  select(-c(thema, `ambitie/strategisch.doel`, rekeneenheid_tekst))%>%
  pivot_wider(names_from = variabele, values_from = waarde)

# wegschrijven van verschillende formats
write.xlsx(data_wide_var,  "06 output files/dataset_MAZO_wide_var.xlsx", overwrite = T)
write.xlsx(data_wide_jaar, "06 output files/dataset_MAZO_wide_jaar.xlsx", overwrite = T)
write.xlsx(data_totaal,    "06 output files/dataset_MAZO_long.xlsx", overwrite = T)


# wegschrijven met ambitie per tabblad
data_totaal_amb<- split(data_totaal, data_totaal$`ambitie/strategisch.doel`)


# wegschrijven met ambitie per sheet -
write.xlsx(data_totaal_amb,    "06 output files/dataset_MAZO_per_sheet.xlsx", overwrite = T)

# wegschrijven metadata
write.xlsx(meta_totaal,    "06 output files/metadata_MAZO_totaal.xlsx", overwrite = T)

### filter op Zuidoost ---
data_ZO <- data_totaal %>%
  filter(gebied_code %in% c("AMS", 'T','GT21', 'GT22', 'GT23', 'GT24', 'DX20, DX21, DX22') |
         grepl("T", gebied_code))

write.xlsx(data_ZO,    "06 output files/dataset_MAZO_long_FILTERZO.xlsx", overwrite = T)


### 24 mei 2023 Bestand jeugd en onderwijs uitdraaien 

data_jeugd <- data_totaal %>% 
  select(thema,`ambitie/strategisch.doel`,gebied_niveau,gebied_naam,variabele,label,jaar,waarde,rekeneenheid_tekst,bron) %>% 
  filter(variabele %in% c("bminregjong_p","bminregjong_p", 
                          "iminjong120_p",
                          "jeugdhulp_0tm5",
                          "jeugdhulp_12tm17",
                          "jeugdhulp_18tm22",
                          "jeugdhulp_23plus",
                          "jeugdhulp_6tm11",
                          "o_afstroom_p",
                          "o_onderadvies_p",
                          "o_streefniveau_l",
                          "o_streefniveau_r",
                          "o_streefniveau_t",
                          "o_streefniveau_l",
                          "oraanbodspelen_r",
                          "orgroen_r",
                          "sk017_kwets34_p",
                          "sk1826_kwets34_p", 
                          "srsportgelegenheden_r", 
                          "startkwal_1826_seshoog_p",
                          "startkwal_1826_seslaag_p",
                          "startkwal_1826_sesmidden_p", 
                          "startkwal_1826_sesonbekend_p",
                          "werkopl_1826_p"))
  
data_jeugd_ZO <- data_jeugd %>%
  filter(gebied_naam == "Zuidoost" | gebied_naam == "Amsterdam") %>% 
  select(gebied_niveau,gebied_naam,thema,`ambitie/strategisch.doel`,variabele,label,jaar,waarde,rekeneenheid_tekst,bron)

write.xlsx(data_jeugd,    "06 output files/dataset_MPZO_jeugd.xlsx", overwrite = T)
write.xlsx(data_jeugd_ZO,    "06 output files/dataset_jeugd_ZO.xlsx", overwrite = T)

### 19 oktober mei 2023 Bestand stadsdelen uitdraaieb 

data_MPZO_stadsdelen <- data_totaal %>% 
  select(thema,`ambitie/strategisch.doel`,gebied_niveau,gebied_naam,variabele,label,jaar,waarde,rekeneenheid_tekst,bron)

data_MPZO_stadsdelen <- data_totaal %>%
  filter(gebied_niveau == "gemeente" | gebied_niveau == "stadsdeel") %>% 
  select(gebied_niveau,gebied_naam,thema,`ambitie/strategisch.doel`,variabele,label,jaar,waarde,rekeneenheid_tekst,bron)

# pivot wider data
data_MPZO_stadsdelen_wide<-data_MPZO_stadsdelen%>%
  arrange(jaar)%>%
  select(-gebied_niveau)%>%
  pivot_wider(names_from = gebied_naam, values_from = waarde, values_fill = NA)

write.xlsx(data_MPZO_stadsdelen_wide, "06 output files/dataset_MPZO_stadsdelen.xlsx", overwrite = T)

