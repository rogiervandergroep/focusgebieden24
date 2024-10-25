#inladen library
library(tidyverse)
library(openxlsx)


#inladen data
geboorteplek <- read.xlsx("00 ruwe data/jongeren_leeftijdsgroep_geboorteplaats_buurt_wijk.xlsx", sheet = "T5")

#aandeel jongeren wel/niet geboren in NL
geboorteplek_aandeel_perwijk <- geboorteplek %>% 
  group_by(VERSLAGJAAR, WIJK, LEEFTIJDSGROEP) %>% 
  mutate(aandeel_perwijk = aantal/sum(aantal)*100)

#wegschrijven in excel
write.xlsx(list("geboorteplek_aandeel_perwijk" = geboorteplek_aandeel_perwijk),
           "./00 ruwe data/geboorteplekjong_aandeel_perwijk.xlsx")