library(openxlsx)
library(glue)
library(tidyverse)

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))

# maak een lege list
style <- list()

# en voeg alle stijlcodes toe
style$font <- "Amsterdam Sans"
style$fontsize <- 11


# Define a style with blue background for the header row
style$header_left <- createStyle(
  fontName = style$font,
  fontColour = "#FFFFFF",      
  fgFill = "#004699",          
  halign = "LEFT",             
  textDecoration = "bold",
  fontSize = style$fontsize)

style$header_colofon <- createStyle(
  fontName = style$font,
  halign = "LEFT",             
  textDecoration = "bold",
  fontSize = 12)

style$data_colofon <- createStyle(
  fontName = style$font,
  halign = "LEFT",  
  fontSize = 11)

# header uitlijning linkg
style$header_right <- createStyle(
  fontName = style$font,
  fontColour = "#FFFFFF",      
  fgFill = "#004699",          
  halign = "RIGHT",             
  textDecoration = "bold",
  fontSize = style$fontsize)

# header midden (voor boolean)
style$header_centre <- createStyle(
  fontName = style$font,
  fontColour = "#FFFFFF",      
  fgFill = "#004699",          
  halign = "center",             
  textDecoration = "bold",
  fontSize = style$fontsize)


style$data_base <- createStyle(
  fontName = style$font,
  fontSize = style$fontsize)

style$data_bold <- createStyle(
  textDecoration = "bold",
  fontName = style$font,
  fontSize = style$fontsize)

style$light_blue <- createStyle(
  fontName = style$font,
  fgFill = "#b8bcdd",
  fontSize = style$fontsize)

style$dark_blue <- createStyle(
  fontName = style$font,
  fgFill = "#707ebb",
  fontColour = "#FFFFFF" ,
  fontSize = style$fontsize)

# dit is het colofon voor de nul- en eenmeting tabellen
colofon_eenmeting <- tibble::tibble("Tabellenrapportage" = c(
  "",
  "Ter ondersteuning van de {naam_monitor}",
  "",
  "Projectnummer: 24015",
  "",
  "Eva Karacay",
  "Marloes de Hoon",
  "Rogier van der Groep",
  "Ralph Rusconi",
  "",
  "onderzoek.amsterdam.nl",
  "rogier.van.der.groep@amsterdam.nl",
  "Amsterdam, november 2024",
  "",
  "In samenwerking met { naam_focusgebied } zijn indicatoren geselecteerd voor de outcome-monitoring van de integrale aanpak van problemen in { naam_stadsdeel }.",
  "Bij dit beleidsprogramma zijn ambities en strategische doelen vastgelegd waarmee de alliantie van { naam_focusgebied } de komende 20 tot 25 jaar aan de slag gaat.",
  "Deze ambities en doelen hebben betrekking op het verbeteren van de positie van inwoners in buurten.",
  "Meer informatie hierover is te vinden op { naam_website_focusgebied }.",
  "",
  "Aan de hand van de indicatoren wordt onderzocht of de alliantie van { naam_focusgebied } op de goede weg is om de ambities en de strategische doelen te behalen.",
  "",
  "In dit Excelbestand wordt per tabblad en per ambitie of doel de uitkomsten weergegeven van de indicatoren op stadsdeelniveau en gemeenteniveau.",
  "Voor de overzichtelijkheid zijn alleen de waardes weergegeven op het moment van de nulmeting (met als peildatum op of rond 2020)",
  "en op het moment van de éénmeting (met als peildatum op of rond 2024).",
  "Deze lijst met indicatoren is ook gebruikt als basis voor de achtergrondrapportage: ‘{ naam_monitor }’.",
  "",
  "De volledige dataset met data op wijk- en buurtniveau en voor meerdere jaargangen is te raadplegen via 'tabel alle data { naam_focusgebied } nov 2024.xlsx'", 
  "op de pagina onderzoek.amsterdam.nl/dataset/focusgebieden-amsterdam.",
  "",
  "",
  "Inhoudsopgave",
  ""
))

# dit is de colofon voor de totale tabellen
colofon_totaal <- tibble::tibble("Tabellenrapportage" = c(
  "",
  "Ter ondersteuning van de { naam_monitor }",
  "",
  "Projectnummer: 24015",
  "",
  "Eva Karacay",
  "Marloes de Hoon",
  "Rogier van der Groep",
  "Ralph Rusconi",
  "",
  "onderzoek.amsterdam.nl",
  "rogier.van.der.groep@amsterdam.nl",
  "Amsterdam, november 2024",
  "",
  "In samenwerking met { naam_focusgebied } zijn indicatoren geselecteerd voor de outcome-monitoring van de integrale aanpak van problemen in { naam_stadsdeel }.",
  "Bij dit beleidsprogramma zijn ambities en strategische doelen vastgelegd waarmee de alliantie van { naam_focusgebied } de komende 20 tot 25 jaar aan de slag gaat.",
  "Deze ambities en doelen hebben betrekking op het verbeteren van de positie van inwoners in buurten.",
  "Meer informatie hierover is te vinden op { naam_website_focusgebied }.",
  "",
  "Aan de hand van de indicatoren wordt onderzocht of de alliantie van { naam_focusgebied } op de goede weg is om de ambities en de strategische doelen te behalen.",
  "",
  "In dit Excelbestand wordt per tabblad en per ambitie of doel de uitkomsten weergegeven op alle beschikbare aggregatieniveaus en alle beschikbare jaren.",
  "Deze lijst met indicatoren is ook gebruikt als basis voor de achtergrondrapportage: ‘{ naam_monitor }’",
  "De dataset met de waardes van de nul- en eenmeting op stadsdeelniveau is te raadplegen via 'tabel eenmeting { naam_focusgebied } nov 2024.xlsx'.",
  "",
  "",
  "Inhoudsopgave",
  ""
))


colofon_nplv <- tibble("Indicatorenoverzicht NPLV" = c(
  "",
  "ter ondersteuning van de outcome monitoring van",
  "het Masterplan Zuidoost, het Nationaal Programma Samen Nieuw-West en Aanpak Noord",
  "Projectnummer: 24015",
  "",
  "Eva Karacay",
  "Marloes de Hoon",
  "Rogier van der Groep",
  "Ralph Rusconi",
  "",
  "onderzoek.amsterdam.nl",
  "rogier.van.der.groep@amsterdam.nl",
  "Amsterdam, november 2024",
  "",
  "Vanuit het Nationaal Programma Leefbaarheid en Veiligheid (NPLV) is een lijst met indicatoren opgesteld om de voortgang van de nationale programma's rondom de focusgebieden in Nederland te monitoren.", 
  "Deze 'NPLV-lijst' vormt de basis van de indicatorenlijsten van Masterplan Zuidoost, Samen Nieuw-West en Aanpak Noord.",
  "De bevindingen van de NPLV-lijst worden gebruikt voor de outcome monitoring in Amsterdam op stadsdeelniveau.",
  ""
))



# col_dark_bl:  vector van kolommen die donkerblauw moeten zijn 
# col_light_bl: vector van kolommen die licht moeten zijn   
# colofon_type: keuze uit colofon_nplv, colofon_eenmeting, colofon_totaal

my_style_sheet <- function (x, col_dark_bl, col_light_bl, colofon_type) {
  
  sheet_nr       <- c(1:length(x))
  
  # maak een leeg workbook aan
  wb <- createWorkbook()
  
  # Loop through the list and add each element to a new sheet
  for (i in sheet_nr) {
    
    sheet_name <- names(x)[i]  
    addWorksheet(wb, sheet_name)     
    writeData(wb, sheet_name, x[[i]])

    setColWidths(wb, sheet_name, cols = 1, widths = 50)
    setColWidths(wb, sheet_name, cols = 2:ncol(x[[i]]), widths = 30)
    
    # basisstijl
    addStyle(wb, sheet_name, style$data_base, rows = 1:(nrow(x[[i]])+1), cols = 1:ncol(x[[i]]),gridExpand = TRUE)
    
    # maak de rijen met kernindicatoren bol
    addStyle(wb, sheet_name, style$data_bold, rows = which(str_detect(x[[i]][['naam indicator']], 'kernindicator')) + 1, cols = 1:ncol(x[[i]]), gridExpand = T)
    
    # selecteer de kolommen die lichtblauw moeten zijn (met zwarte tekst)
    addStyle(wb, sheet_name, style$dark_blue, rows = 2:(nrow(x[[i]])+ 1), cols = col_dark_bl, gridExpand = TRUE)
    
    # selecteer de kolommen die donderblauw moeten zijn (met witte tekst)
    addStyle(wb, sheet_name, style$light_blue, rows = 2:(nrow(x[[i]])+ 1), cols = col_light_bl, gridExpand = TRUE)
    
    # zet de headertekst op links en op rechts
    addStyle(wb, sheet_name, style$header_left,   rows = 1, cols = which(sapply(x[[i]],is.character)), gridExpand = TRUE)
    addStyle(wb, sheet_name, style$header_left,   rows = 1, cols = which(sapply(x[[i]],is.factor)), gridExpand = TRUE)
    addStyle(wb, sheet_name, style$header_right,  rows = 1, cols = which(sapply(x[[i]],is.numeric)), gridExpand = TRUE)
    addStyle(wb, sheet_name, style$header_centre, rows = 1, cols = which(sapply(x[[i]],is.logical)), gridExpand = TRUE)
    
    
  }

  ### voeg colofon toe ---
  
  if (identical(colofon_type, colofon_nplv)) {
    
    colofon_sheet = colofon_type 
    
  } else if (identical(colofon_type, colofon_totaal) | identical(colofon_type, colofon_eenmeting)) {
    
  # naam van kolom
  colofon_kolom_naam  <- names(colofon_type)[1]

  # keuze uit colofon_eenmeting of colofon_totaal
  tekst <-  colofon_type |>
      mutate(
        !!colofon_kolom_naam := map_chr(
        colofon_type[[colofon_kolom_naam]], \(y) glue(y))
        )
  
  # inhoudsopgave uit de tabel_lijst
  inhoudsopg <-  tibble(!!colofon_kolom_naam := names(x))
  
  inhoudsopg <- inhoudsopg |>
      mutate(
        !!colofon_kolom_naam := glue("sheet { inhoudsopg[[ colofon_kolom_naam ]] }")
        )
  
  # koppel colofontekst aan de inhoudsopgave
  colofon_sheet <- bind_rows(tekst, inhoudsopg)
  
  } 
  
  # voeg het colofon aan bestaande workboek toe als nieuwe sheet
  addWorksheet(wb, "Colofon")
  
  writeData(wb, "Colofon", colofon_sheet)
  addStyle(wb,  "Colofon", style$data_colofon,   rows = 1:nrow(colofon_type)+1, cols = 1, gridExpand = TRUE)
  addStyle(wb,  "Colofon", style$header_colofon, rows = 1:3,                    cols = 1, gridExpand = TRUE)

  # zet het colofon vooraan
  sheet_nr_plus1 <- length(sheet_nr)+1
  worksheetOrder(wb) <- c(sheet_nr_plus1, sheet_nr)
  
  return(wb)
  
}







