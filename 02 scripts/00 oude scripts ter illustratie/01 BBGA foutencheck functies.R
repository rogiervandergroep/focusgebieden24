






####################################################################################
### spatial_code - spatial_type - spatial_date - temporal_date - measure - value ---
####################################################################################

# 1. check dubbelingen
# 2. check spatial_code, spatial_type en bijbehorende spatial_date
# 3. check measure (kloppen de variabelnamen met variabelnamen uit metadata)
# 4. check hoeveel lege cellen met een kaartje 

####################################################################################
### spatial_code - spatial_type - spatial_date - temporal_date - measure - value ---
####################################################################################



# stap 1. check op dubbelen, gebiednaam en gebiedcode en variabelen ---
functie_duplicates <- function (x) {
  
   y <- x |>
     group_by( spatial_code, spatial_date, temporal_date, measure ) |>
     filter(n() > 1)
  
   if (nrow(y) == 0){
    
    print ("er zijn geen duplicates")
    
   } else {
    
     print ("waarschuwing, er zijn duplicates")
     return(y)
  }
  
  
}

# stap 2: check of spatial_code klopt -
functie_spatial_date_new <- function(x) {
  

  y <- x|>
      filter(
        spatial_date == '20220324',
        !spatial_code  %in% (geo_list |>
                               filter(spatial_date == '20220324')|>
                               pull(spatial_code))) 

  
  if (nrow(y) == 0){
    
    print ("alle gebiedscodes kloppen")
    
  } else {
    
    print ("waarschuwing, er staan onbekende gebiedcodes in!")
    return(y)
  }
}
    
functie_spatial_wg <- function(x) {
  
  # check winkelgebieden
    y <- x|>
      filter(
        
        spatial_type == 'winkelgebieden',
        !spatial_code  %in% (
          geo_list |>
            filter(spatial_type == 'winkelgebieden')|>
            pull(spatial_code)
          )
      ) 
  
  if (nrow(y) == 0){
    
    print ("alle wg gebiedscodes kloppen")
    
    } else {
      
      print ("waarschuwing, er staan onbekende wg gebiedcodes in!")
      return(y)
      
    }
}

# stap 3: check of variabelnamen voorkomen in meta_lijst (en vice versa)
functie_measure   <- function(x, meta) {

  measure_meta <- meta  |>
    select("Variabele") |>
    pull()
  
  measure_data <- x  |>
    select("measure") |>
    pull()
  
  # check of data-namen meta voorkomen
  y <- x|>
    filter(!measure_data  %in% measure_meta)
  
  
  if (nrow(y) == 0 ){

    print ("variabelnamen komen overeen")

    } else  {

      print ("waarschuwing, niet alle variabelen zitten in de metafile of vice versa")
      
      return(y)
      
    }


}


# stap 4 : check voor NA 
functie_na <- function(x) {
  
  # check na's per kolom
  y <- x|>
    summarise(across(everything(), ~ sum(is.na(.))))
  
  if (sum(y) == 0){
    
    print ("er zijn geen NA's")
    
  } else {
    
    print ("waarschuwing, er staan NA's in ")
    
    return(y)
    
  }
}


# stap 5 : check for Weesp 
functie_weesp <- function(x) {
  
  y <- x |>
    mutate(temporal_date= as.integer(temporal_date))|>
    filter(spatial_code %in% c("SB", "SC", "SD", "SE") & temporal_date < 20220324)
  
  
  
  if (sum(y) == 0 ){
    
    print ("er is geen data van Weesp van voor de fusie ")
    
  } else {
    
    print ("waarschuwing, er is data van Weesp aanwezig van voor de fusie")
    
    return(y)
    
  }
}
  
  
  
  






### samenvatting foutenchecks --- 
functie_check      <- function(data, meta) {
  
  fouten <- list(
  
   duplicated     = data |> functie_duplicates(),
   nas            = data |> functie_na(),
   sp_wijk23_code = data |> functie_spatial_date_new(),
   sp_wg_code     = data |> functie_spatial_wg(),
   vreemde_vars   = data |> functie_measure(meta)
   )
  
return(fouten)

 }

