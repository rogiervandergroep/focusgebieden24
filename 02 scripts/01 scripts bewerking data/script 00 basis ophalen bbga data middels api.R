### TEST API ####

#install.packages("httr2")

library(httr2)
library(jsonlite)
library(tidyverse)

indicatoren <- "https://api.data.amsterdam.nl/v1/statistieken/v2/indicatoren"
cijfers <- "https://api.data.amsterdam.nl/v1/statistieken/v2/cijfers"
kengetallen <- "https://api.data.amsterdam.nl/v1/statistieken/v2/kengetallen"

## voor 1 pagina
# resp <- request(cijfers) |>
#   req_perform()

# body <- resp |> resp_body_string()   # get raw text
# data <- jsonlite::fromJSON(body)  # parse manually

# data_df <- data[["_embedded"]][["cijfers"]]

all_data <- list()
page <- 1

repeat {
  resp <- request(cijfers) |>
    req_url_query(page = page) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  data <- resp |>
    resp_body_string() |>
    jsonlite::fromJSON()

  records <- data[["_embedded"]][["cijfers"]]

  if (length(records) == 0) {
    break
  }

  all_data[[page]] <- records
  cat(
    "Fetched page",
    page,
    "| Records so far:",
    sum(sapply(all_data, nrow)),
    "\n"
  )

  page <- page + 1
}

final_df <- bind_rows(lapply(all_data, as.data.frame))


#res <- GET(cijfers, query = list(gebiedscode = "KF", indicatorId = ""))

cijfers_data = fromJSON(rawToChar(cijfers_raw$content))

kengetallen_data = fromJSON(rawToChar(kengetallen_raw$content))

indicatoren_data = fromJSON(rawToChar(indicatoren_raw$content))


df_cijfers <- cijfers_data[["_embedded"]][["cijfers"]]

df_kengetallen <- kengetallen_data[["_embedded"]][["kengetallen"]]

df_indicatoren <- indicatoren_data[["_embedded"]][["indicatoren"]]

names(df_cijfers)

names(df_kengetallen)

names(df_indicatoren)
