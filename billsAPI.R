library(httr)
library(jsonlite)
library(tidyverse)
library(httr2)
library(dplyr)
library(purrr)

# fetching all bills data
bills_url <- "https://bills-api.parliament.uk/api/v1/Bills?Take=50"

bills_response <- GET(
  bills_url,
  add_headers(accept = "text/plain")
)

# tidying up response
bills_text <- content(bills_response, as = "text", encoding = "UTF-8")
bills_json <- fromJSON(bills_text)

all_bills <- bills_json$items |> 
  tibble()

# extracting list of bill ID's
bill_ids <- all_bills$billId


# Filter to King's Speech bills using bill ID
kings_ids <- c(3733, 3881, 3734, 3938) # include all relevant bill IDs here
kings_bills <- all_bills |> filter (billId %in% kings_ids)


# Get bill stages data for one bill 
# You can comment this out, but I've left in just to show you the thought process behind the final script. 
# It's best to get the data in the format you need for just one item (in this case bill 3881) and then convert that into a function to loop through all items.
stages_url <- "https://bills-api.parliament.uk/api/v1/Bills/3881/Stages"

stages_response <- GET(
  stages_url,
  add_headers(accept = "text/plain")
)

stages_text <- content(stages_response, as = "text", encoding = "UTF-8")
stages_json <- fromJSON(stages_text)


first_and_current_stages <- stages_json$items |> 
  tibble() |> 
  unnest(stageSittings, names_sep = "_") |> 
  filter(stageSittings_date == min(stageSittings_date) | stageSittings_date == max(stageSittings_date))

# result <- test2 %>%
#   filter(stageSittings_date == min(stageSittings_date) | stageSittings_date == max(stageSittings_date))


# function to get bill stages - not finished

get_bill_stages <- function(bill_id) {

  bills_stages_url <- sprintf("https://bills-api.parliament.uk/api/v1/Bills/%s/Stages", bill_id) 

  stages_response <- GET(
  bills_stages_url,
  add_headers(accept = "text/plain")
)
  stages_text <- content(stages_response, as = "text", encoding = "UTF-8")

  stages_json <- fromJSON(stages_text)

  first_and_current_stages <- stages_json$items |> 
  tibble() |> 
  unnest(stageSittings, names_sep = "_") |> 
  filter(stageSittings_date == min(stageSittings_date) | stageSittings_date == max(stageSittings_date))  


}

# testing function
# stages <- map_dfr(bill_ids, get_bill_stages, .id = "bill_id")
stages <- lapply(bill_ids, get_bill_stages)
stages <- stages |> 
  bind_rows()
stages_test <- get_bill_stages(27)

final_data <- left_join(all_bills, stages, by = c("billId" = "stageSittings_billId"))


# testing httr2
stages_response_httr2 <- request(stages_url) |> req_headers(Accept = "text/plain") |> req_perform()

stages_text_httr2 <- stages_response_httr2 |> resp_body_string()

stages_json_httr2 <- jsonlite::fromJSON(stages_text_httr2)

first_and_current_stages_httr2 <- stages_json_httr2$items |> 
  tibble() |> 
  unnest(stageSittings, names_sep = "_") |> 
  filter(stageSittings_date == min(stageSittings_date) | stageSittings_date == max(stageSittings_date))

