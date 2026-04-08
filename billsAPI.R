library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
library(httr2)
library(dplyr)
library(purrr)
library(janitor)
library(xml2)

# creating list of original bill titles from Kings speech announcement
announcement_url <- "https://commonslibrary.parliament.uk/research-briefings/cbp-10314/"

page <- read_html(announcement_url)
table <- page |> 
  html_table() |> 
  purrr::pluck(1) |> 
  row_to_names(row_number = 1)


# fetching all bills data
bills_url <- "https://bills-api.parliament.uk/api/v1/Bills?Take=10000"

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
stages_url <- "https://bills-api.parliament.uk/api/v1/Bills/3733/Stages"

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

# Next get introduction bill title and info on current stage from bill news dataset
# Create a function that gets data for one bill at a time and then repeats this for each bill before binding data together
get_news <- function(kings_ids) {
 
news_url <- sprintf("https://bills-api.parliament.uk/api/v1/Bills/%s/NewsArticles", kings_ids)
 
news_response <- GET(
  news_url,
  add_headers(accept = "text/plain")
)
 
news_text <- content(news_response, as = "text", encoding = "UTF-8")
news_json <- fromJSON(news_text)
 
news_data <- news_json$items |>
  tibble()
}
 
# Repeat for each King's Speech bills
news_data <- lapply(kings_ids, get_news)
 
# Collate data into one table and tidy to include only relevant data
news_data <- news_data |>
  bind_rows() |>
    mutate(billId = kings_ids) |>  # add column with bill IDs  
    select(billId, title, content) # delete columns other than those showing bill IDs, member name and member party


# Next get info on related Library briefings
# Create a function that gets data for one bill at a time and then repeats this for each bill before binding data together
 
get_publications <- function(kings_ids) {
 
publications_url <- sprintf("https://bills-api.parliament.uk/api/v1/Bills/%s/Publications", kings_ids)
 
publications_response <- GET(
  publications_url,
  add_headers(accept = "text/plain")
)
 
publications_text <- content(publications_response, as = "text", encoding = "UTF-8")
publications_json <- fromJSON(publications_text)
 
publications_data <- publications_json$publications |>
  tibble()  |>
  unnest(publicationType, names_sep = "_") |>
  unnest(links, names_sep = "_") |>
  # filter(publicationType_name == "Briefing papers" | house == "Commons") |> # Not working for some reason
  mutate(billId = publications_json$billId) |> # Create column with bill ID
  relocate(billId)
}
 
# Repeat for each King's Speech bills
publications_data <- lapply(kings_ids, get_publications)
 
# Collate data into one table and remove unnecessary columns
publications_data <- publications_data |>
  bind_rows() |>
  filter(publicationType_name == "Briefing papers") |> 
  filter(house == "Commons") |> 
  select(billId, title, links_url) # delete columns other than those showing bill IDs, member name and member party
 


# # testing httr2
# stages_response_httr2 <- request(stages_url) |> req_headers(Accept = "text/plain") |> req_perform()

# stages_text_httr2 <- stages_response_httr2 |> resp_body_string()

# stages_json_httr2 <- jsonlite::fromJSON(stages_text_httr2)

# first_and_current_stages_httr2 <- stages_json_httr2$items |> 
#   tibble() |> 
#   unnest(stageSittings, names_sep = "_") |> 
#   filter(stageSittings_date == min(stageSittings_date) | stageSittings_date == max(stageSittings_date))

