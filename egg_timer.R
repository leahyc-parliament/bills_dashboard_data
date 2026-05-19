library(dplyr)
library(readr)
library(stringr)
library(httr2)

# download calendar csvs
years <- 2018:2027
months <- sprintf("%02d", 1:12)

dir.create("calendars", showWarnings = FALSE)

for (year in years) {
  for(month in months){

    csv_url <- sprintf("https://api.parliament.uk/egg-timer/calendar/%s/%s.csv", year, month)

    file_name <- sprintf("calendars/%s_%s.csv", year, month)

    download.file(csv_url, file_name)
  }
}


# combine all csvs into one df - using file names to build date column
file_names <- list.files("calendars", full.names = TRUE)

convert_dates <- function(csv) {
  file_name <- basename(csv)
  year_month <- str_split(file_name, "_|\\.csv", simplify = TRUE)
  year <- year_month[1]
  month <- year_month[2]

  data <- read_csv(csv)

  data <- data |> 
    mutate(year = year, 
          month = month, 
          day = sprintf("%02d", !!sym(names(data)[1])),
          date = as.Date(paste(year, month, day, sep = "-")))
  
  return(data)
}

calendar <- lapply(file_names, convert_dates) |> 
  bind_rows() |> 
  select("date",
         "Commons day type",
        "Lords day type")


# count sitting rows between inputted dates
calculate_sitting_day <- function(start_date, end_date) {
  filtered <- calendar |> 
    filter(
      date >= as.Date(start_date),
      date <= as.Date(end_date)
    )
  
  commons_count <- filtered |> 
    filter(`Commons day type` == "Parliamentary sitting day") |>
    nrow()

  lords_count <- filtered |>
    filter(`Lords day type` == "Parliamentary sitting day") |>
    nrow()

   return(list(
    commons = commons_count,
    lords = lords_count
  ))

}

# final sitting days returned
sitting_days <- calculate_sitting_day("2019-01-01", "2019-02-01")


# waaaaay simpler method lol (depending how we set up the shiny app, might actually be better to rely on local calendar tables rather than sending API requests for each calc?)
get_sitting_days <- function(start_date, end_date){
  url <- sprintf("https://api.parliament.uk/egg-timer/calculator/interval/calculate.json?end-date=%s&start-date=%s", end_date, start_date)

  response <- request(url) |> 
    req_perform() |> 
    resp_body_json()

   tibble(
    commons = as.integer(response$house_of_commons_sitting_day_count),
    lords   = as.integer(response$house_of_lords_sitting_day_count)
  )
}

sitting_days <- get_sitting_days("2019-01-01", "2019-02-01")

# for first_and_current_stages, if description = "Royal Ascent", use date, if else, use todays date. 
# Output of bill_id, start_date, end_date.
# if introduced to lords display "15 (Lords)"


