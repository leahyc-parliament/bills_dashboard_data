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

sitting_days <- calculate_sitting_day("2019-01-01", "2019-02-01")


# waaaaay simpler method lol

get_sitting_days <- function(start_date, end_date){
  url <- sprintf("https://api.parliament.uk/egg-timer/calculator/interval/calculate.json?end-date=%s&start-date=%s", end_date, start_date)

  response <- request(url) |> 
    req_perform()
}



resp <- request(url) |> 
  req_perform() |> 
  resp_body_json()

