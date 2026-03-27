
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

calendar <- read.csv("calendars/2018_01.csv")
