# Clean environment ------------------------------------------------------------
rm(list = ls())

# Library and System -----------------------------------------------------------

# Installation
pack_to_install <- c("dplyr", "purrr", "jsonlite", "httr", "MMWRweek",
                     "stringr", "tidyr")
if (length(which(!(pack_to_install %in% rownames(installed.packages())))) > 0) {
  pack_to_install <- pack_to_install[which(!(pack_to_install %in%
                                               rownames(installed.packages())))]
  install.packages(pack_to_install)
}

# Library
library(jsonlite)
library(purrr)     # for list management
library(dplyr)     # for data frame management
library(httr)
library(MMWRweek)

# Prerequisite -----------------------------------------------------------------
# Source utils function
source("./code/obs-data/obs-data_utils.R")

# Location information
model_location <- read.csv("data/data-locations/locations.csv")
number2location <- setNames(model_location$location_name,
                            model_location$location)
number2location <- c(number2location, setNames("New York City", 57))
abbr2location <- setNames(c(model_location$location_name, "US"),
                          c(model_location$abbreviation, "USA"))
location2number <- setNames(c(model_location$location, "US"),
                            c(model_location$location_name, "United States"))

# Date limit for Gold Standard data (to have data until the last full epiweek):
eweek <- MMWRweek::MMWRweek(Sys.Date())$MMWRweek
eyear <- MMWRweek::MMWRweek(Sys.Date())$MMWRyear

limit_date <- as.Date("2021-01-02") +
  (as.numeric(eweek) + 52 * (as.numeric(eyear) - 2021)) * 7
if (limit_date > Sys.Date()) limit_date <- limit_date - 1

vect_week_date <- as.Date("2020-01-04")
while (max(vect_week_date) < limit_date) {
  j <- max(vect_week_date) + 7
  vect_week_date <- c(vect_week_date, j)
}
rm(j)

# Seasons information
season_json <-
  "https://gis.cdc.gov/grasp/flu7/GetPhase07InitApp?appVersion=Public"
seasons_info <- jsonlite::fromJSON(season_json)
seasons_info <- dplyr::filter(seasons_info$seasons, grepl("202.|2019", label))

seasons_param <- paste('{"ID":', seasons_info$seasonid, "}", sep = "",
                       collapse = ",")
number2season <- setNames(gsub("-", "-20", seasons_info$label),
                          as.numeric(seasons_info$seasonid))


# FluView COVID-19 Death Data --------------------------------------------------
user_agent_info <-
  paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 ",
         "(KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36")
headers <- c(`authority` = "gis.cdc.gov",
             `accept` = "application/json, text/plain, */*",
             `accept-language` = "en-US,en;q=0.9",
             `cache-control` = "no-cache",
             `content-type` = "application/json;charset=UTF-8",
             `origin` = "https://gis.cdc.gov",
             `pragma` = "no-cache",
             `referer` = "https://gis.cdc.gov/grasp/fluview/mortality.html",
             `sec-ch-ua` = paste0('"Google Chrome";v="111", ',
                                  '"Not(A:Brand";v="8", "Chromium";v="111"'),
             `sec-ch-ua-mobile` = "?0",
             `sec-ch-ua-platform` = '"macOS"',
             `sec-fetch-dest` = "empty",
             `sec-fetch-mode` = "cors",
             `sec-fetch-site` = "same-origin",
             `user-agent` = user_agent_info)
state_fv_data <- paste0('{"AppVersion":"Public","AreaParameters":[{"ID":2}],',
                        '"SeasonsParameters":[', seasons_param,
                        '],"AgegroupsParameters":[{"ID":1}]}')
fv_data <- paste0('{"AppVersion":"Public","AreaParameters":[{"ID":1}],',
                  '"SeasonsParameters":[', seasons_param,
                  '],"AgegroupsParameters":[{"ID":1}]}')
url <- "https://gis.cdc.gov/grasp/flu7/PostPhase07DownloadData"

# Add NY City for COVID-19 Death data (source FluView)
number2location <- c(number2location, setNames("New York City", 57))

# Extract and standardized data
df_state <- fluview_data(headers, state_fv_data, "state", number2location, url)
df_us <- fluview_data(headers, fv_data, "US", number2location, url)

df_death <- standard_fluview(df_us, df_state, number2season,
                             end_date = limit_date - 28) |>
  dplyr::mutate(age_group = "0-130", target = "inc death")

# ED Visit ---------------------------------------------------------------------
cdc_api <- "https://data.cdc.gov/api/views/"
df_ed_demo <- read.csv(paste0(cdc_api,
                              "7xva-uux8/rows.csv?&accessType=DOWNLOAD")) |>
  dplyr::filter(demographics_type == "Age Group", pathogen == "COVID-19") |>
  dplyr::mutate(
    age_group =
      dplyr::case_when(demographics_values == "0-4 years" ~ "0-4",
                       demographics_values == "5-17 years" ~ "5-17",
                       demographics_values == "65+ years" ~ "65-130",
                       demographics_values == "18-64 years" ~ "18-64"),
    target = "% ed visit", location = "US") |>
  dplyr::select(location, date = week_end, observation = percent_visits,
                age_group, target)

df_ed_state <- read.csv(paste0(cdc_api,
                               "vutn-jzwm/rows.csv?&accessType=DOWNLOAD")) |>
  dplyr::filter(pathogen == "COVID-19") |>
  dplyr::mutate(location = location2number[geography],
                age_group = "0-130", target = "% ed visit") |>
  dplyr::select(location, date = week_end, observation = percent_visits,
                age_group, target)

df_ed <- rbind(df_ed_state, df_ed_demo)

# NHSN Hospitalization Data ----------------------------------------------------
df_nhsn <- extract_nhsn(hosp_report = TRUE, pathogen = "COVID") |>
  standard_nhsn(location2number, abbr2location) |>
  dplyr::mutate(target = "inc hosp")

# Write output -----------------------------------------------------------------
write.csv(rbind(df_death, df_nhsn, df_ed),
          paste0("./data/obs-data/time-series_", Sys.Date(), ".csv"),
          row.names = FALSE)

# Clean environment ------------------------------------------------------------
rm(list = ls())
