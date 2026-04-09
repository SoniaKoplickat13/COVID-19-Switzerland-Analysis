# ============================================================
# 01_data_cleaning.R
# Exploratory Analysis of COVID-19 Trends in Switzerland Using R
# ============================================================

# ---------------------------
# 1. Load packages
# ---------------------------
required_packages <- c("tidyverse", "lubridate")

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------
# 2. Create folders
# ---------------------------
dirs_to_create <- c("data/raw", "data/processed", "plots")

for (folder in dirs_to_create) {
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
}

# ---------------------------
# 3. Define data sources
# ---------------------------
owid_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
openzh_url <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv"

# ---------------------------
# 4. Download raw data
# ---------------------------
message("Downloading OWID Switzerland data...")
download.file(owid_url, destfile = "data/raw/owid_covid_data.csv", mode = "wb")

message("Downloading openZH canton data...")
download.file(openzh_url, destfile = "data/raw/openzh_canton_data.csv", mode = "wb")

# ---------------------------
# 5. Read raw data
# ---------------------------
owid_raw <- read_csv("data/raw/owid_covid_data.csv", show_col_types = FALSE)
openzh_raw <- read_csv("data/raw/openzh_canton_data.csv", show_col_types = FALSE)

# ---------------------------
# 6. Clean national Switzerland data
# ---------------------------
switzerland_national <- owid_raw %>%
  filter(location == "Switzerland") %>%
  transmute(
    date = ymd(date),
    location,
    new_cases = as.numeric(new_cases),
    new_cases_smoothed = as.numeric(new_cases_smoothed),
    total_cases = as.numeric(total_cases),
    new_deaths = as.numeric(new_deaths),
    new_deaths_smoothed = as.numeric(new_deaths_smoothed),
    total_deaths = as.numeric(total_deaths),
    people_vaccinated_per_hundred = as.numeric(people_vaccinated_per_hundred),
    people_fully_vaccinated_per_hundred = as.numeric(people_fully_vaccinated_per_hundred),
    total_boosters_per_hundred = as.numeric(total_boosters_per_hundred)
  ) %>%
  arrange(date)

# ---------------------------
# 7. Clean canton-level data
# ---------------------------
switzerland_canton_daily <- openzh_raw %>%
  mutate(date = ymd(date)) %>%
  filter(abbreviation_canton_and_fl != "FL") %>%
  transmute(
    date,
    canton_code = abbreviation_canton_and_fl,
    cumulative_confirmed_cases = as.numeric(ncumul_conf)
  ) %>%
  arrange(canton_code, date) %>%
  group_by(canton_code) %>%
  mutate(
    new_confirmed_cases = cumulative_confirmed_cases - lag(cumulative_confirmed_cases),
    new_confirmed_cases = if_else(new_confirmed_cases < 0, NA_real_, new_confirmed_cases)
  ) %>%
  ungroup()

# ---------------------------
# 8. Determine a comparable canton snapshot date
# ---------------------------
comparison_date <- switzerland_canton_daily %>%
  filter(!is.na(cumulative_confirmed_cases)) %>%
  count(date, name = "n_reporting_cantons") %>%
  filter(n_reporting_cantons >= 20) %>%
  summarise(comparison_date = max(date, na.rm = TRUE)) %>%
  pull(comparison_date)

# Fallback in case the threshold is too strict for the downloaded file
if (length(comparison_date) == 0 || is.na(comparison_date)) {
  comparison_date <- switzerland_canton_daily %>%
    filter(!is.na(cumulative_confirmed_cases)) %>%
    summarise(comparison_date = max(date, na.rm = TRUE)) %>%
    pull(comparison_date)
}

canton_name_lookup <- tibble(
  canton_code = c(
    "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR",
    "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG",
    "TI", "UR", "VD", "VS", "ZG", "ZH"
  ),
  canton_name = c(
    "Aargau", "Appenzell Innerrhoden", "Appenzell Ausserrhoden", "Bern",
    "Basel-Landschaft", "Basel-Stadt", "Fribourg", "Geneva", "Glarus",
    "Graubünden", "Jura", "Lucerne", "Neuchâtel", "Nidwalden", "Obwalden",
    "St. Gallen", "Schaffhausen", "Solothurn", "Schwyz", "Thurgau", "Ticino",
    "Uri", "Vaud", "Valais", "Zug", "Zurich"
  )
)

switzerland_canton_comparison <- switzerland_canton_daily %>%
  filter(date == comparison_date) %>%
  left_join(canton_name_lookup, by = "canton_code") %>%
  arrange(desc(cumulative_confirmed_cases))

# ---------------------------
# 9. Save processed data
# ---------------------------
write_csv(switzerland_national, "data/processed/switzerland_national_clean.csv")
write_csv(switzerland_canton_daily, "data/processed/switzerland_canton_daily_clean.csv")
write_csv(switzerland_canton_comparison, "data/processed/switzerland_canton_comparison.csv")

# ---------------------------
# 10. Quick checks in console
# ---------------------------
message("\nData cleaning complete.")
message("National rows: ", nrow(switzerland_national))
message("Canton daily rows: ", nrow(switzerland_canton_daily))
message("Comparison date used for canton chart: ", comparison_date)
message("Top 5 cantons by cumulative confirmed cases on comparison date:")

print(
  switzerland_canton_comparison %>%
    select(canton_code, canton_name, cumulative_confirmed_cases) %>%
    slice_head(n = 5)
)
