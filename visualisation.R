# ============================================================
# 02_visualisation.R
# Exploratory Analysis of COVID-19 Trends in Switzerland Using R
# ============================================================

# ---------------------------
# 1. Load packages
# ---------------------------
required_packages <- c("tidyverse", "lubridate", "scales", "patchwork")

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------
# 2. Check required files
# ---------------------------
required_files <- c(
  "data/processed/switzerland_national_clean.csv",
  "data/processed/switzerland_canton_comparison.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Processed data files are missing. Run '01_data_cleaning.R' first. Missing files: ",
    paste(missing_files, collapse = ", ")
  )
}

if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

# ---------------------------
# 3. Read processed data
# ---------------------------
switzerland_national <- read_csv(
  "data/processed/switzerland_national_clean.csv",
  show_col_types = FALSE
) %>% mutate(date = ymd(date))

switzerland_canton_comparison <- read_csv(
  "data/processed/switzerland_canton_comparison.csv",
  show_col_types = FALSE
) %>% mutate(date = ymd(date))

comparison_date_label <- format(unique(switzerland_canton_comparison$date), "%d %B %Y")

# ---------------------------
# 4. Plot: cases over time
# ---------------------------
plot_cases <- ggplot(
  switzerland_national,
  aes(x = date, y = new_cases_smoothed)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "COVID-19 Cases in Switzerland",
    subtitle = "7-day smoothed daily confirmed cases",
    x = "Date",
    y = "New confirmed cases"
  ) +
  theme_minimal(base_size = 12)

# ---------------------------
# 5. Plot: deaths over time
# ---------------------------
plot_deaths <- ggplot(
  switzerland_national,
  aes(x = date, y = new_deaths_smoothed)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "COVID-19 Deaths in Switzerland",
    subtitle = "7-day smoothed daily reported deaths",
    x = "Date",
    y = "New deaths"
  ) +
  theme_minimal(base_size = 12)

# ---------------------------
# 6. Plot: vaccination uptake
# ---------------------------
vaccination_long <- switzerland_national %>%
  select(
    date,
    people_vaccinated_per_hundred,
    people_fully_vaccinated_per_hundred,
    total_boosters_per_hundred
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      people_vaccinated_per_hundred = "At least one dose",
      people_fully_vaccinated_per_hundred = "Fully vaccinated",
      total_boosters_per_hundred = "Booster doses"
    )
  )

plot_vaccinations <- ggplot(vaccination_long, aes(x = date, y = value, linetype = metric)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "COVID-19 Vaccination Uptake in Switzerland",
    subtitle = "Cumulative doses or people per 100 population",
    x = "Date",
    y = "Per 100 people",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ---------------------------
# 7. Plot: canton comparison
# ---------------------------
plot_cantons <- switzerland_canton_comparison %>%
  mutate(canton_name = fct_reorder(canton_name, cumulative_confirmed_cases)) %>%
  ggplot(aes(x = canton_name, y = cumulative_confirmed_cases)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Cumulative Confirmed COVID-19 Cases by Canton",
    subtitle = paste0("Comparable reporting date: ", comparison_date_label),
    x = NULL,
    y = "Cumulative confirmed cases"
  ) +
  theme_minimal(base_size = 12)

# ---------------------------
# 8. Combined dashboard
# ---------------------------
combined_dashboard <- (plot_cases / plot_deaths) / (plot_vaccinations / plot_cantons)

# ---------------------------
# 9. Saving Plots
# ---------------------------
ggsave("plots/01_cases_over_time.png", plot_cases, width = 10, height = 6, dpi = 300)
ggsave("plots/02_deaths_over_time.png", plot_deaths, width = 10, height = 6, dpi = 300)
ggsave("plots/03_vaccination_uptake.png", plot_vaccinations, width = 10, height = 6, dpi = 300)
ggsave("plots/04_canton_comparison.png", plot_cantons, width = 10, height = 8, dpi = 300)
ggsave("plots/05_combined_dashboard.png", combined_dashboard, width = 14, height = 14, dpi = 300)

# ---------------------------
# 10. Summary
# ---------------------------
latest_total_cases <- switzerland_national %>%
  filter(!is.na(total_cases)) %>%
  slice_tail(n = 1) %>%
  pull(total_cases)

latest_total_deaths <- switzerland_national %>%
  filter(!is.na(total_deaths)) %>%
  slice_tail(n = 1) %>%
  pull(total_deaths)

message("\nVisualisation complete.")
message("Latest total cases in processed national data: ", comma(latest_total_cases))
message("Latest total deaths in processed national data: ", comma(latest_total_deaths))
message("Plots saved to the 'plots/' folder.")
