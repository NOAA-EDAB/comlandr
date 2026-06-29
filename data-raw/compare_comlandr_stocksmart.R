# =========================================================================
# Title: Compare comlandr Commercial Landings vs stocksmart Assessment Data
#
# Purpose: Investigate discrepancies between comlandr landings and stock
#          assessment data across ALL overlapping species (Issue #43).
# Hypothesis: Differences are driven by discards, recreational landings,
#             and geographic area mismatches.
# Updates: Grouped skate complex, removed foreign landings, isolated overages,
#          dynamically converted complex units (e.g., 'Thousand lbs'),
#          capped visual outliers, and hardcoded BSB stock exception.
# =========================================================================

# 1. Setup & Load Libraries -----------------------------------------------
library(comlandr)
library(stocksmart)
library(tidyverse)
library(data.table)
library(dbutils)
library(DBI)

start_year <- 1985
end_year <- as.integer(format(Sys.Date(), "%Y")) - 1

message(sprintf(
  "Starting full species comparison: %d-%d",
  start_year,
  end_year
))

# Ensure the target directory exists for our outputs
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# 2. Pull Data from NEFSC Database ----------------------------------------
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

# Create a master area map covering all possible US statistical areas (1 to 999)
coastwide <- data.table(AREA = 1:999, EPU = "Coastwide")
coastwide[, NESPP3 := 1]
coastwide[, MeanProp := 1]

message(
  "Pulling Coastwide landings from comlandr (excluding foreign landings)..."
)
landings <- comlandr::get_comland_data(
  channel,
  filterByYear = start_year:end_year,
  userAreas = coastwide,
  aggGear = TRUE,
  aggArea = TRUE,
  useForeign = FALSE # Explicitly exclude foreign landings
)

message("Pulling commercial discards from comlandr...")
discards <- comlandr::get_comdisc_data(
  channel,
  landings,
  aggArea = TRUE,
  aggGear = TRUE
)

message("Pulling species name lookup table via comlandr...")
spp_lookup_raw <- comlandr::get_species_itis(channel, species = "all")

# Cleanly close the database connection
DBI::dbDisconnect(channel)

# Extract the first 3 characters of NESPP4 to create the NESPP3 joining key
spp_lookup <- spp_lookup_raw$data |>
  mutate(NESPP3 = as.numeric(substr(as.character(NESPP4), 1, 3))) |>
  select(NESPP3, COMMON_NAME) |>
  distinct() |>
  group_by(NESPP3) |>
  slice(1) |> # Force a 1-to-1 mapping
  ungroup()

# 3. Standardize comlandr Species Names (Landings & Discards) -------------
message("Standardizing comlandr species names & grouping Skate Complex...")

# Helper function to clean names and group all skates into "skate complex"
clean_cfdbs_names <- function(df) {
  df |>
    mutate(
      JoinName = tolower(COMMON_NAME),
      JoinName = str_replace_all(JoinName, "[,\\.]", " "),
      JoinName = str_squish(JoinName),
      JoinName = case_when(
        str_detect(JoinName, "skate") ~ "skate complex", # Group all skates
        JoinName == "sea bass black" ~ "black sea bass",
        JoinName == "flounder summer fluke" ~ "summer flounder",
        JoinName == "flounder summer" ~ "summer flounder",
        JoinName == "flounder winter" ~ "winter flounder",
        JoinName == "flounder yellowtail" ~ "yellowtail flounder",
        JoinName == "flounder windowpane" ~ "windowpane",
        JoinName == "hake silver whiting" ~ "silver hake",
        JoinName == "hake silver" ~ "silver hake",
        JoinName == "hake atlantic red" ~ "red hake",
        JoinName == "hake atlantic white" ~ "white hake",
        JoinName == "cod atlantic" ~ "atlantic cod",
        JoinName == "pollock atlantic" ~ "pollock",
        JoinName == "goosefish monkfish" ~ "monkfish",
        JoinName == "scallop sea" ~ "sea scallop",
        TRUE ~ JoinName
      )
    ) |>
    rename(Year = YEAR)
}

# Clean Landings (Native comlandr units are SPPLIVMT: Metric Tons)
comlandr_clean <- landings$comland |>
  as_tibble() |>
  mutate(NESPP3 = as.numeric(NESPP3)) |>
  left_join(spp_lookup, by = "NESPP3") |>
  filter(!is.na(COMMON_NAME)) |>
  clean_cfdbs_names() |>
  group_by(Year, JoinName) |>
  summarise(Com_Landings_mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop")

# Clean Discards (Native comlandr units are DISMT: Metric Tons)
comdisc_clean <- discards$comdisc |>
  as_tibble() |>
  mutate(NESPP3 = as.numeric(NESPP3)) |>
  left_join(spp_lookup, by = "NESPP3") |>
  filter(!is.na(COMMON_NAME)) |>
  clean_cfdbs_names() |>
  group_by(Year, JoinName) |>
  summarise(Com_Discards_mt = sum(DISMT, na.rm = TRUE), .groups = "drop")

# 4. Pull stocksmart Data for ALL species ---------------------------------
message("Aggregating stocksmart Total Catch data via get_latest_metrics()...")

# Fetch latest catch metrics (this returns a list)
ss_raw_list <- stocksmart::get_latest_metrics(metric = "Catch")

# Extract the data frame
ss_raw <- if ("data" %in% names(ss_raw_list)) {
  ss_raw_list$data
} else {
  ss_raw_list[[1]]
}

# Validate and mathematically convert units to ensure equivalence
# NOTE: v1.1.11 uses lowercase column names (units, value, stock_name, etc.)
unique_units <- unique(ss_raw$units)
message("--> StockSMART units detected: ", paste(unique_units, collapse = ", "))
message("--> Standardizing StockSMART units to Metric Tons...")

ss_clean <- ss_raw |>
  as_tibble() |>
  mutate(
    Units_Lower = tolower(units),
    # Dynamically convert complex units into Metric Tons
    Value_mt = case_when(
      grepl("thousand lbs|thousand pounds", Units_Lower) ~ (value * 1000) /
        2204.62,
      grepl("thousand mt|thousand metric tons", Units_Lower) ~ value * 1000,
      grepl("lbs|pound", Units_Lower) ~ value / 2204.62,
      grepl("mt|metric ton", Units_Lower) ~ value,
      grepl("kg|kilogram", Units_Lower) ~ value / 1000,
      TRUE ~ NA_real_
    )
  ) |>
  # Drop anything that couldn't be converted
  filter(!is.na(Value_mt)) |>
  filter(!grepl("Eastern Georges Bank", stock_name)) |>
  mutate(
    JoinName = tolower(common_name),
    JoinName = if_else(str_detect(JoinName, "skate"), "skate complex", JoinName)
  ) |>
  # Group by Stock AND Year so we evaluate the latest data year-by-year
  group_by(stock_name, year, JoinName) |>
  filter(assessment_year == max(assessment_year)) |>
  ungroup() |>
  # Aggregate stocks using the standardized JoinName
  group_by(year, JoinName) |>
  summarise(
    SS_Total_Catch_mt = sum(Value_mt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Rename lowercase 'year' back to 'Year' to match comlandr for the join step
  rename(Year = year)

# 5. Join Datasets --------------------------------------------------------
message("Merging landings, discards, and Stock SMART data...")

# Combine comlandr landings and discards
comlandr_total <- full_join(
  comlandr_clean,
  comdisc_clean,
  by = c("Year", "JoinName")
) |>
  mutate(
    Com_Landings_mt = replace_na(Com_Landings_mt, 0),
    Com_Discards_mt = replace_na(Com_Discards_mt, 0),
    Com_Total_Catch_mt = Com_Landings_mt + Com_Discards_mt
  )

# FULL JOIN with Stock SMART
hypothesis_df <- full_join(
  comlandr_total,
  ss_clean,
  by = c("Year", "JoinName")
) |>
  mutate(Species = str_to_title(JoinName)) |>
  filter(Year >= start_year) |>
  group_by(Species) |>
  filter(any(!is.na(SS_Total_Catch_mt)) & any(!is.na(Com_Total_Catch_mt))) |>
  ungroup() |>
  arrange(Species, Year)

message(sprintf(
  "Successfully matched %d species/complexes between datasets.",
  n_distinct(hypothesis_df$Species)
))

# 6. Visualizations -------------------------------------------------------
message(
  "Generating hypothesis-testing plots (with capped y-axes for readability)..."
)

pdf_path <- "data-raw/issue43_hypothesis_test.pdf"
pdf(pdf_path, width = 10, height = 8)

species_list <- unique(hypothesis_df$Species)
chunk_size <- 6

for (i in seq(1, length(species_list), by = chunk_size)) {
  target_spp <- species_list[i:min((i + chunk_size - 1), length(species_list))]

  p <- hypothesis_df |>
    filter(Species %in% target_spp) |>
    group_by(Species) |>
    mutate(Max_SS = max(SS_Total_Catch_mt, na.rm = TRUE)) |>
    ungroup() |>
    select(
      Year,
      Species,
      Max_SS,
      `1. comlandr Landings Only` = Com_Landings_mt,
      `2. comlandr (Landings + Discards)` = Com_Total_Catch_mt,
      `3. Stock SMART Total Catch` = SS_Total_Catch_mt
    ) |>
    pivot_longer(
      cols = starts_with(c("1", "2", "3")),
      names_to = "Data_Tier",
      values_to = "Catch_mt"
    ) |>
    # CAP OUTLIERS: If a data point exceeds 1.5x the max StockSMART catch, cap it.
    mutate(
      Catch_mt = if_else(Catch_mt > (Max_SS * 1.5), Max_SS * 1.5, Catch_mt)
    ) |>
    ggplot(aes(x = Year, y = Catch_mt, color = Data_Tier)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(~Species, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(
      title = "Hypothesis Test: Do Discards & Recreational Catch Explain the Gap?",
      subtitle = "Extreme outliers are visually capped at 1.5x the maximum Stock SMART catch to preserve the y-axis.",
      x = "Year",
      y = "Metric Tons",
      color = "Data Source"
    ) +
    theme(legend.position = "bottom")

  print(p)
}

dev.off()
message(sprintf("Plots saved to %s", pdf_path))

# 7. Generate Tabular Outputs & Isolate Overages --------------------------
message("Generating comparison tables and isolating comlandr overages...")

comparison_table <- hypothesis_df |>
  mutate(
    Total_Difference_mt = SS_Total_Catch_mt - Com_Total_Catch_mt,
    Landings_Difference_mt = SS_Total_Catch_mt - Com_Landings_mt,
    Percent_Diff = if_else(
      SS_Total_Catch_mt > 0,
      (Total_Difference_mt / SS_Total_Catch_mt) * 100,
      NA_real_
    )
  ) |>
  select(
    Species,
    Year,
    `Stock_SMART_Total_Catch_mt` = SS_Total_Catch_mt,
    `Comlandr_Landings_mt` = Com_Landings_mt,
    `Comlandr_Discards_mt` = Com_Discards_mt,
    `Comlandr_Total_Com_mt` = Com_Total_Catch_mt,
    Landings_Difference_mt,
    Total_Difference_mt,
    Percent_Diff
  ) |>
  mutate(across(where(is.numeric), ~ round(., 1))) |>
  arrange(Species, Year)

# Master Table
csv_path <- "data-raw/issue43_catch_comparison_table.csv"
write_csv(comparison_table, csv_path)

# Isolate anomalies where comlandr landings > StockSMART
comlandr_overages <- comparison_table |>
  filter(Comlandr_Landings_mt > Stock_SMART_Total_Catch_mt) |>
  arrange(Species, Landings_Difference_mt) # Sort ascending so largest overages (most negative) are at the top

overage_path <- "data-raw/issue43_comlandr_overages.csv"
write_csv(comlandr_overages, overage_path)

message(sprintf("Master comparison table saved to %s", csv_path))
message(sprintf(
  "Found %d instances where comlandr landings exceed StockSMART catch. Saved to %s",
  nrow(comlandr_overages),
  overage_path
))
