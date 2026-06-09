# =========================================================================
# Title: Compare comlandr Commercial Landings vs stocksmart Assessment Data
# 
# Purpose: Investigate discrepancies between comlandr landings and stock 
#          assessment data across ALL overlapping species (Issue #43).
# Hypothesis: Differences are driven by discards, recreational landings, 
#             and geographic area mismatches.
# =========================================================================

# 1. Setup & Load Libraries -----------------------------------------------
library(comlandr)
library(stocksmart)
library(tidyverse)
library(data.table)
library(dbutils)
library(DBI)

start_year <- 1985
end_year   <- as.integer(format(Sys.Date(), "%Y")) - 1

message(sprintf("Starting full species comparison: %d-%d", start_year, end_year))

# Ensure the target directory exists for our outputs
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# 2. Pull Data from NEFSC Database ----------------------------------------
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

# Create a master area map covering all possible US statistical areas (1 to 999)
# This captures the South Atlantic and prevents memory-crashing alphanumeric NAFO areas
coastwide <- data.table(AREA = 1:999, EPU = "Coastwide")
coastwide[, NESPP3 := 1]
coastwide[, MeanProp := 1]

message("Pulling Coastwide landings from comlandr...")
landings <- comlandr::get_comland_data(
  channel,
  filterByYear = start_year:end_year,
  userAreas    = coastwide,  # Restoring the filter to save memory
  aggGear      = TRUE,
  aggArea      = TRUE
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
message("Standardizing comlandr species names...")

# Helper function to avoid repeating the exact same name cleaning logic
clean_cfdbs_names <- function(df) {
  df |>
    mutate(
      JoinName = tolower(COMMON_NAME),
      JoinName = str_replace_all(JoinName, "[,\\.]", " "),
      JoinName = str_squish(JoinName),
      JoinName = case_when(
        JoinName == "sea bass black"      ~ "black sea bass",
        JoinName == "flounder summer fluke" ~ "summer flounder",
        JoinName == "flounder summer"     ~ "summer flounder",
        JoinName == "flounder winter"     ~ "winter flounder",
        JoinName == "flounder yellowtail" ~ "yellowtail flounder",
        JoinName == "flounder windowpane" ~ "windowpane",
        JoinName == "hake silver whiting" ~ "silver hake",
        JoinName == "hake silver"         ~ "silver hake",
        JoinName == "hake atlantic red"   ~ "red hake",
        JoinName == "hake atlantic white" ~ "white hake",
        JoinName == "cod atlantic"        ~ "atlantic cod",
        JoinName == "pollock atlantic"    ~ "pollock",
        JoinName == "goosefish monkfish"  ~ "monkfish",
        JoinName == "scallop sea"         ~ "sea scallop",
        JoinName == "skate little"        ~ "little skate",
        JoinName == "skate winter"        ~ "winter skate",
        JoinName == "skate barndoor"      ~ "barndoor skate",
        JoinName == "skate thorny"        ~ "thorny skate",
        JoinName == "skate smooth"        ~ "smooth skate",
        JoinName == "skate clearnose"     ~ "clearnose skate",
        JoinName == "skate rosette"       ~ "rosette skate",
        TRUE ~ JoinName
      )
    ) |>
    rename(Year = YEAR)
}

# Clean Landings
comlandr_clean <- landings$comland |>
  as_tibble() |>
  mutate(NESPP3 = as.numeric(NESPP3)) |> 
  left_join(spp_lookup, by = "NESPP3") |>
  filter(!is.na(COMMON_NAME)) |>
  group_by(YEAR, COMMON_NAME) |>
  summarise(Com_Landings_mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  clean_cfdbs_names()

# Clean Discards
comdisc_clean <- discards$comdisc |>
  as_tibble() |>
  mutate(NESPP3 = as.numeric(NESPP3)) |> 
  left_join(spp_lookup, by = "NESPP3") |>
  filter(!is.na(COMMON_NAME)) |>
  group_by(YEAR, COMMON_NAME) |>
  summarise(Com_Discards_mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  clean_cfdbs_names()

# 4. Pull stocksmart Data for ALL species ---------------------------------
message("Aggregating stocksmart Total Catch data...")
ss_clean <- stocksmart::stockAssessmentData |>
  filter(Metric == "Catch", grepl("mt|metric ton", tolower(Units))) |>
  filter(AssessmentType == "Operational") |>
  # Drop sub-stocks that are already accounted for in broader stock assessments
  filter(!grepl("Eastern Georges Bank", StockName)) |> 
  group_by(StockName) |>
  filter(AssessmentYear == max(AssessmentYear)) |>
  ungroup() |>
  group_by(Year, CommonName) |>
  summarise(SS_Total_Catch_mt = sum(Value, na.rm = TRUE), .groups = "drop") |>
  mutate(JoinName = tolower(CommonName))

# 5. Join Datasets (The Data Staircase) -----------------------------------
message("Merging landings, discards, and Stock SMART data...")

# First, combine comlandr landings and discards
comlandr_total <- full_join(comlandr_clean, comdisc_clean, by = c("Year", "COMMON_NAME", "JoinName")) |>
  mutate(
    Com_Landings_mt = replace_na(Com_Landings_mt, 0),
    Com_Discards_mt = replace_na(Com_Discards_mt, 0),
    Com_Total_Catch_mt = Com_Landings_mt + Com_Discards_mt
  )

# Next, FULL JOIN with Stock SMART so we don't drop unmatched years
hypothesis_df <- full_join(comlandr_total, ss_clean, by = c("Year", "JoinName")) |>
  mutate(Species = str_to_title(JoinName)) |>
  filter(Year >= start_year) |>
  # Filter to only keep species that exist in BOTH databases at some point
  group_by(Species) |>
  filter(any(!is.na(SS_Total_Catch_mt)) & any(!is.na(Com_Total_Catch_mt))) |>
  ungroup() |>
  # Ensure NAs in plotting columns don't break the lines
  arrange(Species, Year)

message(sprintf("Successfully matched %d species between datasets.", n_distinct(hypothesis_df$Species)))
# 6. Visualizations (Testing the Hypothesis) ------------------------------
message("Generating hypothesis-testing plots...")

pdf_path <- "data-raw/issue43_hypothesis_test.pdf"
pdf(pdf_path, width = 10, height = 8)

species_list <- unique(hypothesis_df$Species)
chunk_size <- 6

for (i in seq(1, length(species_list), by = chunk_size)) {
  target_spp <- species_list[i:min((i + chunk_size - 1), length(species_list))]
  
  p <- hypothesis_df |>
    filter(Species %in% target_spp) |>
    select(Year, Species, 
           `1. comlandr Landings Only` = Com_Landings_mt, 
           `2. comlandr (Landings + Discards)` = Com_Total_Catch_mt, 
           `3. Stock SMART Total Catch` = SS_Total_Catch_mt) |>
    pivot_longer(cols = starts_with(c("1", "2", "3")), 
                 names_to = "Data_Tier", values_to = "Catch_mt") |>
    ggplot(aes(x = Year, y = Catch_mt, color = Data_Tier)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(~Species, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(title = "Hypothesis Test: Do Discards & Recreational Catch Explain the Gap?",
         subtitle = "The gap between Line 2 and Line 3 represents missing Recreational/State Catch",
         x = "Year", y = "Metric Tons", color = "Data Source") +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()
message(sprintf("Plots saved to %s", pdf_path))

# 7. Generate Tabular Output ----------------------------------------------
message("Generating comparison table...")

comparison_table <- hypothesis_df |>
  mutate(
    # The gap between Stock SMART and total commercial catch
    Difference_mt = SS_Total_Catch_mt - Com_Total_Catch_mt,
    # Percentage difference relative to the Stock SMART baseline
    Percent_Diff = if_else(SS_Total_Catch_mt > 0, 
                           (Difference_mt / SS_Total_Catch_mt) * 100, 
                           NA_real_)
  ) |>
  select(
    Species,
    Year,
    `Stock_SMART_Total_Catch_mt` = SS_Total_Catch_mt,
    `Comlandr_Landings_mt`       = Com_Landings_mt,
    `Comlandr_Discards_mt`       = Com_Discards_mt,
    `Comlandr_Total_Com_mt`      = Com_Total_Catch_mt,
    Difference_mt,
    Percent_Diff
  ) |>
  # Round to 1 decimal place for clean viewing in Excel/CSV
  mutate(across(where(is.numeric), ~round(., 1))) |>
  arrange(Species, Year)

csv_path <- "data-raw/issue43_catch_comparison_table.csv"
write_csv(comparison_table, csv_path)
message(sprintf("Comparison table saved to %s", csv_path))