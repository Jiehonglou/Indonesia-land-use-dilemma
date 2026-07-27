################################################################################################
## Title: Land use optimization for food security, bioenergy, and forest conservation in Indonesia
## R script for calculating Supplementary Figure 1  
## AUTHOR:            Claire Squire
## Date: 2025 Nov
################################################################################################

##Oil palm
library(dplyr)

data <- merged_data_clean

total_area_ha <- sum(data$Total_Area, na.rm = TRUE)
total_prod <- sum(data$Prod_tons_2030, na.rm = TRUE)

if (!"Score" %in% colnames(data)) stop("Column 'Score' not found")
data$Score <- as.numeric(data$Score)

# Set the total production target
target_production <- 17867000  # in tons

# Example: province shares (adjust these as needed)
# Names must match province identifiers in your data (e.g., province code or number)
province_shares <- c(
  "Aceh" = 0.0329,
  "Sumatera Utara" = 0.0981,
  "Sumatera Barat" = 0.0291,
  "Riau" = 0.1944,
  "Jambi" = 0.0721,
  "Sumatera Selatan" = .0780,
  "Bengkulu" = 0.0239,
  "Lampung" = 0.0136,
  "Kepulauan Bangka Belitung" = 0.0161,
  "Kepulauan Riau" = 0.0005,
  "Jawa Barat" = 0.001,
  "Banten" = 0.0013,
  "Kalimantan Barat" = 0.1315,
  "Kalimantan Tengah" = 0.1277,
  "Kalimantan Selatan" = 0.0331,
  "Kalimantan Timur" = 0.0889,
  "Kalimantan Utara" = 0.0141,
  "Sulawesi Tengah" = 0.0091,
  "Sulawesi Selatan" = 0.0032,
  "Sulawesi Tenggara" = 0.0046,
  "Gorontalo" = 0.0009,
  "Sulawesi Barat" = 0.0102,
  "Maluku" = 0.0007,
  "Maluku Utara" = 0.0003,
  "Papua Barat" = 0.0040,
  "Papua" = 0.0107,
  "DI Yogyakarta" = 0.0,
  "Bali" = 0.0,
  "Dki Jakarta"  = 0.0,
  "Nusa Tenggara Barat" = 0.0,
  "Nusa Tenggara Timur"  = 0.0,
  "Jawa Timur" = 0.0,
  "Jawa Tengah"= 0.0,
  "Sulawesi Utara" = 0.0
)

# Normalize in case shares don’t add up exactly to 1
province_shares <- province_shares / sum(province_shares)

# Debug: check matches
cat("Unique province names in data:\n")
print(unique(data$ADM1_EN))
cat("\nProvince shares in script:\n")
print(names(province_shares))

selected_plots <- data.frame()

# Loop through provinces
for (prov in names(province_shares)) {
  
  prov_data <- data %>% filter(trimws(ADM1_EN) == prov)
  
  cat("\nProcessing province:", prov, " — found", nrow(prov_data), "plots\n")
  
  if (nrow(prov_data) == 0) next
  
  prov_target <- target_production * province_shares[[prov]]
  cumulative_production <- 0
  prov_selected <- data.frame()
  
  for (rank in sort(unique(prov_data$Score), decreasing = TRUE)) {
    rank_data <- prov_data %>%
      filter(Score == rank) %>%
      arrange(desc(Productivity2030))
    
    if (nrow(rank_data) == 0) next
    
    for (i in 1:nrow(rank_data)) {
      plot_production <- rank_data$Prod_tons_2030[i]
      plot_area <- rank_data$Total_Area[i]
      
      if (is.na(plot_production) || is.na(plot_area)) next
      
      if (cumulative_production + plot_production > prov_target) {
        remaining_production <- prov_target - cumulative_production
        rank_data$Prod_tons_2030[i] <- remaining_production
        rank_data$Area_used[i] <- remaining_production / plot_production * plot_area
        prov_selected <- bind_rows(prov_selected, rank_data[i, ])
        cumulative_production <- prov_target
        break
      } else {
        cumulative_production <- cumulative_production + plot_production
        rank_data$Area_used[i] <- plot_area
        prov_selected <- bind_rows(prov_selected, rank_data[i, ])
      }
    }
    if (cumulative_production >= prov_target) break
  }
  
  cat("✅", prov, "selected:", nrow(prov_selected),
      "rows — production:", sum(prov_selected$Prod_tons_2030, na.rm = TRUE), "\n")
  
  selected_plots <- bind_rows(selected_plots, prov_selected)
}

cat("\nTOTAL SELECTED PLOTS:", nrow(selected_plots), "\n")

# Export if non-empty
if (nrow(selected_plots) > 0) {
  write.csv(selected_plots,
            "C:/Users/csquire/Documents/Forests, fuel, food/output/def_opB50_by_province.csv",
            row.names = FALSE)
  cat("\n✅ CSV exported successfully.\n")
} else {
  cat("\n⚠️ No plots selected — check province names or Score/Productivity columns.\n")
}



##Rice

library(tidyr)
library(dplyr)

rice_prod_new <- read.csv("C:/Users/csquire/Documents/Forests, fuel, food/Input/rice_prod_new.csv")
gapcoeff <- read.csv("C:/Users/csquire/Documents/Forests, fuel, food/Input/gapcoeff_withdummy.csv")

gapcoeffnodummy <- read.csv("C:/Users/csquire/Documents/Forests, fuel, food/Input/gapcoeff.csv")


avg_yield <- gapcoeffnodummy %>%
  group_by(ADM1_EN, MANAGEMENT) %>%
  summarise(
    Average_Yield = mean(Potentialyield, na.rm = TRUE),
    Min_Yield = min(Potentialyield, na.rm = TRUE),
    Max_Yield = max(Potentialyield, na.rm = TRUE),
    .groups = "drop"
  )


# Merge
merged_df <- avg_yield %>%
  left_join(rice_prod_new, by = c("ADM1_EN", "MANAGEMENT"))


# Yield range
yieldbyprov <- merged_df %>%
  group_by(ADM1_EN) %>%
  summarise(
    Total_Yield = sum(Average_Yield * Share * Area23, na.rm = TRUE),
    .groups = "drop"
  )

minyieldbyprov <- merged_df %>%
  group_by(ADM1_EN) %>%
  summarise(
    Total_Yield = sum(Min_Yield * Share * Area23, na.rm = TRUE),
    .groups = "drop"
  )

maxyieldbyprov <- merged_df %>%
  group_by(ADM1_EN) %>%
  summarise(
    Total_Yield = sum(Max_Yield * Share * Area23, na.rm = TRUE),
    .groups = "drop"
  )