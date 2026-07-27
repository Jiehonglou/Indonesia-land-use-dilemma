################################################################################################
## Title: Land use optimization for food security, bioenergy, and forest conservation in Indonesia
## R script for creating Figure 6  
## AUTHOR:            Claire Squire
## Date: 2025 Nov
################################################################################################

# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(showtext)

# Add Times New Roman
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# Load data
##df <- fread("Figure6.csv")

data$Scenario <- factor(
  data$Scenario,
  levels = c("B50", "B40", "B35", "Exporter", "Self-Sufficiency", "Reduced Imports")
)


data_sum <- data %>%
  group_by(Crop) %>%
  summarise(
    Mean = first(Mean),
    Min = first(Min),
    Max = first(Max)
  )


# Plot
color_mapping <- c(
  "B35" = "#99CC99",
  "B40" = "#0B310B",
  "B50" = "#5A716A",
  "Reduced Imports" = "#6F333D",
  "Self-Sufficiency" = "#B4CDED",
  "Exporter" = "#979551"
)

p <- ggplot(data, aes(x = Crop, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity") +
  
# Uncertainty range
  geom_errorbar(
    data = data_sum,
    inherit.aes = FALSE,
    aes(x = Crop, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = 1
  ) +
  
# Mean marker
  geom_point(
    data = data_sum,
    inherit.aes = FALSE,
    aes(x = Crop, y = Mean),
    shape = 16,   # diamond
    size = 4,
    color = "black"
  ) +
  
# Theme
  scale_fill_manual(values = color_mapping) +  
  theme_minimal(base_size = 14) +
  labs(
    title = "Yield Gap by Crop and Potential for Yield Improvement",
    x = "",
    y = "Demand gap in 2030\n(mtCPO or mtGKG)",  # multi-line y-axis label
    fill = "Scenario"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    line = element_line(colour = "black"),
    text = element_text(family = "Times New Roman", colour = "black", size = 14),
    axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
    axis.text.x = element_text(size = 12, colour = "black", face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.text = element_text(face = "bold", size = 12), 
    legend.title = element_text(face = "bold", size = 14)
  )


# Print
print(p)