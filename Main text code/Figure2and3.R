################################################################################################
## Title: Land use optimization for food security, bioenergy, and forest conservation in Indonesia
## R script for creating Figure 2a, e, i and Figure 3a, e, i  
## AUTHOR:            Claire Squire
## Date: 2025 Nov
################################################################################################

# Load libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(patchwork)
library(showtext)

# Add Times New Roman
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# Read data
df <- fread("Figure2aeiFigure3aei.csv")

source_order_legend <- c("Sumatra", "Kalimantan", "Java", "Bali & Nusa Tenggara", 
                         "Maluku",  "Papua", "Sulawesi")

source_order_top <- c( "Maluku", "Bali & Nusa Tenggara",  "Papua", "Sulawesi",
                        "Java", "Sumatra","Kalimantan")

source_order_bottom <- c("Sulawesi", "Maluku", "Bali & Nusa Tenggara", "Java",  "Kalimantan", 
                         "Sumatra","Papua")

df_top <- df %>% 
  filter(PlotGroup %in% c("Plot1", "Plot2", "Plot3")) %>% 
  mutate(Island_Group = factor(Island_Group, levels = source_order_top))

df_bottom <- df %>% 
  filter(PlotGroup %in% c("Plot4", "Plot5", "Plot6")) %>% 
  mutate(Island_Group = factor(Island_Group, levels = source_order_bottom),
         Scenario = factor(Scenario, levels = c("Reduced Imports", "Self-Sufficiency", "Exporter"))) # Reorder Scenario

max_y_top <- 9000
min_y_top <- -200
max_y_bottom <- 2500
min_y_bottom <- -200

# Theme
theme_panel <- theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
  line = element_line(colour = "black"),
  text = element_text(family = "Times New Roman", colour = "black", size = 14),
  axis.text.y = element_text(size = 10, colour = "black", face = "bold"),
  axis.text.x = element_text(size = 7, colour = "black", face = "bold", hjust = 0.5, vjust = 1),
  axis.title.y = element_text(size = 12, face = "bold"), 
  axis.title.x = element_text(size = 12, face = "bold"),
  panel.border = element_rect(colour = "black"),
  legend.text = element_text(face = "bold", size = 12), 
  legend.title = element_text(face = "bold", size = 14))


color_mapping <- c(
  "Sumatra" = "#BEB8EB",
  "Java" = "#A2BCE0",
  "Bali & Nusa Tenggara" = "#252323",
  "Kalimantan" = "#5A716A",
  "Papua" = "#084887",
  "Maluku" = "#9F4A54",
  "Sulawesi" = "#F5F2A7")

# Create six individual plots
plot1 <- ggplot(df_top %>% filter(PlotGroup == "Plot1"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Oil Palm - Concessions', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot2 <- ggplot(df_top %>% filter(PlotGroup == "Plot2"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Oil Palm - Open Development', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot3 <- ggplot(df_top %>% filter(PlotGroup == "Plot3"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Oil Palm - Forest Protection', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot4 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot4"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Rice - Concessions', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))

plot5 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot5"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Rice - Open Development', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))

plot6 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot6"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Rice - Forest Protection', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))

# Combine all plots in a 2x3 grid
combined_plot <- (plot1 + plot2 + plot3) / (plot4 + plot5 + plot6) +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom') &
  scale_fill_manual(values = color_mapping, breaks = source_order_legend)

# Print
print(combined_plot)