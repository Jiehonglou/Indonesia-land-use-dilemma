################################################################################################
## Title: Land use optimization for food security, bioenergy, and forest conservation in Indonesia
## R script for creating Figure 5  
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
df <- read.csv('C:/Users/csquire/Documents/Forests, fuel, food/input/Figure4.csv')

source_order_legend <- c("LUC - Primary Forest", "LUC - Secondary Forest", "LUC - Oil Palm", "LUC - Rice", 
                         "LUC - Agriculture",  "LUC - Other", "Clearing Fire", 
                         "Peat Drainage", "Production & Processing",
                          "Fuel Switching")

source_order_top <- c("Production & Processing", "LUC - Other", "Peat Drainage",  
                      "LUC - Agriculture", "LUC - Primary Forest", "Clearing Fire", "LUC - Secondary Forest", 
                      "LUC - Rice", "LUC - Oil Palm", "Fuel Switching")

source_order_middle <- c("Fuel Switching", "LUC - Oil Palm", "LUC - Rice",  
                         "LUC - Agriculture", "LUC - Other","Peat Drainage",
                         "Clearing Fire", "LUC - Primary Forest","Production & Processing", "LUC - Secondary Forest")


source_order_bottom <- c("Production & Processing", "LUC - Other", "Peat Drainage",  
                         "LUC - Agriculture", "LUC - Primary Forest", "Clearing Fire", "LUC - Secondary Forest",
                          "Fuel Switching")


df_top <- df %>% 
  filter(PlotGroup %in% c("Plot1", "Plot2", "Plot3")) %>% 
  mutate(Source = factor(Source, levels = source_order_top))

df_middle <- df %>% 
  filter(PlotGroup %in% c("Plot4", "Plot5", "Plot6")) %>% 
  mutate(Source = factor(Source, levels = source_order_middle),
         Scenario = factor(Scenario, levels = c("Reduced Imports", "Self-Sufficiency", "Exporter")))

df_bottom <- df %>% 
  filter(PlotGroup %in% c("Plot7", "Plot8", "Plot9")) %>% 
  mutate(Source = factor(Source, levels = source_order_bottom),
         Scenario = factor(Scenario, levels = c("B35/ Reduced Imports", "B40/ Self-Sufficiency", "B50/ Exporter")))

max_y_top <- 4800
min_y_top <- -400
max_y_middle <- 1600
min_y_middle <- -400
max_y_bottom <- 6400
min_y_bottom <- 0

# Define theme
theme_panel <- theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
  line = element_line(colour = "black"),
  text = element_text(family = "Times New Roman", colour = "black", size = 14),
  axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
  axis.text.x = element_text(size = 8, colour = "black", face = "bold", hjust = 0.5, vjust = 1),
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.title.x = element_text(size = 14, face = "bold"),
  panel.border = element_rect(colour = "black"),
  legend.text = element_text(face = "bold", size = 12), 
  legend.title = element_text(face = "bold", size = 14))


color_mapping <- c(
  "LUC - Agriculture" = "#864C78",
  "LUC - Secondary Forest" = "#99CC99",
  "LUC - Primary Forest" = "#0B310B",
  "Production & Processing" = "#5A716A",
  "Clearing Fire" = "#D9883D",
  "LUC - Other" = "#084887",
  "LUC - Rice" = "#979551",
  "Peat Drainage" = "#B4CDED",
  "LUC - Oil Palm" = "#6F333D",
  "Fuel Switching" = "black")

# Summarize Min and Max
data_sum <- df %>%
  group_by(PlotGroup, Scenario) %>%
  summarise(
    Min = min(Min, na.rm = TRUE),
    Max = max(Max, na.rm = TRUE),
    .groups = "drop"
  )


# Create nine individual plots
plot1 <- ggplot(df_top %>% filter(PlotGroup == "Plot1"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot1"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Oil Palm - Concessions', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot2 <- ggplot(df_top %>% filter(PlotGroup == "Plot2"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot2"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Oil Palm - Open Development', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot3 <- ggplot(df_top %>% filter(PlotGroup == "Plot3"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot3"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Oil Palm - Forest Protection', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot4 <- ggplot(df_middle %>% filter(PlotGroup == "Plot4"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot4"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Rice - Concessions', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_middle, max_y_middle))

plot5 <- ggplot(df_middle %>% filter(PlotGroup == "Plot5"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot5"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Rice - Open Development', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_middle, max_y_middle))

plot6 <- ggplot(df_middle %>% filter(PlotGroup == "Plot6"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot6"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Rice - Forest Protection', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_middle, max_y_middle))

plot7 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot7"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot7"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Joint - Concessions', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))

plot8 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot8"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot8"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Joint - Open Development', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))

plot9 <- ggplot(df_bottom %>% filter(PlotGroup == "Plot9"), aes(x = Scenario, y = mmtCO2e, fill = Source)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    data = data_sum %>% filter(PlotGroup == "Plot9"),
    inherit.aes = FALSE,  
    aes(x = Scenario, ymin = Min, ymax = Max),
    width = 0.3,
    color = "black",
    linewidth = .5
  ) +
  labs(title = 'Joint - Forest Protection', x = 'Scenario', y = expression(bold("MtCO"[2]*"e"))) +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_bottom, max_y_bottom))




# Combine all plots in a 3x3 grid
combined_plot <- (plot1 + plot2 + plot3) / (plot4 + plot5 + plot6) / (plot7 + plot8 + plot9) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", 
        plot.tag = element_text(face = "bold")) &  
  scale_fill_manual(values = color_mapping, breaks = source_order_legend)

# Add subplot labels with periods
combined_plot <- combined_plot + plot_annotation(
  tag_levels = "a", 
  tag_suffix = "."  
)

# Print
print(combined_plot)

