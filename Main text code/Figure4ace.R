################################################################################################
## Title: Land use optimization for food security, bioenergy, and forest conservation in Indonesia
## R script for creating Figure 4a, c, e 
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
df <- fread("Figure4ace.csv")

source_order_legend <- c("Sumatra", "Kalimantan", "Java", "Bali & Nusa Tenggara", 
                         "Maluku",  "Papua", "Sulawesi")

source_order <- c( "Maluku", "Bali & Nusa Tenggara",  "Java", "Sulawesi",
                        "Sumatra", "Papua","Kalimantan")

df$Island_Group <- factor(df$Island_Group, levels = source_order)

max_y_top <- 11000
min_y_top <- -200


# Theme
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
  "Sumatra" = "#BEB8EB",
  "Java" = "#A2BCE0",
  "Bali & Nusa Tenggara" = "#252323",
  "Kalimantan" = "#5A716A",
  "Papua" = "#084887",
  "Maluku" = "#9F4A54",
  "Sulawesi" = "#F5F2A7")

plot1 <- ggplot(df %>% filter(`PlotGroup` == "Plot1"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Concessions', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot2 <- ggplot(df %>% filter(`PlotGroup` == "Plot2"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Open Development', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))

plot3 <- ggplot(df %>% filter(`PlotGroup` == "Plot3"), aes(x = Scenario, y = kHa, fill = Island_Group)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Forest Protection', x = 'Scenario', y = 'kHa') +
  theme_bw() +
  scale_fill_manual(values = color_mapping) +
  theme_panel +
  scale_y_continuous(labels = scales::comma, limits = c(min_y_top, max_y_top))


combined_plot <- (plot1 + plot2 + plot3)  +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom') &
  scale_fill_manual(values = color_mapping, breaks = source_order_legend)


# Print
print(combined_plot)