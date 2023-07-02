# Load the necessary packages
library(dplyr)
library(Lahman)
library(ggplot2)
library(tidyr)
library(plotly)

# Filter the Teams dataset for the year 2022
teams_2022 <- Teams %>%
  filter(yearID == 2022)

# Select the desired variables
selected_vars <- c("teamID", "W", "SB", "HR", "ERA", "H", "FP")  # Add "FP" for Fielding Percentage

# Subset the teams data for the selected variables
teams_subset <- teams_2022 %>%
  select(all_of(selected_vars))

# Calculate the correlation matrix
cor_matrix <- cor(teams_subset[, -1])  # Exclude the teamID column

# Convert correlation matrix to long format
cor_matrix_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_matrix_long) <- c("Variable 1", "Variable 2", "Correlation")

# Calculate R-squared values
rsquared <- cor_matrix^2

# Create a lower triangular matrix
cor_matrix_lower <- cor_matrix_long %>%
  filter(as.numeric(`Variable 1`) > as.numeric(`Variable 2`)) %>%
  mutate(RSquared = rsquared[lower.tri(cor_matrix)])

# Create a heatmap of the correlation matrix with numbers, R-squared values, and variables
plot <- ggplot(data = cor_matrix_lower, aes(x = `Variable 1`, y = `Variable 2`, fill = Correlation,
                                            text = paste("Variable 1: ", `Variable 1`,
                                                         "<br>Variable 2: ", `Variable 2`,
                                                         "<br>R: ", round(Correlation, 2),
                                                         "<br>R-squared: ", round(RSquared, 2)))) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3, family = "Arial") +  # Add numbers inside the tiles
  scale_fill_gradient2(low = "#0099CC", mid = "white", high = "#8ECF8D", midpoint = 0,
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("-1", "0", "1")) +
  labs(x = NULL, y = NULL) +  # Remove axis labels
  ggtitle("MLB 2022 Correlation Matrix") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial", size = 16, face = "bold", hjust = 0.5),  # Center the title
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        axis.title = element_blank(),  # Remove axis title
        legend.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black", face = "bold"),  # Add legend title
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.background = element_rect(fill = "#FFCC99"),  # Set the background color to light goldenrod
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),  # Keep the legend background white with a grey border
        legend.key = element_rect(color = "black", fill = "#0099CC"),  # Style the legend key
        legend.key.size = unit(0.5, "cm"),  # Decrease the size of the legend key
        legend.key.width = unit(0.5, "cm"),  # Adjust the width of the legend key
        legend.key.height = unit(1.2, "cm"),  # Adjust the height of the legend key
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title = "R Coeff."))  # Update legend label to "R Coeff."

# Create the interactive plotly object
plotly_obj <- ggplotly(plot, tooltip = "text")

# Display the interactive plotly object
plotly_obj

# library(htmlwidgets)

# Convert plotly object to widget
# widget <- as_widget(plotly_obj)

# Save the widget as an HTML file
# saveWidget(widget, "correlation_matrix.html")
