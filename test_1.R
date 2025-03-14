library(tidyverse)

# Load the simulation txt file
data <- readLines("C:/Users/carlo/OneDrive/Documents/R/NetFlux/test_sim_1.txt")

# The original data is separated by space
split_data <- strsplit(data, "\\s+")

# Tranform the data to Data Frame format
df <- do.call(rbind, lapply(split_data, function(x) as.data.frame(t(x))))

# First row is actually the column names
colnames(df) <- df[1, ]

# Removing the redundant first row
df <- df[-1, ]

# Removinf the "Ts" from the time points (from T0.1, T0.2 to 0.1, 0.2, for example)
colnames(df) <- sub("T", "", colnames(df))

# Change data frame to a format (every specie will have its own columns and time point will be all in one column)
df_reshaped <- df %>%
  pivot_longer(
    cols = -Species_Name,       # Columns to pivot, excluding "Species"
    names_to = "Time",     # Name for the new column containing time points
    values_to = "Value"    # Name for the new column containing the values
  ) %>% pivot_wider(
    names_from = Species_Name,  # Column to spread into separate columns
    values_from = Value    # Column containing the values to spread
  )

#Change to Numeric
df_final <- df_reshaped %>% mutate(across(everything(), as.numeric))

# Plots
ggplot((df_final)) + 
  geom_line(aes(x = Time, y = A, color = "A"), alpha = 0.3, size = 1.0) + 
  geom_line(aes(x = Time, y = B, color = "B"),alpha = 0.3, size = 1.0, position = position_nudge(x = 1)) + 
  geom_line(aes(x = Time, y = E, color = "E"), size = 1.0) + theme_bw() + 
  scale_color_manual(
    name = "Species",
    values = c("A" = "red", "B" = "green", "E" = "black")
  ) + 
  labs(x = "Time (a.u.)", y = "Activity", title = "NetFlux simulation example") + 
  theme(legend.position = c(0.925, 0.88), legend.background = element_rect(fill = "white", color = "black")) + 
  theme(axis.title.x = element_text(size = 16)) + theme(axis.title.y = element_text(size = 16)) + theme(title = element_text(size = 16))
