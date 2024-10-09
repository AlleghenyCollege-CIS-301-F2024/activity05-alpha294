# Manova demo using Skulls dataset.

# Alpha Bah

rm(list = ls()) # clear out the variables from memory to make a clean execution of the code.

# If you want to remove all previous plots and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console

# Load necessary libraries
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

if(!require('HSAUR2')) {
  install.packages('HSAUR2')
  library('HSAUR2')
}

# The skulls data concerns measurements made on Egyptian skulls from five epochs.
data("skulls") # use this data set for proceeding code
names(skulls) # get the variables
summary(skulls) # summary of the data

##################################
# Plotting to visualize the means.
##################################

# Calculate the means of each column by epoch
means_by_epoch <- skulls %>%
  group_by(epoch) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Reshape the data for plotting
means_long <- means_by_epoch %>%
  pivot_longer(cols = -epoch, names_to = "measurement", values_to = "mean_value")

# Create the plot
ggplot(means_long, aes(x = epoch, y = mean_value, fill = measurement)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Values of Skull Measurements by Epoch",
       x = "Epoch",
       y = "Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# MANOVA
##################################

# Prepare the data for MANOVA
# Response variables (skull measurements)
response_vars <- skulls[, 1:4]  # Select the first 4 columns (these are the measurements)

# Grouping variable (epochs)
grouping_var <- skulls$epoch

# Perform MANOVA
manova_fit <- manova(as.matrix(response_vars) ~ grouping_var)

# Display MANOVA summary
summary(manova_fit)

# Wilks' Lambda test for MANOVA (to test for significant differences between groups)
summary(manova_fit, test = "Wilks")

##################################
# Interpretation
##################################
# The MANOVA results tell us whether the skull measurements (response variables)
# significantly differ across the epochs (grouping variable).
# The Wilks' Lambda test provides the p-value to assess significance.
