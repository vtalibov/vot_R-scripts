# Load packages

packages <- c('tidyverse', 'ineq')

# Read data.
# Clean DiscoverX data for unique kinases.
# Mutants, phosphorylated forms, autoinhibited forms and complexes are discarded.

unique_data <- 
  read_csv('study_data_report.csv') %>% 
  filter(`DiscoveRx Gene Symbol` %in%
           read_csv('unique_kinases.csv')$`DiscoveRx Gene Symbol`)

# Quantify percent saturation/inhibition

unique_data$'Percent Saturation' <- 100 - unique_data$'Percent Control'

# Sort dataframe.

unique_data <- arrange(unique_data, 'Compound name', 'Percent Saturation')

# Quantify cumulative fractions and cumulative saturations. Used only to plot
# Lorenz curves.

lorenz_curve = data.frame(p = Lc(unique_data$'Percent Saturation')$p, 
                          L = Lc(unique_data$'Percent Saturation')$L)

# Gini coefficient and S-scores are quantified.

Gini_index <- Gini(unique_data$'Percent Saturation')

s1 <- as.double(sum(unique_data$'Percent Saturation' >= 99) / count(unique_data)) %>%
  round(3)

s10 <- as.double(sum(unique_data$'Percent Saturation' >= 90) / count(unique_data)) %>%
  round(3)

s35 <- as.double(sum(unique_data$'Percent Saturation' >= 65) / count(unique_data)) %>%
  round(3)

# Make figures

histogram_plot <- ggplot(unique_data, aes(unique_data$'Percent Saturation')) +
  geom_histogram(color = 'black', fill = 'light blue', bins = 20)

lorenz_plot = ggplot(lorenz_curve, aes(p, L)) + geom_line(color = 'blue') + 
  geom_point() + geom_abline(slope=1)
