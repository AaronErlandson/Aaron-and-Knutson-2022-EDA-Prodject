# Load data into R
library(readxl)
usa_cwd <- read_excel("C:/Users/Aaron Erlandson/Desktop/usa_cwd.xlsx")
View(usa_cwd)

# Scatter plot of total_positive_samples vs year
ggplot(data = usa_cwd) +
  geom_point(mapping =aes(x = year, y = total_positive_samples))

# Scatter plot of total_collected_samples vs year
ggplot(data = usa_cwd) +
  geom_point(mapping =aes(x = year, y = total_collected_samples))

# Bar chart of total_positive_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e")

# Bar chart of total_collected_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_collected_samples), fill = "#a6192e")

# Bar chart showing species vs total_positive_samples
ggplot(data = usa_cwd,
       mapping = aes(x = species)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e")

# Bar chart showing species vs total_collected_samples
ggplot(data = usa_cwd,
       mapping = aes(x = species)) +
  geom_col(mapping = aes(y = total_collected_samples), fill = "#a6192e")



# Bar chart showing state vs total_positive_samples vs states
ggplot(data = usa_cwd,
       mapping = aes(x = state)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  theme(axis.text.x = element_text(angle = 90, size = 10))

# Calculating Positivity sample
ps <- select(usa_cwd, total_collected_samples, total_positive_samples)
mutate(ps, positivity_sample = total_positive_samples / total_collected_samples)