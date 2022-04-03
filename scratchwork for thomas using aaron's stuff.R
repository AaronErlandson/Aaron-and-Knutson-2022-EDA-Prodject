# Load data into R
library(googlesheets4)
usa_cwd <- read_sheet("https://docs.google.com/spreadsheets/d/18nwJNkR9APotdyceGLIBKdA8ktHZqGjZSj1FsKNT7KY/edit#gid=0")
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
usa_cwd %>%
  group_by(state) %>% 
  summarise(total_positive_samples = sum(total_positive_samples)) %>% 
  arrange(desc(total_positive_samples)) %>% 
  mutate(state = as_factor(state)) %>% 
  ggplot(mapping = aes(x = state)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  theme_gray(base_size =24 )+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Total Number of Positive Samples", x = NULL)
ggsave(filename = "output/thomas states graph.png",width = 8, height = 5,
       units = "in", dpi = 400)



# Calculating Positivity sample
ps <- select(usa_cwd, total_collected_samples, total_positive_samples, state, year, species)
ps <- mutate(ps, positivity_sample = total_positive_samples / total_collected_samples)


Calculating 

#3 Graph Thing
usa_cwd %>% 
  group_by(year) %>% 
  summarise(Positive = sum(total_positive_samples), Samples = sum(total_collected_samples)) %>% 
  mutate(`Positivity Rate`=Positive/Samples) %>% 
  pivot_longer(cols = c(Samples, Positive, `Positivity Rate`), 
               names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable=as_factor(Variable)) %>% 
  ggplot(aes(x=year, y=Value, fill = Variable))+
  geom_col()+
  facet_wrap(~Variable, scales = "free_y")+
  guides(fill ="none")+
  labs(x=NULL, y=NULL, title = "Total Samples Collected, Positive Cases, and Positivity Rate")
ggsave(filename = "output/thomas USA Yearly Cases 3 graphs.png",width = 5, height = 3,
       units = "in", dpi = 400)

#positivity thing
ps <- select(usa_cwd, total_collected_samples, total_positive_samples, state, year, species)
ps <- mutate(ps, positivity_sample = total_positive_samples / total_collected_samples)
# 
usa_cwd %>%
  group_by(state) %>% 
  summarise(ps = sum(ps)) %>% 
  arrange(desc(ps)) %>% 
  mutate(state = as_factor(state)) %>% 
  ggplot(mapping = aes(x = state)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  theme_gray(base_size =24 )+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Positive Cases", x = NULL, title = "Total Number of Positive Samples by State")
ggsave(filename = "output/thomas positivity rate states graph.png",width = 8, height = 5,
       units = "in", dpi = 400)

# Bar chart of total_positive_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e")+
  labs(title = "Total Positive Samples per Year", y = "Total Positive Samples", x = "Year") 
ggsave(filename = "output/thomas total u.s. positive samples per year.png",width = 5, height = 3,
       units = "in", dpi = 400)

#This works Dots are small
ggplot(data = usa_cwd) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`, size = 2))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Collection Zones", fill = "Permit Area", size = NULL)
ggsave(filename = "output/MN Yearly Cases by Collection Zones.png",width = 5, height = 3,
       units = "in", dpi = 400)
