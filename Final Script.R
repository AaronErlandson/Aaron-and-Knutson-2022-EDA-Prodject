#Thomas' stuff
library(googlesheets4)
library(tidyverse)
library(urbnmapr)

#Read google sheets data into R
cwdpositive <- read_sheet('1bazVSrrfMWM6scjJzAvDXBw-EeX2BpK1BI1FRdx5GCk') %>% 
  rename(permitarea=`Permit\nArea`, sampleacquisition=`Sample\nAcquisition`) %>% 
  mutate(permitarea=factor(permitarea), Year = as.integer(Year)) %>% 
  print()

distributionofcwdinunitedstates <- read_sheet('https://docs.google.com/spreadsheets/d/1AJFJeUI25nDAGxogKNd7oA70r1roLoU92hv9408OR6Q/edit#gid=0')

minnesotayearlysamplingdata <- read_sheet  ('https://docs.google.com/spreadsheets/d/1ABj6j_GxRa1dpEzdP5ipZHaDyJgaZrYWtITForcFfW4/edit#gid=0')                                            

#Aaron's data stuff
usa_cwd <- read_sheet("https://docs.google.com/spreadsheets/d/18nwJNkR9APotdyceGLIBKdA8ktHZqGjZSj1FsKNT7KY/edit#gid=0")

#This works MN Yearly Cases by Age
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Age))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Age")
ggsave(filename = "output/MN Yearly Cases by Age.png",width = 5, height = 3,
       units = "in", dpi = 400)

#This works MN Yearly Cases by Sex
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Sex))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sex")
ggsave(filename = "output/MN Yearly Cases by Sex.png",width = 5, height = 3,
       units = "in", dpi = 400)

#3 Graphs Thing samples, positive, positivity rate
minnesotayearlysamplingdata %>% 
  separate(Year, into = c("start year", "Year"), sep = "-") %>% 
  group_by(Year) %>% 
  summarise(Positive = sum(Positive), Samples = sum(Samples)) %>% 
  mutate(`Positivity Rate`=Positive/Samples) %>% 
  pivot_longer(cols = c(Samples, Positive, `Positivity Rate`), 
               names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable=as_factor(Variable)) %>% 
  ggplot(aes(x=Year, y=Value, fill = Variable))+
  geom_col()+
  facet_wrap(~Variable, scales = "free_y")+
  guides(fill ="none")+
  labs(x=NULL, y=NULL, title = "Total Samples Collected, Positive Cases collected, and Positivity Rate")
ggsave(filename = "output/MN Total Samples, Positive Cases, Positivity Rate.png",width = 7, height = 3,
       units = "in", dpi = 400)

#MN Yearly Cases by Permit Area
cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n, fill = permitarea))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Permit Area", fill = "Permit Area")
ggsave(filename = "output/MN Yearly Cases by Permit Area.png",width = 5, height = 3,
       units = "in", dpi = 400)

#Bar Chart sample acquisition
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = sampleacquisition))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Acquisition Method", fill = "Sample Acquisition", size = NULL)
ggsave(filename = "output/MN Yearly Cases by acquisition.png",width = 5, height = 3,
       units = "in", dpi = 400)

#This works Dots are small
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Collection Zones", fill = "Permit Area", size = NULL)
ggsave(filename = "output/MN Yearly Cases by Collection Zones.png",width = 5, height = 3,
       units = "in", dpi = 400)

#Setting up stuff for urbanmapr data
states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)



#Map of States infected Free Ranging
states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Free-ranging cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="State Wild Cervid Infections")+
  theme_grey(base_size = 24)
ggsave(filename = "output/State Wild Cervids.png",width = 8, height = 5,
       units = "in", dpi = 600)

#Map of captive cervids by state
states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Captive cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="Captive Cervid Infections")+
  theme_grey(base_size = 24)
ggsave(filename = "output/State Captive Cervids.png",width = 8, height = 5,
       units = "in", dpi = 600)
#Data for captive cases by county for national map
captivecountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1Bqnrw1BbulGervJO2zODaUC6wX49SRcCu2zdQ57sNXw/edit#gid=0')

#Data for wild cases by county for national map
wildcountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1kZsk7IcZDokeosX1HKxCdFl-Dp7uQa4y5KmcNyPrIoI/edit#gid=0')

#Wild County Data, works, just need correct size.
countydata <- counties %>%
  left_join(wildcountydata %>%
              mutate(Wild=1, County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(countydata, mapping = aes(long, lat, group = group, fill = Wild)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill="none")+
  labs(fill = "Wild Infections",
       title="Nationwide County Distribution of Infections in Wild Cervids")
theme_grey(base_size = 24)
ggsave(filename = "output/County Wild Cervids.png",width = 8, height = 5,
       units = "in", dpi = 600)

#Counties Captive, works
captive <- counties %>%
  left_join(captivecountydata %>%
              mutate( County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(captive, mapping = aes(long, lat, group = group, fill = Count)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Captive Infections",
       title="Captive Cervid Infections by County")+
  theme_grey(base_size = 24)
ggsave(filename = "output/County Captive Cervids.png",width = 8, height = 5,
       units = "in", dpi = 600)

#Aaron's graphs
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

# Bar chart of total_positive_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e")+
  labs(title = "Total Positive Samples per Year", y = "Total Positive Samples", x = "Year") 
ggsave(filename = "output/thomas total u.s. positive samples per year.png",width = 5, height = 3,
       units = "in", dpi = 400)

# Bar chart showing species vs total_collected_samples
ggplot(data = usa_cwd,
       mapping = aes(x = species)) +
  geom_col(mapping = aes(y = total_collected_samples), fill = "#a6192e")
ggsave(filename = "output/thomas total collected samples per year graph2.png",width = 8, height = 5,
       units = "in", dpi = 400)

# Bar chart of total_positive_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  labs(y = "Total Positive Samples", title = "Total Positive Samples Per Year", x = "Year")
ggsave(filename = "output/thomas total positive samples per year2.png",width = 5, height = 3,
       units = "in", dpi = 400)
