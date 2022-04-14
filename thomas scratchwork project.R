library(googlesheets4)
library(tidyverse)


#Read google sheets data into R
cwdpositive <- read_sheet('1bazVSrrfMWM6scjJzAvDXBw-EeX2BpK1BI1FRdx5GCk') %>% 
  rename(permitarea=`Permit\nArea`, sampleacquisition=`Sample\nAcquisition`) %>% 
  mutate(permitarea=factor(permitarea), Year = as.integer(Year)) %>% 
  print()


distributionofcwdinunitedstates <- read_sheet('https://docs.google.com/spreadsheets/d/1AJFJeUI25nDAGxogKNd7oA70r1roLoU92hv9408OR6Q/edit#gid=0')

minnesotayearlysamplingdata <- read_sheet  ('https://docs.google.com/spreadsheets/d/1ABj6j_GxRa1dpEzdP5ipZHaDyJgaZrYWtITForcFfW4/edit#gid=0')                                            

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

cwdpositive %>%
  count(Year) %>%
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n))

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year))
#3 Graph Thing
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


#This works, MN Yearly Cases by Permit Area
cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n, fill = permitarea))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Permit Area", fill = "Permit Area")
ggsave(filename = "output/MN Yearly Cases by Permit Area.png",width = 5, height = 3,
       units = "in", dpi = 400)

#Scatter Plot with Lines MN Yearly Cases by Permit Area, Works not using
cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot(aes(x=Year, y = n, color = permitarea)) + 
  geom_point()+
  geom_line()+
  labs(y="Positive Cases")
  
#Bar Chart sample acquisition
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = sampleacquisition))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Acquisition Method", fill = "Sample Acquisition", size = NULL)
ggsave(filename = "output/MN Yearly Cases by acquisition.png",width = 5, height = 3,
       units = "in", dpi = 400)

#This geom_bar is not great for this data set:
ggplot(data = minnesotayearlysamplingdata) + 
  geom_bar(mapping = aes(x = Year, fill = Positive))

#Scatter plot of Positive per Year
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive))

#This works Dots are small
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Collection Zones", fill = "Permit Area", size = NULL)
ggsave(filename = "output/MN Yearly Cases by Collection Zones.png",width = 5, height = 3,
       units = "in", dpi = 400)

#Bar Chart of Zones Tested
minnesotayearlysamplingdata %>%
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = Positive, fill = `Zones Tested`))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Collection Zone", fill = "Zones Tested",)
ggsave(filename = "output/MN Yearly Cases by Sample Collection Zone.png",width = 5, height = 3,
       units = "in", dpi = 400)


#This doesn't work
ggplot(data = cwdpositive) + 
  geom_point(mapping = aes(x = Year, shape = Age, color = Age))

#This stuff is basically good for now
library(tidyverse)
library(urbnmapr)

states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

#This works, States infected Free Ranging
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
library(googlesheets4)
captivecountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1Bqnrw1BbulGervJO2zODaUC6wX49SRcCu2zdQ57sNXw/edit#gid=0')

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
