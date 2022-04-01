library(googlesheets4)
library(tidyverse)
#Lines 3 through 17 are Tutorial for google sheets
#Read google sheets data into R Tutorial for google sheets
x <- read_sheet('https://docs.google.com/spreadsheets/d/1J9-ZpmQT_oxLZ4kfe5gRvBs7vZhEGhSCIpNS78XOQUE/edit?usp=sharing')

#Reads data into R
df <- read_sheet('https://docs.google.com/spreadsheets/d/1J9-ZpmQT_oxLZ4kfe5gRvBs7vZhEGhSCIpNS78XOQUE/edit?usp=sharing')

#Prints the data
df

#Reads the data with Sheet ID into R
df <- read_sheet('1J9-ZpmQT_oxLZ4kfe5gRvBs7vZhEGhSCIpNS78XOQUE')

#Prints the data
df

#Read google sheets data into R
cwdpositive <- read_sheet('1bazVSrrfMWM6scjJzAvDXBw-EeX2BpK1BI1FRdx5GCk') %>% 
  rename(permitarea=`Permit\nArea`, sampleacquisition=`Sample\nAcquisition`) %>% 
  mutate(permitarea=factor(permitarea), Year = as.integer(Year)) %>% 
  print()

cwdtestingminnesota <- read_sheet('https://docs.google.com/spreadsheets/d/1ABj6j_GxRa1dpEzdP5ipZHaDyJgaZrYWtITForcFfW4/edit#gid=0')

wisconsindata  <- read_sheet('https://docs.google.com/spreadsheets/d/1a92jEHh5HxLY88B4ui8aZ7j140nuSk2W2FH43TLHYt4/edit#gid=0')

distributionofcwdinunitedstates <- read_sheet('https://docs.google.com/spreadsheets/d/1AJFJeUI25nDAGxogKNd7oA70r1roLoU92hv9408OR6Q/edit#gid=0')

minnesotayearlysamplingdata <- read_sheet  ('https://docs.google.com/spreadsheets/d/1ABj6j_GxRa1dpEzdP5ipZHaDyJgaZrYWtITForcFfW4/edit#gid=0')                                            

#This works
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Age))

#This works
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Sex))

cwdpositive %>%
  count(Year) %>%
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n))

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year))

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
  labs(x=NULL, y=NULL)
  
#These Permit Area and Sample Acquisition did not fill correctly
#Ask M %>% erkord
#This works, change n to something with a different name
cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n, fill = permitarea))

cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot(aes(x=Year, y = n, color = permitarea)) + 
  geom_point()+
  geom_line()+
  labs(y="Positive Cases")
  

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = `Sample\nAcqui~`))

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = `Sample\nAcqui~`), position = "dodge")

#This geom_bar is not great for this data set:
ggplot(data = minnesotayearlysamplingdata) + 
  geom_bar(mapping = aes(x = Year, fill = Positive))

#Scatter plot of Positive per Year
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive))

#This works but has typos in the data
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`))

#This also doesn't work correctly it continues to just give
#one "Zones Tested"
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, size = `Zones Tested`))

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

ccdf <- get_urbn_map(map = "territories_counties")

ccdf %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  scale_x_continuous(limits = c(-141, -55)) +
  scale_y_continuous(limits = c(24, 50)) +  
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Free-ranging cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="Distribution of infections in free ranging cervids")

states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Captive cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="Distribution of infections in captive cervids")
library(googlesheets4)
captivecountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1Bqnrw1BbulGervJO2zODaUC6wX49SRcCu2zdQ57sNXw/edit#gid=0')

wildcountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1kZsk7IcZDokeosX1HKxCdFl-Dp7uQa4y5KmcNyPrIoI/edit#gid=0')

countydata <- counties %>%
  left_join(wildcountydata %>%
              mutate(Wild=1, County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(countydata, mapping = aes(long, lat, group = group, fill = Wild)) +
  geom_polygon(color = "#ffffff", size = 0.15) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Wild Infections",
       title="Nationwide County Distribution of Infections in Wild Cervids")

captive <- counties %>%
  left_join(captivecountydata %>%
              mutate( County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(countydata, mapping = aes(long, lat, group = group, fill = Wild)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill="none") +
  labs(fill = "Captive Infections",
       title="Natonwide County Distribution of Infections in Captive Cervids")+
  theme_grey(base_size = 24)
ggsave(filename = "output/County Wild Cervids.png",width = 14, height = 9,
       units = "in", dpi = 600)
