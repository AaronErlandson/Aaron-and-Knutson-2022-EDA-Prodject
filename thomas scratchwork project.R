library(googlesheets4)

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
cwdpositive <- read_sheet('1bazVSrrfMWM6scjJzAvDXBw-EeX2BpK1BI1FRdx5GCk')

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

#These Permit Area and Sample Acquisition did not fill correctly
#Ask Merkord
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = `Permit\nArea`))

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

#Again this doesn't work and makes all zones red?
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`))

#This also doesn't work correctly it continues to just give
#one "Zones Tested"
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, size = `Zones Tested`))

#This doesn't work
ggplot(data = cwdpositive) + 
  geom_point(mapping = aes(x = Year, size = Age))

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
  labs(fill = "Captive Infections",
       title="Natonwide County Distribution of Infections in Captive Cervids")
