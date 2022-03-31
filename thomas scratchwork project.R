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
  geom_bar(mapping = aes(x = Year, fill = 'Permit Area'))

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = 'Sample Acquisition'))

ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = 'Sample Acquisition'), position = "dodge")

#This geom_bar is not great for this data set:
ggplot(data = minnesotayearlysamplingdata) + 
  geom_bar(mapping = aes(x = Year, fill = Positive))

#Scatter plot of Positive per Year
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive))

#Again this doesn't work and makes all zones red?
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = 'Zones Tested'))

#This also doesn't work correctly it continues to just give
#one "Zones Tested"
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, size = 'Zones Tested'))
