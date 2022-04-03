#3 Graph Thing
minnesotayearlysamplingdata %>% 
  separate(Year, into = c("start year", "Year"), sep = "-") %>% 
  group_by(Year) %>% 
  summarise(Positive = sum(Positive), Samples = sum(Total)) %>% 
  mutate(`Positivity Rate`=Positive/Samples) %>% 
  pivot_longer(cols = c(Samples, Positive, `Positivity Rate`), 
               names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable=as_factor(Variable)) %>% 
  ggplot(aes(x=Year, y=Value, fill = Variable))+
  geom_col()+
  facet_wrap(~Variable, scales = "free_y")+
  guides(fill ="none")+
  labs(x=NULL, y=NULL, title = "Total Samples Collected, Positive Cases, and Positivity Rate")
ggsave(filename = "output/MN Yearly Cases by Sex.png",width = 5, height = 3,
       units = "in", dpi = 400)
