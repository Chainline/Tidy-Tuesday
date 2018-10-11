library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

voter_turnout <- read_csv("R/Tidy Tuesday/Week 28 - Voter Turnout/voter_turnout.txt")

names(voter_turnout)

pres.election.years <- seq(1980, 2012, by = 4)

voter_turnout.v2 <- voter_turnout %>%
  select(-X1)%>%
  mutate(turnout = (votes/eligible_voters)*100,
         cycle = ifelse(year %in% pres.election.years, "Presidential", "Midterm"),
         winning_party = ifelse(year %in% c(1980, 1984, 1988, 1994, 2000, 2002, 2004, 2010, 2014), 
                                "Republican", "Democrat"))

voter_turnout.v3 <- voter_turnout.v2 %>%
  filter(state != "United States")

ggplot(data = voter_turnout.v3, aes(x = year, y = turnout, col = cycle))+
  geom_point()+
  geom_smooth(se=F)+
  ylab("Voter Turnout (%)")+
  xlab("Election Year")+
  scale_x_continuous(limits = c(1980, 2014), breaks = seq(1980, 2016, by=4))+
  ggtitle("Voter turnout in each state from 1980 to 2014")+
  scale_color_discrete(name = "Election Cycle")+
  theme(panel.grid.minor.x = element_blank())+
  scale_y_continuous(limits = c(min(voter_turnout.v3$turnout),80), breaks = seq(25,80,5))+
  geom_vline(xintercept = c(1980, 1984, 1988, 1994, 2000, 2002, 2004, 2010, 2014), colour="red", 
             alpha = 0.3, linetype = "dashed")+
  geom_vline(xintercept = c(1982, 1986, 1990, 1992, 1996, 1998, 2006, 2008, 2012), colour="blue", 
             alpha = 0.3, linetype = "dashed")+
  labs(caption = "*Red vertically dashed lines indicate Republican vitcory during that year, while blue vertically dashed lines indicate Democratic victory.
  During midterms, victory is considered a net gain of Congressional seats")+
  theme(plot.caption = element_text(size = 8, hjust = 0))
  
  
  
  
  
  

  