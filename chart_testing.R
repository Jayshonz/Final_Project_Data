library(data.table)
library(janitor)
library(readr)
library(readxl)
library(datapasta)
library(tidyr)
library(gt)
library(tidyverse)

g20 <- read_csv("raw/g20_financial_indicators.csv") %>% clean_names()
pop <- read_csv("raw/country_pop.csv", skip=1) %>% clean_names() %>% setnames(old=c("x2"), new=c("country"))

financial_inclucsion <- list("Deposit accounts per 1,000 adults",	
                             "Retail cashless transactions per 1,000 adults",
                             "Outstanding loans per 1,000 adults")


inclusion_df <- g20 %>% 
  filter(series_name %in% financial_inclucsion) %>% 
  setnames(old=c("x2011_yr2011","x2012_yr2012", "x2013_yr2013","x2014_yr2014", "x2015_yr2015", 
                 "x2016_yr2016", "x2017_yr2017"), new=c("2011", "2012", "2013", "2014", "2015", "2016", "2017"), skip_absent=TRUE) %>% 
  gather(Year, Total, 5:11) %>% 
  transform(Total= as.numeric(Total)) %>% transform(Year= as.numeric(Year))

cashless_by_year <- inclusion_df %>% filter(series_name =="Retail cashless transactions per 1,000 adults") %>% 
  filter(Year > 2011) %>% 
  filter(Year < 2016) %>% 
  ggplot(aes(x=Year, y=Total, color = country_code)) + 
  geom_line(show.legend = FALSE) +
          scale_y_log10() +
  theme_minimal() +
  geom_point(aes(group = seq_along(Year), show.legend = FALSE)) +
  transition_reveal(Year) + 
  labs(title = "Cashless Transactions by Year", y = "Cashless Transactions / 1000 adults")

cashless_by_year

inclusion_df %>% full_join(pop, by=c("country_name" = "country")) %>% 
  filter(series_name =="Retail cashless transactions per 1,000 adults" | series_name=="Deposit accounts per 1,000 adults") %>% 
  filter(Year > 2011) %>% 
  filter(Year < 2015) %>% 
  select(-series_code) %>% spread(series_name, Total) %>% 
  ggplot(aes(y=`Retail cashless transactions per 1,000 adults`, x=`Deposit accounts per 1,000 adults`, color = country_code, size=population)) + 
        scale_y_log10() +
        labs(title = "Impact of Deposit Accounts on Cashless Transactions", x= "Deposit Accounts / 1000 adults", y = "Cashless Transactions / 1000 adults") +
        theme_minimal() +
        geom_xspline() + geom_point()


