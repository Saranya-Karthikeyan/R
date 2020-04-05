library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
C19 = read.csv("C:/Users/saran/Desktop/Final_Project/Covid19/us-counties.csv")

C19$date = as.Date(C19$date)
C19 = data[order(C19$date), ]
C19

# todos menos NY e NJ - casos
dataCASES <- data  %>% group_by(state,date) %>% summarise(nCasos = sum(cases))
dataCASESsny <- dataCASES  %>% filter(state != "New York" & state != "New Jersey")

C19casesny <- C19cases  %>% filter(state == "New York" | state == "New Jersey")

ggplot(data = C19casesny,
       mapping = aes(x = date, y = nCasos)) +
  geom_line() +
  labs(x = "Data", y = "Number of cases",title = 'Number of cases COVID-19') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=50)) +
  theme(axis.text.x=element_text(angle=10)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~ C19casesny$state, ncol=10)

###DEATH RATE####

dataDEATHS <- C19  %>% group_by(state,date) %>% summarise(nCasos = sum(deaths))
dataDEATHSsny <- dataDEATHS  %>% filter(state != "New York" & state != "New Jersey")

ggplot(data = dataDEATHSsny,
       mapping = aes(x = date, y = nCasos)) +
  geom_line() +
  labs(x = "Data", y = "Number of deaths",title = 'Number of deaths COVID-19') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=50)) +
  theme(axis.text.x=element_text(angle=10)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~ dataDEATHSsny$state, ncol=10)

