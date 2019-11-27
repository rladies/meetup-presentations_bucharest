## Script prezentat de Andra Garoi si Maria Romanescu ( studente Master ASE - Statistica Aplicata si Data Science)
## Explorarea datelor folosind dplyr
## R-Ladies Bucharest Community Meetups - 26.11.2019


#instalare dplyr 
install.packages(dplyr)
library(dplyr)

#am incarcat de pe github datele deja curatate
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 


#glimpse
glimpse(jobs_gender)

#select
select(jobs_gender, year, occupation, total_earnings_female)

#filter
filter(jobs_gender,occupation=="Cost estimators")

#arrange
arrange(jobs_gender, desc(total_earnings_female))

#summarise
summarise(jobs_gender, 
          medie_femei_angajate=mean(workers_female), 
          medie_barbati_angajati=mean(workers_male))

#mutate
mutate(jobs_gender, diferenta = total_earnings_male - total_earnings_female)           


#gorup_by
group_by(jobs_gender, year)

#introducem operatorul pipe
select(jobs_gender, total_earnings_female) 

jobs_gender %>% # din tabelul jobs_gender 
  select(total_earnings_female) # selectam coloana total_earnings_female

jobs_gender %>% 
  group_by(major_category)%>% 
  filter(!is.na(total_earnings_female)) %>% 
  summarise(media_venit_femei = mean(total_earnings_female)) %>% 
  arrange(desc(media_venit_femei))

#inner_join
#tabel 1
jobs_gender %>%
  group_by(year) %>%
  summarise(medie_angajati_total=mean(total_workers),
            femei=  mean(workers_female), barbati=mean(workers_male))

#tabel 2
employed_gender %>%
  select(year, full_time_female, part_time_female) %>%
  arrange(desc(year))

#Unire cele doua tabele:
jobs_gender %>% group_by(year) %>%
  summarise(medie_angajati_total=mean(total_workers),
            femei=mean(workers_female), barbati=mean(workers_male)) %>%
  inner_join(employed_gender, jobs_gender, by="year") %>%
  select(year, medie_angajati_total, femei, full_time_female, part_time_female)

