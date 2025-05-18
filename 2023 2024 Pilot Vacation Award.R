library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(glue)
library(ggtext)
library(DT)

### Imoort Vacation Spreadsheet ###

vacation_award_file <- read_excel("~/OneDrive - NJASAP/_Action NJASAP/2024 - 2025 Pilot Vacation Awards PQ Build.xlsx", 
                                                       sheet = "tVacaAward")
vacation_award <- vacation_award_file %>% 
  rename_with(~tolower(gsub(" ","_", .x))) %>% 
  rename(seniority = "sen#") %>% 
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date),
         fleet = ifelse(fleet == "CE-680", "CE-680x", fleet))

rm(vacation_award_file)

### Rank by Fleet Seat ###

rank_vacation_award <- vacation_award %>% 
  arrange(seniority) %>% 
  filter(week == "A") %>% 
  select(seniority, fleet, seat) %>% 
  group_by(fleet, seat) %>% 
  mutate(fleet_rank = rank(seniority),
         fleet_snrty = percent_rank(desc(seniority))
  )
  
vacation_award_join <- vacation_award %>% 
  left_join(rank_vacation_award, by = c("seniority", "fleet")) %>% 
  select(-matches(".*\\.y$")) %>% 
  arrange(seniority)

### Build Award Datatable ###

vacation_award_join %>% 
  select(-name, -end_date, -week_number) %>% 
  datatable(
    colnames = c("Senioirty", "Fleet", "Seat", "Week", "Start Date",
                 "No. Days", "Fleet Rank", "Flt. Pct. Rank"),
    rownames = F,
    filter = "top",
    options = list(
      paging = TRUE,
      pageLength = 25,
      autoWidth = TRUE,
      #    scrollY="100vh",
      scrollCollapse = FALSE
    ),
    caption = "2024-2025 NJASAP Vacation Award") %>% 
  formatPercentage(8, digits = 1)
