library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(glue)
library(ggtext)

### Ops Schedule Import & Clean ###
ops_sched_file <- dir(path = "~/OneDrive - NJASAP/Documents/Operational Schedules/Ops Schedules Merge PowerQuery/2021",
                       full.names = T,
                       pattern = "2025-06 All Fleets.*\\.xlsx$")

tjune_ops_sched <- read_excel(ops_sched_file,
                          sheet = "Sheet1",
                          range = cell_cols(1:6)
                          )

tjune_ops_sched <- tjune_ops_sched %>% 
  rename_with(~tolower(gsub(" ","_", .x)))

tjune_ops_sched %>% 
  select(position) %>% 
  unique()
              

tjune_bid_clean <- tjune_ops_sched %>% 
  rename_with(~tolower(gsub(" ","_", .x))) %>% 
  filter(str_detect(schedule_type, "^7|^8"),
         ! position %in% c("FA", "FACA", "FIOE", "NQPS", "NQC")) %>% 
  mutate(across(schedule_type, ~gsub("7 & 7 - ", "7&7_", .x)),
         across(schedule_type, ~gsub("8&6 - ", "8&6_", .x)),
         across(schedule_type, ~gsub(" W/Travel - ", "_", .x)),
         across(schedule_type, ~gsub(" TSP - ", "_", .x)),
         line_num = as.double(str_extract(schedule_type, "\\d{1,2}$")),
         weekend = case_when(line_num == 2 ~ "Weekend",
                             line_num == 3 ~ "Weekend",
                             line_num == 9 ~ "Weekend",
                             line_num == 10 ~ "Weekend",
                             TRUE ~ "Weekday"
                              ),
         seat = case_when(position == "CA" ~ "PIC",
                          position == "TR" ~ "PIC",
                          position == "SIOE" ~ "SIC",
                          TRUE ~ position)
         )


View(tjune_bid_clean)

### Seniority List Import and Clean ###

seniority_file <- dir(path = "~/OneDrive - NJASAP/Documents/Seniority Related/Seniority List - Union/2025",
                      full.names = T,
                      pattern = "2025-04.*\\.xlsx$")

tseniority <- read_excel(seniority_file,
                         sheet = "UNION_EXCEL_FILE",
                         range = cell_cols(1:16)
)

tseniority <- tseniority %>% 
  rename_with(~tolower(gsub(" ","_",.x))) %>% 
  mutate(cmi = as.numeric(cmi))


### Join to Add Seniority ###

tjune_bid_clean <- tjune_bid_clean %>% 
  left_join(tseniority, by = c("cmi" = "cmi")) %>% 
  select(1:9, union_seniority.x) %>% 
  rename(name = name.x, seat = seat.x, union_seniority = union_seniority.x)
  

### Plot Construction ###
## 7&7 ##

#t7n7_all <- 
tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  count(line_num, schedule_type, weekend, name = "count") %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num),
         percent = label_percent(accuracy = 0.1)(count / sum(count))) %>%
  ggplot(aes(schedule_type, count))+
  geom_col(aes(fill = weekend))+
  geom_text(aes(label = glue("{count} ({percent})")), vjust = -0.5)+
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "7&7 Line Distribution",
       subtitle = "*All Fleets and Seats*",
       fill = ""
       )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown()
        )

### Add Average Seniority ###

tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  group_by(line_num, schedule_type, weekend) %>% 
  summarise(count = n(),
            avg_snrty = mean(union_seniority, na.rm = T),
            .groups = "drop"
            ) %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num),
         percent = label_percent(accuracy = 0.1)(count / sum(count)),
         avg_snrty = format(round(avg_snrty, 0), big.mark = ",")) %>%
  ggplot(aes(schedule_type, count))+
  geom_col(aes(fill = weekend))+
  geom_text(aes(label = glue("{count} ({percent})")), vjust = -0.5)+
  # geom_text(aes(label = glue("Avg. Snrty\n{avg_snrty}")),
  #           vjust = 1.3,
  #           color = "white")+
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "7&7 Line Distribution",
       subtitle = "*All Fleets and Seats*",
       fill = ""
  )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "top",
        #legend.direction =  "horizontal"
  )

ggsave("7_7_Global_Line_Dist_Snrty.png", path = "images",
       width = 1888,
       height = 900,
       units =c("px"),
       device = NULL,
       dpi = 173)

 ## 8&6 ##

tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^8")) %>% 
  group_by(line_num, schedule_type, weekend) %>% 
  summarise(count = n(),
            avg_snrty = mean(union_seniority, na.rm = T),
            .groups = "drop"
  ) %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num),
         percent = label_percent(accuracy = 0.1)(count / sum(count)),
         avg_snrty = format(round(avg_snrty, 0), big.mark = ",")) %>%
  ggplot(aes(schedule_type, count))+
  geom_col(aes(fill = weekend))+
  geom_text(aes(label = glue("{count} ({percent})")), vjust = -0.5)+
  # geom_text(aes(label = glue("Avg. Snrty\n{avg_snrty}")),
  #           vjust = 1.3,
  #           color = "white")+
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "8&6 Line Distribution",
       subtitle = "*All Fleets and Seats*",
       fill = ""
  )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "top",
        #legend.direction =  "horizontal"
  )

ggsave("8_6_Global_Line_Dist_Snrty.png", path = "images",
       width = 1888,
       height = 900,
       units =c("px"),
       device = NULL,
       dpi = 173)

### Weekend v Weekday Summary ###

## 7&7 ##

tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  count(weekend, name = "count") %>% 
  mutate(percent = label_percent(accuracy = 0.1)(count / sum(count))) %>% 
  ggplot(aes(weekend, count))+
  geom_col(aes(fill = weekend), legend = F)+
  geom_text(aes(label = glue("{count} ({percent})")), hjust = 1.1, color = "white" )+ #fill = "steelblue", color = "#2C5171"
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "7&7 Weekend to Weekday Ratio",
       subtitle = "*All Fleets and Seats*",
       fill = ""
  )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        legend.position = "none"
  )+
  coord_flip()

## 8&6 ##
  
tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^8")) %>% 
  count(weekend, name = "count") %>% 
  mutate(percent = label_percent(accuracy = 0.1)(count / sum(count))) %>% 
  ggplot(aes(weekend, count))+
  geom_col(aes(fill = weekend), show.legend = F)+
  geom_text(aes(label = glue("{count} ({percent})")), hjust = 1.1, color = "white" )+ #fill = "steelblue", color = "#2C5171"
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "8&6 Weekend to Weekday Ratio",
       subtitle = "*All Fleets and Seats*",
       fill = ""
  )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        legend.position = "none"
  )+
  coord_flip()

### Fleet and Seat ###

## Table to Pass to Function ##

tfunction_variables <- tjune_bid_clean %>% 
  select(schedule_type, seat, fleet) %>% 
  mutate(schedx = ifelse(str_detect(schedule_type, "^7"), "^7", "^8"),
         seatx = seat,
         fleetx = fleet) %>% 
  group_by(schedx, seatx, fleetx) %>%
  count() %>% 
  select(schedx, seatx, fleetx)


## Function ##

fline_distribution <- function(schedx, seatx, fleetx){
  
line_type <- ifelse(schedx == "^7", "7&7", "8&6")

tjune_bid_clean %>% 
  filter(str_detect(schedule_type, schedx),
         fleet == fleetx,
         seat == seatx) %>% 
  group_by(line_num, schedule_type, weekend) %>% 
  summarise(count = n(),
            avg_snrty = mean(union_seniority, na.rm = T),
            .groups = "drop") %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num),
         percent = label_percent(accuracy = 0.1)(count / sum(count)),
         avg_snrty = format(round(avg_snrty, 0), big.mark = ",")) %>%
  ggplot(aes(schedule_type, count))+
  geom_col(aes(fill = weekend))+
  geom_text(aes(label = glue("{count} ({percent})")), vjust = -0.5)+
  # geom_text(aes(label = glue("Avg. Snrty\n{avg_snrty}")),
  #           vjust = 1.3,
  #           color = "white")+
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = glue("{line_type} Line Distribution"),
       subtitle = glue("*{fleetx} {seatx}*"),
       fill = ""
  )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "top",
        #legend.direction =  "horizontal"
  )
  
  ggsave(glue("{line_type}_{fleetx}_{seatx}.png"), path = "images/Line_Dists",
         width = 1888,
         height = 900,
         units =c("px"),
         device = NULL,
         dpi = 173)
 
}

fline_distribution("^7", "PIC", "CE-680")

tfunction_variables %>% 
  pmap_chr(fline_distribution)
