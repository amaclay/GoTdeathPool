
# Load resources
setwd("~/Documents/Entertainment/GoTdeathPool")
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

picks <- rbind(
  read.csv("data/GoTdeathPool_key.csv") %>% mutate(Person = "key"),
  read.csv("data/GoTdeathPool_al.csv") %>% mutate(Person = "al"),
  read.csv("data/GoTdeathPool_bw.csv") %>% mutate(Person = "bw"),
  read.csv("data/GoTdeathPool_df.csv") %>% mutate(Person = "df"),
  read.csv("data/GoTdeathPool_jk.csv") %>% mutate(Person = "jk"),
  read.csv("data/GoTdeathPool_ms.csv") %>% mutate(Person = "ms"),
  read.csv("data/GoTdeathPool_ng.csv") %>% mutate(Person = "ng"),
  read.csv("data/GoTdeathPool_rf.csv") %>% mutate(Person = "rf"),
  read.csv("data/GoTdeathPool_td.csv") %>% mutate(Person = "td"),
  read.csv("data/GoTdeathPool_am.csv") %>% mutate(Person = "am"))

# Tabular datatables of each Person's picks, colored by correctness (correct: green, wrong: red, NA: gray/clear)
picks_chars <- picks %>%
  filter(!is.na(Wight.White.Walker)) %>%
  mutate(Alive.Dead = case_when(Wight.White.Walker == "1" ~ "2", 
                                TRUE ~ Alive.Dead)) %>%
  select(-Wight.White.Walker) %>%
  mutate(Alive.Dead = factor(Alive.Dead, levels = 0:2, labels = c("Dead", "Alive", "White Walker"))) %>%
  rename(Status = "Alive.Dead")
picks_key <- picks_chars %>% filter(Person == "key") %>% select(-Person)
picks_chars <- picks_chars %>% 
  filter(Person != "key")

# Correctness
picks_chars$correct <- NA
for (char in picks_chars$Character) {
  picks_chars$correct[which(picks_chars$Character == char)] <- picks_chars$Status[which(picks_chars$Character == char)] == picks_key$Status[which(picks_key$Character == char)]
}

standings <- picks_chars %>%
  mutate(Score = case_when(correct & Status == "White Walker" ~ 2,
                           correct ~ 1,
                           TRUE ~ 0)) %>%
  group_by(Person) %>%
  summarise(Score = sum(Score)) %>%
  mutate(Person = c("Andy", "Maclay", "Bryan", "David", "Jasmine", "Mark", "Neale", "Ryan", "Tony")) %>%
  arrange(desc(Score))


char_death <- list()
picks_chars_formatted <- picks_chars %>% filter(Person != "key") %>% group_by(Character, Status) %>% count()
pal <- c("Alive" = "#1577b4", "Dead" = "#ff7f0e", "White Walker" = "#7f7f7f")
mapColors <- data.frame(Status = names(pal),
                        color = unname(pal),
                        stringsAsFactors = F)
# picks_chars_formatted <- picks_chars_formatted %>%
#   mutate(color = mapColors$Status)
for (char in unique(picks_chars$Character)) {
  formatted_subset <- picks_chars_formatted %>% filter(Character == char)
  mapColorsSub <- mapColors[match(formatted_subset$Status, mapColors$Status), 'color']
  char_death[[char]] <- plot_ly(formatted_subset,
                                labels = ~Status,
                                values = ~n,
                                type = "pie",
                                marker = list(color = ~Status,
                                              colors = mapColorsSub),
                                # color = ~Status,
                                # colors = mapColorsSub,
                                height = 200) %>%
    layout(title = char,
           xaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
           yaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
           margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0),
           showlegend = FALSE) %>%
    config(displayModeBar = F)
}

save(standings, picks_key, picks_chars, char_death, file = "data/data.RData")

# rmarkdown::render('GoTdeathPool.Rmd',output_file = "index.html")

