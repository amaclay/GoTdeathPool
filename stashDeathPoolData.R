# Potential points left
# place to all_standings

# Load resources
setwd("~/Documents/Entertainment/GoTdeathPool")
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

options(stringsAsFactors = F)

WEEK <- 4
ambiguousChars <- c("Hot Pie", "Meera Reed", "Robert Arryn", "Edmure Tully", "Daario Naharis", "Elia Sand")

fileMap <- c(key_week0 = "key0",
             key_week1 = "key1",
             key_week2 = "key2",
             key_week3 = "key3",
             key_week4 = "key4",
             al = "Andy",
             bw = "Bryan",
             df = "David",
             jk = "Jasmine",
             ms = "Mark",
             ng = "Neale",
             rf = "Ryan",
             td = "Tony",
             am = "Maclay")

picks <- data.frame()
for (fileID in names(fileMap)) {
  picks <- rbind(picks,
                 read.csv(paste0("data/GoTdeathPool_", fileID, ".csv")) %>% mutate(Person = fileMap[[fileID]]))
}

# Tabular datatables of each Person's picks, colored by correctness (correct: green, wrong: red, NA: gray/clear)
picks_formatted <- picks %>%
  # filter(Person == "df" | Person == "key") %>%
  # Combine Alive.Dead and Wight.White.Walker
  mutate(Alive.Dead = case_when(!is.na(Wight.White.Walker) & Alive.Dead == "0" ~ "Dead",
                                !is.na(Wight.White.Walker) & Alive.Dead == "1" ~ "Alive",
                                any(Character %in% c("danny_preg", "dead_night_king", "babby_survive")) & Alive.Dead == "0" ~ "No",
                                any(Character %in% c("danny_preg", "dead_night_king", "babby_survive")) & Alive.Dead == "1" ~ "Yes",
                                TRUE ~ as.character(Alive.Dead)),
         Character = case_when(Character == "danny_preg" ~ "Danny Pregnant",
                               Character == "babby_survive" ~ "Baby Survives",
                               Character == "dead_night_king" ~ "NK Dies",
                               Character == "night_king_killer" ~ "NK Killer",
                               Character == "iron_throne" ~ "7 Kingdoms Champ",
                               TRUE ~ Character)) %>%
  rename(Pick = "Alive.Dead",
         Wight = "Wight.White.Walker")

picks_chars <- picks_formatted %>%
  filter(!grepl("key", Person))

###############
## Standings ##
###############

all_standings <- data.frame(Person = unique(picks_chars$Person))
for (week in 0:WEEK) {
  # Split key from participants
  picks_key <- picks_formatted %>% 
    filter(Person == paste0("key", week),
           !Character %in% ambiguousChars) %>%
    select(-Person)
  
  # Correctness
  picks_chars$correct <- NA
  for (char in picks_key$Character) {
    picks_chars$correct[which(picks_chars$Character == char)] <- picks_chars$Pick[which(picks_chars$Character == char)] == picks_key$Pick[which(picks_key$Character == char)]
    # set correct to FALSE if wight selected incorrectly
    wight_key <- picks_key$Wight[which(picks_key$Character == char)]
    picks_chars <- picks_chars %>%
      mutate(correct = case_when(correct &
                                   Character == char &
                                   Wight == "1" &
                                   wight_key == "0" ~ FALSE,
                                 TRUE ~ correct))
  }
  
  standings <- picks_chars %>%
    mutate(Score = case_when(correct & Wight == "1" ~ 2,
                             correct ~ 1,
                             TRUE ~ 0)) %>%
    filter(!is.na(correct)) %>%
    group_by(Person) %>%
    summarise(!!paste0("week", week) := sum(Score))
  
  all_standings <- all_standings %>%
    left_join(standings, by = "Person")
}

all_standings <- all_standings %>%
  arrange(desc(!!rlang::sym(paste0("week", WEEK))), desc(!!rlang::sym(paste0("week", WEEK-1))))

standings <- all_standings[c(1, ncol(all_standings))] %>%
  rename(Score = paste0("week", WEEK))

all_standings <- all_standings %>%
  gather("week", "score", -Person)

#############
## Display ##
#############

# Overwrite Pick label if wight
picks_chars <- picks_chars %>%
  mutate(Pick = case_when(Wight == "1" ~ "Wight",
                          TRUE ~ Pick))

char_death <- list()
picks_chars_formatted <- picks_chars %>%
  mutate(Pick = case_when(is.na(Pick) ~ "NA",
                          TRUE ~ Pick)) %>%
  group_by(Character, Pick) %>% 
  count()

source("clutch_picks.R")
picks_chars <- markClutchPicks(picks_chars, picks_chars_formatted)

pal <- c("Alive" = "#1577b4", "Dead" = "#ff7f0e", "Wight" = "#7f7f7f")
mapColors <- data.frame(Pick = names(pal),
                        color = unname(pal),
                        stringsAsFactors = F)
i <- 0
for (char in unique(picks_chars$Character)) {
  formatted_subset <- picks_chars_formatted %>% filter(Character == char)
  if (i < 39) {
    # Custom color mapping for character alive/dead
    mapColorsSub <- mapColors[match(formatted_subset$Pick, mapColors$Pick), 'color']
    char_death[[char]] <- plot_ly(formatted_subset,
                                  labels = ~Pick,
                                  values = ~n,
                                  text = ~paste0(Pick, "\n", round((n / sum(n))*100, 0), "%"),
                                  textinfo='text',
                                  type = "pie",
                                  hoverinfo="none",
                                  textposition="inside",
                                  marker = list(color = ~Pick,
                                                colors = mapColorsSub),
                                  # color = ~Pick,
                                  # colors = mapColorsSub,
                                  height = 200) %>%
      layout(title = char,
             xaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
             yaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
             margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0),
             showlegend = FALSE) %>%
      config(displayModeBar = F)
  } else { 
    # No custom color mapping
    char_death[[char]] <- plot_ly(formatted_subset,
                                  labels = ~Pick,
                                  values = ~n,
                                  text = ~paste0(round((n / sum(n))*100, 0), "%"),
                                  textinfo = 'text',
                                  textposition="inside",
                                  hoverinfo="text",
                                  hovertext = formatted_subset$Pick,
                                  type = "pie",
                                  height = 200) %>%
      layout(title = char,
             xaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
             yaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabes = FALSE),
             margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0),
             showlegend = FALSE) %>%
      config(displayModeBar = F)
  }
  i <- i + 1
}

trace_al <- all_standings %>% filter(Person == "Andy")
trace_bw <- all_standings %>% filter(Person == "Bryan") %>% pull(score)
trace_df <- all_standings %>% filter(Person == "David") %>% pull(score)
trace_jk <- all_standings %>% filter(Person == "Jasmine") %>% pull(score)
trace_ms <- all_standings %>% filter(Person == "Mark") %>% pull(score)
trace_ng <- all_standings %>% filter(Person == "Neale") %>% pull(score)
trace_rf <- all_standings %>% filter(Person == "Ryan") %>% pull(score)
trace_td <- all_standings %>% filter(Person == "Tony") %>% pull(score)
trace_am <- all_standings %>% filter(Person == "Maclay") %>% pull(score)

standingsProgression <- plot_ly(trace_al, name = "Andy", mode = 'lines+markers',
                                x = ~week, y = ~score, type = 'scatter', height = '300', width = '500') %>%
  add_trace(y = ~trace_bw, name = 'Bryan', mode = 'lines+markers') %>%
  add_trace(y = ~trace_df, name = 'Tony', mode = 'lines+markers') %>%
  add_trace(y = ~trace_jk, name = 'Jasmine', mode = 'lines+markers') %>%
  add_trace(y = ~trace_ms, name = 'Mark', mode = 'lines+markers') %>%
  add_trace(y = ~trace_ng, name = 'Neale', mode = 'lines+markers') %>%
  add_trace(y = ~trace_rf, name = 'Ryan', mode = 'lines+markers') %>%
  add_trace(y = ~trace_td, name = 'Tony', mode = 'lines+markers') %>%
  add_trace(y = ~trace_am, name = 'Maclay', mode = 'lines+markers') %>%
  config(displayModeBar = FALSE) %>%
  layout(xaxis = list(title = "", tickangle = -45), yaxis = list(title = ""))

save(standings, picks_key, picks_chars, char_death, file = "data/data.RData")

rmarkdown::render('GoTdeathPool.Rmd',output_file = "index.html")
rmarkdown::render('picksDistribution.Rmd',output_file = "picks.html")
