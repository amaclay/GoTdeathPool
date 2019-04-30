# Who should you root for?
# Pick dead and majority alives

# Load resources
setwd("~/Documents/Entertainment/GoTdeathPool")
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

options(stringsAsFactors = F)

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
# Split key from participants
picks_key <- picks_chars %>% filter(Person == "key") %>% select(-Person)
picks_chars <- picks_chars %>% 
  filter(Person != "key")

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
  group_by(Person) %>%
  summarise(Score = sum(Score)) %>%
  mutate(Person = c("Andy", "Maclay", "Bryan", "David", "Jasmine", "Mark", "Neale", "Ryan", "Tony")) %>%
  arrange(desc(Score))

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

save(standings, picks_key, picks_chars, char_death, file = "data/data.RData")

# rmarkdown::render('GoTdeathPool.Rmd',output_file = "index.html")

