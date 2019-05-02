# clutch picks
# identifies high value picks
# must be alive/undetermined/not wight
# adds asterisks to Pick label
# "*" = 5|6 picks against, "**" = 7|8 picks against

markClutchPicks <- function(picks_chars, picks_chars_formatted) {
  # Mark picks whose fate is undecided
  indeterminant_chars <- picks_key %>%
    filter(Pick %in% c("Alive", "No", "")) %>%
    pull(Character)
  picks_chars <- picks_chars %>%
    mutate(indeterminant = Character %in% indeterminant_chars)
  
  # Add asterisks to clutch picks
  for (i in 1:nrow(picks_chars)) {
    # Wight not selected (NK dead)
    if (is.na(picks_chars$Wight[i]) || picks_chars$Wight[i] == 0) {
      # if indeterminant and pick isn't NA
      if (picks_chars$indeterminant[i] && !is.na(picks_chars$Pick)) {
        # Get total counts for that character
        counts <- picks_chars_formatted %>% filter(Character == picks_chars$Character[i])
        # Determine majority/minority
        matches_yes <- counts %>% filter(Pick == picks_chars$Pick[i]) %>% pull(n) %>% sum
        matches_no <- counts %>% filter(Pick != picks_chars$Pick[i]) %>% pull(n) %>% sum
        # If not unanimous
        if (matches_yes != 0) {
          # If 7 or 8 people voted differently, super clutch
          if (matches_no - matches_yes >= 5) {
            picks_chars$Pick[i] <- paste0(picks_chars$Pick[i], "**")
          }
          # Else if most voted differently, still clutch
          else if (matches_no >= matches_yes) {
            picks_chars$Pick[i] <- paste0(picks_chars$Pick[i], "*")
          }
        }
      }
    }
  }
  picks_chars <- picks_chars %>%
    dplyr::select(-indeterminant)
  
  return(picks_chars)
}