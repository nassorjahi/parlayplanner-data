# ==========================================================
# LOCAL Daily Slate Builder (hoopR)
# - Writes docs/slate_today.json inside your local repo
# ==========================================================

library(hoopR)
library(dplyr)
library(jsonlite)
library(tibble)

# ---------- SETTINGS ----------
# This path matches your GitHub desktop folder location
REPO_DIR <- "C:/Users/nasso/OneDrive/Desktop/parlayplanner-data"
OUT_FILE <- file.path(REPO_DIR, "docs", "slate_today.json")

# ---------- Helpers ----------
write_json_out <- function(obj) {
  dir.create(file.path(REPO_DIR, "docs"), showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(obj, auto_unbox = TRUE, pretty = TRUE), OUT_FILE)
}

top5 <- function(df, col) {
  if (is.null(df) || nrow(df) == 0) return(tibble(playerName = character(), value = numeric()))
  df %>%
    arrange(desc(.data[[col]])) %>%
    slice_head(n = 5) %>%
    transmute(playerName = as.character(PLAYER_NAME),
              value = as.numeric(.data[[col]]))
}

# ---------- Logic ----------
today <- Sys.Date()
season <- "2023-24" # Current NBA season format
game_date_str <- format(today, "%Y-%m-%d")

sb_list <- tryCatch(nba_scoreboardv2(game_date = game_date_str), error = function(e) NULL)

if (is.null(sb_list)) {
  write_json_out(list(date = game_date_str, games = list(), note = "Local failed: No scoreboard"))
  quit(save = "no", status = 0)
}

games_df <- sb_list[["GameHeader"]]
teams <- nba_teams()
team_map <- teams %>% transmute(TEAM_ID = team_id, TEAM_ABBR = team_abbreviation)

games_today <- games_df %>%
  left_join(team_map, by = c("HOME_TEAM_ID" = "TEAM_ID")) %>% rename(HOME_ABBR = TEAM_ABBR) %>%
  left_join(team_map, by = c("VISITOR_TEAM_ID" = "TEAM_ID")) %>% rename(ROAD_ABBR = TEAM_ABBR)

# Fetching player stats (Simplified for reliability)
stats <- nba_leaguedashplayerstats(season = season, per_mode = "PerGame")[["LeagueDashPlayerStats"]]

games_out <- list()
for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  game_stats <- stats %>% filter(TEAM_ABBREVIATION %in% c(g$HOME_ABBR, g$ROAD_ABBR))
  
  games_out[[length(games_out) + 1]] <- list(
    id = paste0(g$ROAD_ABBR, "@", g$HOME_ABBR),
    title = paste0(g$ROAD_ABBR, " vs ", g$HOME_ABBR),
    pointsTop5   = top5(game_stats, "PTS"),
    reboundsTop5 = top5(game_stats, "REB"),
    assistsTop5  = top5(game_stats, "AST"),
    threesTop5   = top5(game_stats, "FG3M")
  )
}

write_json_out(list(date = game_date_str, games = games_out, note = "Updated locally via hoopR"))
message("✅ Wrote: ", OUT_FILE)