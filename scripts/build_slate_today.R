library(hoopR)
library(dplyr)
library(jsonlite)

# ---------- helpers ----------
top5 <- function(df, col) {
  df %>%
    arrange(desc(.data[[col]])) %>%
    slice_head(n = 5) %>%
    transmute(
      playerName = as.character(PLAYER_NAME),
      value      = as.numeric(.data[[col]])
    )
}

# ---------- date/season ----------
today <- Sys.Date()
today_year  <- as.integer(format(today, "%Y"))
today_month <- as.integer(format(today, "%m"))

start_year <- if (today_month >= 10) today_year else today_year - 1
end_year   <- (start_year + 1) %% 100
season     <- sprintf("%d-%02d", start_year, end_year)

game_date_str <- format(today, "%Y-%m-%d")

message("Date: ", game_date_str)
message("Season: ", season)

# ---------- 1) todays games ----------
sb_list <- nba_scoreboardv2(game_date = game_date_str, league_id = "00")
games_df <- sb_list[["GameHeader"]]

# If no games, still output valid JSON
if (is.null(games_df) || nrow(games_df) == 0) {
  out <- list(date = game_date_str, games = list())
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")
  quit(save = "no", status = 0)
}

teams <- nba_teams()
team_map <- teams %>%
  transmute(
    TEAM_ID           = team_id,
    TEAM_NAME         = team_name,
    TEAM_ABBREVIATION = team_abbreviation
  )

games_today <- games_df %>%
  left_join(team_map, by = c("HOME_TEAM_ID" = "TEAM_ID")) %>%
  rename(HOME_TEAM = TEAM_NAME, HOME_ABBR = TEAM_ABBREVIATION) %>%
  left_join(team_map, by = c("VISITOR_TEAM_ID" = "TEAM_ID")) %>%
  rename(ROAD_TEAM = TEAM_NAME, ROAD_ABBR = TEAM_ABBREVIATION)

# ---------- 2) splits ----------
pull_splits <- function(location_value) {
  x <- nba_leaguedashplayerstats(
    season       = season,
    season_type  = "Regular Season",
    per_mode     = "PerGame",
    measure_type = "Base",
    location     = location_value
  )
  raw <- x[["LeagueDashPlayerStats"]]
  
  raw %>%
    mutate(
      GP   = as.numeric(GP),
      MIN  = as.numeric(MIN),
      PTS  = as.numeric(PTS),
      FG3M = as.numeric(FG3M),
      REB  = as.numeric(REB),
      AST  = as.numeric(AST)
    )
}

home_stats <- pull_splits("Home")
road_stats <- pull_splits("Road")

# ---------- 3) build games json ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  home_tbl <- home_stats %>% filter(TEAM_ABBREVIATION == home_abbr)
  road_tbl <- road_stats %>% filter(TEAM_ABBREVIATION == road_abbr)
  
  game_df <- bind_rows(home_tbl, road_tbl) %>%
    filter(is.na(MIN) | MIN >= 18)
  
  game_id <- paste0(road_abbr, "@", home_abbr)
  game_title <- paste0(g$ROAD_TEAM, " vs ", g$HOME_TEAM)
  
  games_out[[length(games_out) + 1]] <- list(
    id = game_id,
    title = game_title,
    pointsTop5   = top5(game_df, "PTS"),
    reboundsTop5 = top5(game_df, "REB"),
    assistsTop5  = top5(game_df, "AST"),
    threesTop5   = top5(game_df, "FG3M")
  )
}

out <- list(date = game_date_str, games = games_out)

dir.create("docs", showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")

message("✅ Wrote docs/slate_today.json")
