# ==========================================================
# LOCAL Daily Slate Builder (hoopR) -> GitHub Pages JSON
# - Runs on your PC (Task Scheduler)
# - Writes docs/slate_today.json inside your repo
# ==========================================================

library(hoopR)
library(dplyr)
library(jsonlite)
library(tibble)

# ---------- SETTINGS (EDIT THIS) ----------
REPO_DIR <- "C:/Users/nasso/OneDrive/Desktop/parlayplanner-data"  # <-- your local repo folder
OUT_FILE <- file.path(REPO_DIR, "docs", "slate_today.json")
# ----------------------------------------

# ---------- helpers ----------
write_json_out <- function(obj) {
  dir.create(file.path(REPO_DIR, "docs"), showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(obj, auto_unbox = TRUE, pretty = TRUE), OUT_FILE)
}

top5 <- function(df, col) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(playerName = character(), value = numeric()))
  }
  df %>%
    arrange(desc(.data[[col]])) %>%
    slice_head(n = 5) %>%
    transmute(playerName = as.character(PLAYER_NAME),
              value = as.numeric(.data[[col]]))
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

# ---------- 1) games from hoopR scoreboard ----------
sb_list <- tryCatch(
  nba_scoreboardv2(game_date = game_date_str, league_id = "00"),
  error = function(e) NULL
)

if (is.null(sb_list) || length(sb_list) == 0) {
  write_json_out(list(date = game_date_str, games = list(), note = "local: scoreboard call failed"))
  quit(save = "no", status = 0)
}

games_df <- sb_list[["GameHeader"]]
if (is.null(games_df) || nrow(games_df) == 0) {
  write_json_out(list(date = game_date_str, games = list(), note = "local: no games in GameHeader"))
  quit(save = "no", status = 0)
}

# ---------- 2) team mapping ----------
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

if (any(is.na(games_today$HOME_ABBR) | is.na(games_today$ROAD_ABBR))) {
  write_json_out(list(date = game_date_str, games = list(), note = "local: team mapping failed"))
  quit(save = "no", status = 0)
}

# ---------- 3) player splits (home/road) ----------
pull_splits <- function(location_value) {
  x <- tryCatch(
    nba_leaguedashplayerstats(
      season       = season,
      season_type  = "Regular Season",
      per_mode     = "PerGame",
      measure_type = "Base",
      location     = location_value
    ),
    error = function(e) NULL
  )
  
  if (is.null(x) || length(x) == 0) return(NULL)
  raw <- x[["LeagueDashPlayerStats"]]
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  
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

if (is.null(home_stats) || is.null(road_stats)) {
  write_json_out(list(date = game_date_str, games = list(), note = "local: could not load player splits"))
  quit(save = "no", status = 0)
}

# ---------- 4) build JSON schema your app expects ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  # Filter MIN >= 18 (your rule)
  home_tbl <- home_stats %>%
    filter(TEAM_ABBREVIATION == home_abbr) %>%
    filter(is.na(MIN) | MIN >= 18)
  
  road_tbl <- road_stats %>%
    filter(TEAM_ABBREVIATION == road_abbr) %>%
    filter(is.na(MIN) | MIN >= 18)
  
  game_df <- bind_rows(home_tbl, road_tbl)
  
  games_out[[length(games_out) + 1]] <- list(
    id = paste0(road_abbr, "@", home_abbr),
    title = paste0(road_abbr, " vs ", home_abbr),
    pointsTop5   = top5(game_df, "PTS"),
    reboundsTop5 = top5(game_df, "REB"),
    assistsTop5  = top5(game_df, "AST"),
    threesTop5   = top5(game_df, "FG3M")
  )
}

write_json_out(list(
  date = game_date_str,
  games = games_out,
  note = paste0("local: hoopR OK | season=", season, " | MIN>=18")
))

message("✅ Wrote: ", OUT_FILE)
