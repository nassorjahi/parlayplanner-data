library(hoopR)
library(dplyr)
library(jsonlite)

# ---------- helpers ----------
top5 <- function(df, col) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(playerName = character(), value = numeric()))
  }
  
  df %>%
    arrange(desc(.data[[col]])) %>%
    slice_head(n = 5) %>%
    transmute(
      playerName = as.character(PLAYER_NAME),
      value      = as.numeric(.data[[col]])
    )
}

write_empty_json <- function(date_str, msg = NULL) {
  out <- list(
    date = date_str,
    games = list()
  )
  if (!is.null(msg)) out$note <- msg
  
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")
  message("ℹ️ Wrote empty docs/slate_today.json :: ", msg %||% "no games")
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

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

# ---------- 1) todays games (SAFE) ----------
sb_list <- tryCatch(
  nba_scoreboardv2(game_date = game_date_str, league_id = "00"),
  error = function(e) {
    write_empty_json(game_date_str, paste0("scoreboard error: ", e$message))
    return(NULL)
  }
)

if (is.null(sb_list) || length(sb_list) == 0) {
  write_empty_json(game_date_str, "scoreboard returned NULL/empty")
  quit(save = "no", status = 0)
}

games_df <- sb_list[["GameHeader"]]

if (is.null(games_df) || nrow(games_df) == 0) {
  write_empty_json(game_date_str, "no games in GameHeader")
  quit(save = "no", status = 0)
}

# ---------- team map ----------
teams <- tryCatch(
  nba_teams(),
  error = function(e) {
    write_empty_json(game_date_str, paste0("nba_teams error: ", e$message))
    return(NULL)
  }
)

if (is.null(teams) || nrow(teams) == 0) {
  write_empty_json(game_date_str, "nba_teams returned empty")
  quit(save = "no", status = 0)
}

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
  rename(ROAD_TEAM = TEAM_NAME, ROAD_ABBR = TEAM_ABBREVIATION) %>%
  filter(!is.na(HOME_ABBR), !is.na(ROAD_ABBR))

if (nrow(games_today) == 0) {
  write_empty_json(game_date_str, "games exist but team mapping failed")
  quit(save = "no", status = 0)
}

# ---------- 2) splits (SAFE) ----------
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
  write_empty_json(game_date_str, "could not load player splits (home/road)")
  quit(save = "no", status = 0)
}

# ---------- 3) build games json ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  home_tbl <- home_stats %>% filter(TEAM_ABBREVIATION == home_abbr)
  road_tbl <- road_stats %>% filter(TEAM_ABBREVIATION == road_abbr)
  
  game_df <- bind_rows(home_tbl, road_tbl) %>%
    # keep your MIN filter (18+)
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

out <- list(
  date = game_date_str,
  games = games_out
)

dir.create("docs", showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")
message("✅ Wrote docs/slate_today.json with ", length(games_out), " games")
