library(hoopR)
library(dplyr)
library(jsonlite)
library(tibble)
library(httr2)

# ---------- helpers ----------
write_json <- function(obj) {
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(obj, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")
}

top5 <- function(df, col) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(playerName = character(), value = numeric()))
  }
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

# ---------- ESPN schedule (reliable in cloud) ----------
get_games_from_espn <- function(date_str) {
  d <- gsub("-", "", date_str)  # ESPN wants YYYYMMDD
  url <- paste0(
    "https://site.web.api.espn.com/apis/v2/sports/basketball/nba/scoreboard?dates=",
    d
  )
  
  resp <- tryCatch(
    request(url) |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  
  txt <- tryCatch(resp_body_string(resp), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  
  js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(js) || is.null(js$events) || nrow(js$events) == 0) return(NULL)
  
  events <- js$events
  
  rows <- lapply(seq_len(nrow(events)), function(i) {
    # competitions is a list-column; take first competition
    comp <- events$competitions[[i]][[1]]
    comps <- comp$competitors
    
    home_row <- comps[comps$homeAway == "home", ]
    away_row <- comps[comps$homeAway == "away", ]
    
    if (nrow(home_row) == 0 || nrow(away_row) == 0) return(NULL)
    
    home_abbr <- home_row$team$abbreviation[[1]]
    away_abbr <- away_row$team$abbreviation[[1]]
    
    tibble(
      ROAD_ABBR = away_abbr,
      HOME_ABBR = home_abbr,
      id        = paste0(away_abbr, "@", home_abbr),
      title     = paste0(away_abbr, " vs ", home_abbr)
    )
  })
  
  out <- bind_rows(Filter(Negate(is.null), rows))
  if (nrow(out) == 0) return(NULL)
  
  out %>% distinct(ROAD_ABBR, HOME_ABBR, id, title)
}

games_today <- get_games_from_espn(game_date_str)
source_note <- "espn_scoreboard"

if (is.null(games_today) || nrow(games_today) == 0) {
  write_json(list(date = game_date_str, games = list(), note = "no games from ESPN endpoint"))
  quit(save = "no", status = 0)
}

# ---------- player splits (home/road) ----------
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
  write_json(list(date = game_date_str, games = list(), note = "could not load player splits"))
  quit(save = "no", status = 0)
}

# ---------- build games JSON (same schema) ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  home_tbl <- home_stats %>% filter(TEAM_ABBREVIATION == home_abbr)
  road_tbl <- road_stats %>% filter(TEAM_ABBREVIATION == road_abbr)
  
  game_df <- bind_rows(home_tbl, road_tbl) %>%
    # Keep your rule: MIN >= 18 (filter low-rotation noise)
    filter(is.na(MIN) | MIN >= 18)
  
  games_out[[length(games_out) + 1]] <- list(
    id = g$id,
    title = g$title,
    pointsTop5   = top5(game_df, "PTS"),
    reboundsTop5 = top5(game_df, "REB"),
    assistsTop5  = top5(game_df, "AST"),
    threesTop5   = top5(game_df, "FG3M")
  )
}

write_json(list(
  date  = game_date_str,
  games = games_out,
  note  = paste0("source=", source_note)
))

message("✅ Wrote docs/slate_today.json with ", length(games_out), " games (", source_note, ")")
