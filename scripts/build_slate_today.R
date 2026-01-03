library(hoopR)
library(dplyr)
library(jsonlite)
library(tibble)

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
    transmute(playerName = as.character(PLAYER_NAME),
              value      = as.numeric(.data[[col]]))
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

# ---------- schedule-based matchups (primary) ----------
# Use league dash TEAM stats with date_from/date_to and parse MATCHUP like "ATL @ TOR"
get_games_from_schedule <- function() {
  x <- tryCatch(
    nba_leaguedashteamstats(
      season       = season,
      season_type  = "Regular Season",
      per_mode     = "PerGame",
      measure_type = "Base",
      date_from    = game_date_str,
      date_to      = game_date_str
    ),
    error = function(e) NULL
  )
  if (is.null(x) || length(x) == 0) return(NULL)
  
  raw <- x[["LeagueDashTeamStats"]]
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  if (!("MATCHUP" %in% names(raw))) return(NULL)
  
  raw %>%
    distinct(MATCHUP) %>%
    mutate(MATCHUP = as.character(MATCHUP)) %>%
    # Handle both "ATL @ TOR" and "TOR vs. ATL"
    mutate(
      ROAD_ABBR = case_when(
        grepl("@", MATCHUP) ~ trimws(sub(" @.*$", "", MATCHUP)),
        grepl("vs", MATCHUP, ignore.case = TRUE) ~ trimws(sub(" vs.*$", "", MATCHUP)),
        TRUE ~ NA_character_
      ),
      HOME_ABBR = case_when(
        grepl("@", MATCHUP) ~ trimws(sub("^.*@ ", "", MATCHUP)),
        grepl("vs", MATCHUP, ignore.case = TRUE) ~ trimws(sub("^.*vs\\.\\s*", "", MATCHUP)),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(ROAD_ABBR), !is.na(HOME_ABBR)) %>%
    distinct(ROAD_ABBR, HOME_ABBR) %>%
    mutate(
      id    = paste0(ROAD_ABBR, "@", HOME_ABBR),
      title = paste0(ROAD_ABBR, " vs ", HOME_ABBR)
    )
}

games_today <- get_games_from_schedule()

if (is.null(games_today) || nrow(games_today) == 0) {
  write_json(list(date = game_date_str, games = list(), note = "no games from schedule endpoint"))
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

# ---------- build games json ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  home_tbl <- home_stats %>% filter(TEAM_ABBREVIATION == home_abbr)
  road_tbl <- road_stats %>% filter(TEAM_ABBREVIATION == road_abbr)
  
  game_df <- bind_rows(home_tbl, road_tbl) %>%
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
  date = game_date_str,
  games = games_out,
  note = "source=schedule_only"
))

message("✅ Wrote docs/slate_today.json with ", length(games_out), " games (schedule_only)")
