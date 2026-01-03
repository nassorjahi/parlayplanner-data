library(hoopR)
library(dplyr)
library(jsonlite)
library(tibble)

# ---------- helpers ----------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

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

# ---------- team map ----------
teams <- tryCatch(nba_teams(), error = function(e) NULL)

if (is.null(teams) || nrow(teams) == 0) {
  write_json(list(date = game_date_str, games = list(), note = "nba_teams returned empty"))
  quit(save = "no", status = 0)
}

team_map <- teams %>%
  transmute(
    TEAM_ID           = team_id,
    TEAM_NAME         = team_name,
    TEAM_ABBREVIATION = team_abbreviation
  )

abbr_to_name <- function(abbr) {
  nm <- team_map$TEAM_NAME[match(abbr, team_map$TEAM_ABBREVIATION)]
  ifelse(is.na(nm), abbr, nm)
}

# ---------- A) try scoreboard ----------
get_games_from_scoreboard <- function() {
  sb <- tryCatch(
    nba_scoreboardv2(game_date = game_date_str, league_id = "00"),
    error = function(e) NULL
  )
  if (is.null(sb) || length(sb) == 0) return(NULL)
  
  gh <- sb[["GameHeader"]]
  if (is.null(gh) || nrow(gh) == 0) return(NULL)
  
  gh %>%
    left_join(team_map, by = c("HOME_TEAM_ID" = "TEAM_ID")) %>%
    rename(HOME_TEAM = TEAM_NAME, HOME_ABBR = TEAM_ABBREVIATION) %>%
    left_join(team_map, by = c("VISITOR_TEAM_ID" = "TEAM_ID")) %>%
    rename(ROAD_TEAM = TEAM_NAME, ROAD_ABBR = TEAM_ABBREVIATION) %>%
    filter(!is.na(HOME_ABBR), !is.na(ROAD_ABBR)) %>%
    transmute(
      HOME_ABBR, ROAD_ABBR,
      HOME_TEAM, ROAD_TEAM
    )
}

# ---------- B) fallback schedule table ----------
# Uses league dash TEAM stats with "PerGame" + today's date window to pull the games list.
# This endpoint is typically more reliable from cloud runners than scoreboard.
get_games_from_schedule_fallback <- function() {
  # Wide window to avoid timezone edge issues
  start_date <- game_date_str
  end_date   <- game_date_str
  
  sched <- tryCatch(
    nba_leaguedashteamstats(
      season       = season,
      season_type  = "Regular Season",
      per_mode     = "PerGame",
      measure_type = "Base",
      date_from    = start_date,
      date_to      = end_date
    ),
    error = function(e) NULL
  )
  
  if (is.null(sched) || length(sched) == 0) return(NULL)
  
  raw <- sched[["LeagueDashTeamStats"]]
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  
  # This table contains GAME_ID and MATCHUP strings like "BOS @ NYK" or "NYK vs BOS"
  if (!("MATCHUP" %in% names(raw))) return(NULL)
  
  raw %>%
    distinct(MATCHUP) %>%
    mutate(MATCHUP = as.character(MATCHUP)) %>%
    # Prefer "@", but handle "vs."
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
    mutate(
      ROAD_TEAM = abbr_to_name(ROAD_ABBR),
      HOME_TEAM = abbr_to_name(HOME_ABBR)
    ) %>%
    transmute(HOME_ABBR, ROAD_ABBR, HOME_TEAM, ROAD_TEAM)
}

games_today <- get_games_from_scoreboard()

source_note <- "scoreboard"
if (is.null(games_today) || nrow(games_today) == 0) {
  games_today <- get_games_from_schedule_fallback()
  source_note <- "schedule_fallback"
}

if (is.null(games_today) || nrow(games_today) == 0) {
  write_json(list(date = game_date_str, games = list(), note = paste0("no games (", source_note, ")")))
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
  
  # keep your MIN filter (18+)
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

out <- list(
  date  = game_date_str,
  games = games_out,
  note  = paste0("source=", source_note)
)

write_json(out)
message("✅ Wrote docs/slate_today.json with ", length(games_out), " games (", source_note, ")")
