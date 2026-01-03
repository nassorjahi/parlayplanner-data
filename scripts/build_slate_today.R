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

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_str <- function(x, maxn = 200) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) return("")
  substr(x, 1, maxn)
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

# ---------- ESPN games (robust) ----------
get_games_from_espn <- function(date_str) {
  d <- gsub("-", "", date_str)
  
  urls <- c(
    paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?dates=", d),
    paste0("https://site.web.api.espn.com/apis/v2/sports/basketball/nba/scoreboard?dates=", d)
  )
  
  last_note <- NULL
  
  for (url in urls) {
    resp <- tryCatch(
      request(url) |> req_timeout(30) |> req_perform(),
      error = function(e) {
        last_note <<- paste0("ESPN request error: ", e$message)
        NULL
      }
    )
    if (is.null(resp)) next
    
    status <- resp_status(resp)
    body_txt <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (status < 200 || status >= 300) {
      last_note <- paste0("ESPN HTTP ", status, " body=", safe_str(body_txt, 120))
      next
    }
    
    js <- tryCatch(fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(js)) {
      last_note <- paste0("ESPN JSON parse error body=", safe_str(body_txt, 120))
      next
    }
    
    events <- js$events
    if (is.null(events) || length(events) == 0) {
      last_note <- paste0("ESPN no events :: ", url)
      next
    }
    
    rows <- list()
    for (ev in events) {
      comp <- NULL
      if (!is.null(ev$competitions) && length(ev$competitions) >= 1) comp <- ev$competitions[[1]]
      if (is.null(comp)) next
      
      competitors <- comp$competitors
      if (is.null(competitors) || length(competitors) < 2) next
      
      home_abbr <- NULL
      away_abbr <- NULL
      
      for (cpt in competitors) {
        ha <- cpt$homeAway
        ab <- NULL
        if (!is.null(cpt$team) && !is.null(cpt$team$abbreviation)) ab <- cpt$team$abbreviation
        if (!is.null(ha) && !is.null(ab)) {
          if (ha == "home") home_abbr <- ab
          if (ha == "away") away_abbr <- ab
        }
      }
      
      if (is.null(home_abbr) || is.null(away_abbr)) next
      
      rows[[length(rows) + 1]] <- tibble(
        ROAD_ABBR = away_abbr,
        HOME_ABBR = home_abbr,
        id        = paste0(away_abbr, "@", home_abbr),
        title     = paste0(away_abbr, " vs ", home_abbr)
      )
    }
    
    if (length(rows) == 0) {
      last_note <- paste0("ESPN parsed but found 0 games :: ", url)
      next
    }
    
    out <- bind_rows(rows) %>% distinct(ROAD_ABBR, HOME_ABBR, id, title)
    if (nrow(out) == 0) {
      last_note <- paste0("ESPN empty after distinct :: ", url)
      next
    }
    
    return(list(games = out, note = paste0("games=espn url=", url)))
  }
  
  return(list(games = NULL, note = last_note %||% "ESPN failed"))
}

espn <- get_games_from_espn(game_date_str)
games_today <- espn$games
note_games  <- espn$note

if (is.null(games_today) || nrow(games_today) == 0) {
  write_json(list(date = game_date_str, games = list(), note = paste0(note_games)))
  quit(save = "no", status = 0)
}

# ---------- Player stats (more reliable): nba_playerindex ----------
# Returns season per-game stats by player, includes TEAM_ABBREVIATION
get_player_stats <- function() {
  x <- tryCatch(
    nba_playerindex(
      season = season,
      season_type = "Regular Season"
    ),
    error = function(e) NULL
  )
  
  if (is.null(x) || length(x) == 0) return(NULL)
  
  # Different hoopR versions may name the table slightly differently; try common keys
  raw <- x[["PlayerIndex"]] %||% x[["LeaguePlayerIndex"]] %||% x[[1]]
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  
  # Normalize expected columns (best-effort)
  nm <- names(raw)
  
  # Some versions use "MIN" as total minutes; "MPG" for per-game. Prefer MPG if present.
  min_col <- if ("MPG" %in% nm) "MPG" else if ("MIN" %in% nm) "MIN" else NA_character_
  if (is.na(min_col)) return(NULL)
  
  # FG3M sometimes is "FG3M" or "FG3M_PG" in various feeds; try both.
  fg3m_col <- if ("FG3M" %in% nm) "FG3M" else if ("FG3M_PG" %in% nm) "FG3M_PG" else NA_character_
  
  # PTS/REB/AST usually exist; if not, stop.
  if (!("PTS" %in% nm && "REB" %in% nm && "AST" %in% nm)) return(NULL)
  if (!("TEAM_ABBREVIATION" %in% nm || "TEAM" %in% nm)) return(NULL)
  
  team_col <- if ("TEAM_ABBREVIATION" %in% nm) "TEAM_ABBREVIATION" else "TEAM"
  
  out <- raw %>%
    mutate(
      TEAM_ABBREVIATION = as.character(.data[[team_col]]),
      PLAYER_NAME       = as.character(PLAYER_NAME),
      MIN  = as.numeric(.data[[min_col]]),
      PTS  = as.numeric(PTS),
      REB  = as.numeric(REB),
      AST  = as.numeric(AST),
      FG3M = if (!is.na(fg3m_col)) as.numeric(.data[[fg3m_col]]) else NA_real_
    ) %>%
    select(TEAM_ABBREVIATION, PLAYER_NAME, MIN, PTS, REB, AST, FG3M) %>%
    filter(!is.na(TEAM_ABBREVIATION), TEAM_ABBREVIATION != "")
  
  out
}

player_stats <- get_player_stats()
if (is.null(player_stats) || nrow(player_stats) == 0) {
  write_json(list(date = game_date_str, games = list(), note = paste0(note_games, " | players=playerindex_failed")))
  quit(save = "no", status = 0)
}

note_players <- "players=nba_playerindex(season_avgs)"

# ---------- build games JSON ----------
games_out <- list()

for (i in seq_len(nrow(games_today))) {
  g <- games_today[i, ]
  home_abbr <- g$HOME_ABBR
  road_abbr <- g$ROAD_ABBR
  
  game_df <- player_stats %>%
    filter(TEAM_ABBREVIATION %in% c(home_abbr, road_abbr)) %>%
    # Keep your rule: MIN >= 18
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
  note  = paste0(note_games, " | ", note_players, " | filter=MIN>=18")
))

message("✅ Wrote docs/slate_today.json with ", length(games_out), " games")
