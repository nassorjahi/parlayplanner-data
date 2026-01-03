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

safe_str <- function(x, maxn = 300) {
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

# ---------- ESPN schedule (robust) ----------
# We try TWO ESPN endpoints and parse as LIST (simplifyVector=FALSE)
get_games_from_espn <- function(date_str) {
  d <- gsub("-", "", date_str)  # YYYYMMDD
  
  urls <- c(
    paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?dates=", d),
    paste0("https://site.web.api.espn.com/apis/v2/sports/basketball/nba/scoreboard?dates=", d)
  )
  
  last_note <- NULL
  
  for (url in urls) {
    resp <- tryCatch(
      request(url) |>
        req_timeout(30) |>
        req_perform(),
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
    
    js <- tryCatch(
      jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
      error = function(e) {
        last_note <<- paste0("ESPN JSON parse error: ", e$message, " body=", safe_str(body_txt, 120))
        NULL
      }
    )
    if (is.null(js)) next
    
    events <- js$events
    if (is.null(events) || length(events) == 0) {
      last_note <- paste0("ESPN no events (url ok) :: ", url)
      next
    }
    
    rows <- list()
    
    for (ev in events) {
      # competitions is usually a list; take first
      comp <- NULL
      if (!is.null(ev$competitions) && length(ev$competitions) >= 1) comp <- ev$competitions[[1]]
      if (is.null(comp)) next
      
      competitors <- comp$competitors
      if (is.null(competitors) || length(competitors) < 2) next
      
      # Find home/away
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
      last_note <- paste0("ESPN parsed but found 0 home/away games :: ", url)
      next
    }
    
    out <- bind_rows(rows) %>% distinct(ROAD_ABBR, HOME_ABBR, id, title)
    if (nrow(out) == 0) {
      last_note <- paste0("ESPN produced empty after distinct :: ", url)
      next
    }
    
    return(list(games = out, note = paste0("source=espn url=", url)))
  }
  
  return(list(games = NULL, note = last_note %||% "no games from ESPN (unknown)"))
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

espn <- get_games_from_espn(game_date_str)
games_today <- espn$games
source_note <- espn$note

if (is.null(games_today) || nrow(games_today) == 0) {
  write_json(list(date = game_date_str, games = list(), note = source_note))
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
  note  = source_note
))

message("✅ Wrote docs/slate_today.json with ", length(games_out), " games (", source_note, ")")
