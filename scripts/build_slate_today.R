library(jsonlite)
library(dplyr)
library(tibble)
library(httr2)

# ---------- helpers ----------
write_json <- function(obj) {
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(obj, auto_unbox = TRUE, pretty = TRUE), "docs/slate_today.json")
}

num_from <- function(x) {
  # Extract first numeric from strings like "26.4" or "2.1" or "26.4 PPG"
  if (is.null(x)) return(NA_real_)
  s <- as.character(x)
  m <- regmatches(s, regexpr("[-+]?[0-9]*\\.?[0-9]+", s))
  if (length(m) == 0 || m == "") return(NA_real_)
  as.numeric(m)
}

safe_req_json <- function(url) {
  resp <- tryCatch(
    request(url) |>
      req_header("User-Agent", "Mozilla/5.0") |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  if (resp_status(resp) < 200 || resp_status(resp) >= 300) return(NULL)
  
  txt <- tryCatch(resp_body_string(resp), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  
  tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

# ---------- date ----------
today <- Sys.Date()
date_str <- format(today, "%Y-%m-%d")
espn_date <- gsub("-", "", date_str)

# ---------- 1) ESPN scoreboard -> today's games ----------
score_urls <- c(
  paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?dates=", espn_date),
  paste0("https://site.web.api.espn.com/apis/v2/sports/basketball/nba/scoreboard?dates=", espn_date)
)

score <- NULL
score_url_used <- NULL
for (u in score_urls) {
  js <- safe_req_json(u)
  if (!is.null(js) && !is.null(js$events) && length(js$events) > 0) {
    score <- js
    score_url_used <- u
    break
  }
}

if (is.null(score)) {
  write_json(list(date = date_str, games = list(), note = "ESPN scoreboard failed/empty"))
  quit(save = "no", status = 0)
}

events <- score$events
if (is.null(events) || length(events) == 0) {
  write_json(list(date = date_str, games = list(), note = "ESPN scoreboard had no events"))
  quit(save = "no", status = 0)
}

# Extract games: team ids + abbreviations
games_tbl <- list()

for (ev in events) {
  comp <- NULL
  if (!is.null(ev$competitions) && length(ev$competitions) >= 1) comp <- ev$competitions[[1]]
  if (is.null(comp)) next
  
  competitors <- comp$competitors
  if (is.null(competitors) || length(competitors) < 2) next
  
  home <- NULL
  away <- NULL
  
  for (cpt in competitors) {
    ha <- cpt$homeAway
    tm <- cpt$team
    if (is.null(ha) || is.null(tm)) next
    if (ha == "home") home <- tm
    if (ha == "away") away <- tm
  }
  
  if (is.null(home) || is.null(away)) next
  if (is.null(home$id) || is.null(away$id)) next
  if (is.null(home$abbreviation) || is.null(away$abbreviation)) next
  
  games_tbl[[length(games_tbl) + 1]] <- tibble(
    HOME_ID   = as.character(home$id),
    HOME_ABBR = as.character(home$abbreviation),
    AWAY_ID   = as.character(away$id),
    AWAY_ABBR = as.character(away$abbreviation),
    id        = paste0(as.character(away$abbreviation), "@", as.character(home$abbreviation)),
    title     = paste0(as.character(away$abbreviation), " vs ", as.character(home$abbreviation))
  )
}

games_df <- bind_rows(games_tbl) %>% distinct(id, .keep_all = TRUE)

if (nrow(games_df) == 0) {
  write_json(list(date = date_str, games = list(), note = "ESPN events parsed but no games extracted"))
  quit(save = "no", status = 0)
}

# ---------- 2) ESPN team leaders -> top 5 categories ----------
# Endpoint pattern:
# https://site.web.api.espn.com/apis/site/v2/sports/basketball/nba/teams/{TEAM_ID}/leaders
get_team_leaders <- function(team_id) {
  url <- paste0(
    "https://site.web.api.espn.com/apis/site/v2/sports/basketball/nba/teams/",
    team_id,
    "/leaders"
  )
  js <- safe_req_json(url)
  if (is.null(js)) return(NULL)
  js
}

# Flatten ESPN leaders into a searchable table
flatten_leaders <- function(js) {
  if (is.null(js$categories) || length(js$categories) == 0) return(tibble())
  
  rows <- list()
  
  for (cat in js$categories) {
    # cat$name, cat$displayName sometimes
    if (is.null(cat$leaders) || length(cat$leaders) == 0) next
    
    for (block in cat$leaders) {
      # block can represent a stat type with list of leaders
      stat_name <- block$name %||% block$displayName %||% ""
      stat_key  <- tolower(gsub("[^a-z0-9]+", "", stat_name))
      
      if (is.null(block$leaders) || length(block$leaders) == 0) next
      
      for (ld in block$leaders) {
        ath <- ld$athlete
        if (is.null(ath) || is.null(ath$displayName)) next
        
        val <- ld$value %||% ld$displayValue %||% ld$displayValueShort %||% ld$displayValueAbbrev %||% ""
        rows[[length(rows) + 1]] <- tibble(
          stat_key = stat_key,
          stat_name = stat_name,
          playerName = as.character(ath$displayName),
          value = num_from(val)
        )
      }
    }
  }
  
  out <- bind_rows(rows)
  out %>% filter(!is.na(value))
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

pick_top5_for <- function(df, patterns) {
  # patterns: vector of regex-like keys we’ll match against stat_key
  # We pick the first pattern that has data.
  for (p in patterns) {
    x <- df %>% filter(grepl(p, stat_key))
    if (nrow(x) > 0) {
      return(
        x %>%
          arrange(desc(value)) %>%
          slice_head(n = 5) %>%
          transmute(playerName = playerName, value = value)
      )
    }
  }
  tibble(playerName = character(), value = numeric())
}

# Pattern sets (ESPN naming varies; we match broadly)
PTS_PATTERNS  <- c("pointspergame", "ppg", "points")
REB_PATTERNS  <- c("reboundspergame", "rpg", "rebounds")
AST_PATTERNS  <- c("assistspergame", "apg", "assists")
FG3M_PATTERNS <- c("threepointfieldgoalsmadepergame", "threepointmadepergame", "3pm", "threepointfieldgoalsmade", "threepoint")

# ---------- build games output ----------
games_out <- list()
leader_failures <- 0

for (i in seq_len(nrow(games_df))) {
  g <- games_df[i, ]
  
  # Pull leaders for both teams (away + home)
  away_js <- get_team_leaders(g$AWAY_ID)
  home_js <- get_team_leaders(g$HOME_ID)
  
  away_flat <- flatten_leaders(away_js)
  home_flat <- flatten_leaders(home_js)
  
  if (nrow(away_flat) == 0 || nrow(home_flat) == 0) {
    leader_failures <- leader_failures + 1
  }
  
  combined <- bind_rows(away_flat, home_flat)
  
  games_out[[length(games_out) + 1]] <- list(
    id = g$id,
    title = g$title,
    pointsTop5   = pick_top5_for(combined, PTS_PATTERNS),
    reboundsTop5 = pick_top5_for(combined, REB_PATTERNS),
    assistsTop5  = pick_top5_for(combined, AST_PATTERNS),
    threesTop5   = pick_top5_for(combined, FG3M_PATTERNS)
  )
}

note <- paste0(
  "games=espn url=", score_url_used,
  " | players=espn_team_leaders",
  " | leader_failures=", leader_failures
)

write_json(list(
  date  = date_str,
  games = games_out,
  note  = note
))

message("✅ Wrote docs/slate_today.json with ", length(games_out), " games")
