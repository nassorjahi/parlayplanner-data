library(jsonlite)
library(dplyr)
library(tibble)
library(httr2)

SCRIPT_VERSION <- "NBA_DATA_SCOREBOARD_V1"

out_path <- "docs/slate_today.json"

write_json <- function(obj) {
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(obj, auto_unbox = TRUE, pretty = TRUE), out_path)
}

read_existing <- function() {
  if (!file.exists(out_path)) return(NULL)
  txt <- tryCatch(readLines(out_path, warn = FALSE), error = function(e) NULL)
  if (is.null(txt) || length(txt) == 0) return(NULL)
  tryCatch(fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE), error = function(e) NULL)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

num_from <- function(x) {
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

today <- Sys.Date()
date_str <- format(today, "%Y-%m-%d")
yyyymmdd <- gsub("-", "", date_str)

# -------------------------
# 1) NBA DATA scoreboard (reliable)
# -------------------------
nba_score_url <- paste0("https://data.nba.com/data/10s/prod/v1/", yyyymmdd, "/scoreboard.json")
sb <- safe_req_json(nba_score_url)

if (is.null(sb) || is.null(sb$games) || length(sb$games) == 0) {
  existing <- read_existing()
  if (!is.null(existing)) {
    existing$note <- paste0(
      "version=", SCRIPT_VERSION,
      " | nba_data_scoreboard_failed_keep_last",
      " | url=", nba_score_url
    )
    write_json(existing)
    quit(save = "no", status = 0)
  } else {
    write_json(list(date = date_str, games = list(), note = paste0("version=", SCRIPT_VERSION, " | nba_data_scoreboard_failed_empty")))
    quit(save = "no", status = 0)
  }
}

games_tbl <- list()

for (g in sb$games) {
  # Team abbreviations are in hTeam$triCode and vTeam$triCode
  home_abbr <- g$hTeam$triCode %||% NA_character_
  away_abbr <- g$vTeam$triCode %||% NA_character_
  if (is.na(home_abbr) || is.na(away_abbr)) next
  
  # Keep NBA teamId for possible future expansions
  home_id <- g$hTeam$teamId %||% ""
  away_id <- g$vTeam$teamId %||% ""
  
  games_tbl[[length(games_tbl) + 1]] <- tibble(
    HOME_ID = as.character(home_id),
    HOME_ABBR = as.character(home_abbr),
    AWAY_ID = as.character(away_id),
    AWAY_ABBR = as.character(away_abbr),
    id = paste0(away_abbr, "@", home_abbr),
    title = paste0(away_abbr, " vs ", home_abbr)
  )
}

games_df <- bind_rows(games_tbl) %>% distinct(id, .keep_all = TRUE)

if (nrow(games_df) == 0) {
  existing <- read_existing()
  if (!is.null(existing)) {
    existing$note <- paste0("version=", SCRIPT_VERSION, " | nba_data_parsed_no_games_keep_last")
    write_json(existing)
  } else {
    write_json(list(date = date_str, games = list(), note = paste0("version=", SCRIPT_VERSION, " | nba_data_parsed_no_games")))
  }
  quit(save = "no", status = 0)
}

# -------------------------
# 2) ESPN Team Leaders (best-effort)
# If this fails, we still output games + empty lists OR keep-last
# -------------------------
get_team_leaders <- function(team_abbr) {
  # ESPN team search endpoint to resolve team id by abbreviation
  search_url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/teams?limit=1000")
  teams_js <- safe_req_json(search_url)
  if (is.null(teams_js) || is.null(teams_js$sports)) return(NULL)
  
  # Find team id
  team_id <- NULL
  for (sp in teams_js$sports) {
    if (is.null(sp$leagues) || length(sp$leagues) == 0) next
    lg <- sp$leagues[[1]]
    if (is.null(lg$teams) || length(lg$teams) == 0) next
    
    for (twrap in lg$teams) {
      tm <- twrap$team
      if (is.null(tm$abbreviation) || is.null(tm$id)) next
      if (toupper(tm$abbreviation) == toupper(team_abbr)) {
        team_id <- as.character(tm$id)
        break
      }
    }
  }
  
  if (is.null(team_id)) return(NULL)
  
  leaders_url <- paste0("https://site.web.api.espn.com/apis/site/v2/sports/basketball/nba/teams/", team_id, "/leaders")
  safe_req_json(leaders_url)
}

flatten_leaders <- function(js) {
  if (is.null(js$categories) || length(js$categories) == 0) return(tibble())
  rows <- list()
  
  for (cat in js$categories) {
    if (is.null(cat$leaders) || length(cat$leaders) == 0) next
    
    for (block in cat$leaders) {
      stat_name <- block$name %||% block$displayName %||% ""
      stat_key  <- tolower(gsub("[^a-z0-9]+", "", stat_name))
      
      if (is.null(block$leaders) || length(block$leaders) == 0) next
      
      for (ld in block$leaders) {
        ath <- ld$athlete
        if (is.null(ath) || is.null(ath$displayName)) next
        val <- ld$value %||% ld$displayValue %||% ""
        rows[[length(rows) + 1]] <- tibble(
          stat_key = stat_key,
          playerName = as.character(ath$displayName),
          value = num_from(val)
        )
      }
    }
  }
  
  out <- bind_rows(rows)
  out %>% filter(!is.na(value))
}

pick_top5_for <- function(df, patterns) {
  for (p in patterns) {
    x <- df %>% filter(grepl(p, stat_key))
    if (nrow(x) > 0) {
      return(
        x %>% arrange(desc(value)) %>% slice_head(n = 5) %>%
          transmute(playerName = playerName, value = value)
      )
    }
  }
  tibble(playerName = character(), value = numeric())
}

PTS_PATTERNS  <- c("pointspergame", "ppg", "points")
REB_PATTERNS  <- c("reboundspergame", "rpg", "rebounds")
AST_PATTERNS  <- c("assistspergame", "apg", "assists")
FG3M_PATTERNS <- c("threepointfieldgoalsmadepergame","threepointmadepergame","threepointfieldgoalsmade","threepoint")

games_out <- list()
leader_failures <- 0

for (i in seq_len(nrow(games_df))) {
  g <- games_df[i, ]
  
  away_js <- get_team_leaders(g$AWAY_ABBR)
  home_js <- get_team_leaders(g$HOME_ABBR)
  
  away_flat <- flatten_leaders(away_js)
  home_flat <- flatten_leaders(home_js)
  
  if (nrow(away_flat) == 0 || nrow(home_flat) == 0) leader_failures <- leader_failures + 1
  
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
  "version=", SCRIPT_VERSION,
  " | games=nba_data url=", nba_score_url,
  " | players=espn_team_leaders",
  " | leader_failures=", leader_failures
)

# If leaders fail for ALL games, keep last good instead of going blank
if (leader_failures >= nrow(games_df)) {
  existing <- read_existing()
  if (!is.null(existing) && !is.null(existing$games) && length(existing$games) > 0) {
    existing$note <- paste0(note, " | ALL_LEADERS_FAILED_KEEP_LAST")
    write_json(existing)
    quit(save = "no", status = 0)
  }
}

write_json(list(date = date_str, games = games_out, note = note))
message("✅ ", note)
