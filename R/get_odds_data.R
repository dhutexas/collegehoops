################################### Get Odds Data ##############################
#' Get odds data for a given gameId
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @import dplyr tidyr httr jsonlite glue lubridate
#' @importFrom magrittr %>%
#'
#' @param espn_game_id (str) The eight digit gameId for the desired game
#' @return A dataframe with complete odds data, along with season and game info
#'

get_odds_data <- function(espn_game_id) {
  tryCatch(
    {
    # gather data from server
    game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={espn_game_id}")) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    # gather odds data
    game_json$pickcenter %>%
      dplyr::as_tibble() %>%
      dplyr::select(-c('links', 'provider.priority')) -> odds

    # gather basic game info
    comps = game_json$header$competitions$competitors[[1]] %>%
      dplyr::select(id, homeAway, winner, score, matches("rank"),
                    team.location, team.name)

    comps[1,] %>% dplyr::as_tibble() -> home
    colnames(home) <- paste("home", colnames(home), sep = "_")

    comps[2,] %>% dplyr::as_tibble() -> away
    colnames(away) <- paste("away", colnames(away), sep = "_")

    # combine datasets and add game data calculations to odds
    teams = dplyr::bind_cols(home, away)
    df = dplyr::bind_cols(odds, teams) %>%
      tidyr::separate(details, into=c('favorite', 'spread_line'), sep = ' ') %>%
      dplyr::mutate_at(dplyr::vars(overUnder, spread_line, home_score, away_score),
                       list(~as.numeric(.))) %>%
      dplyr::mutate(game_id = game_json$header$id,
                    season = game_json$header$season$year,
                    season_type = game_json$header$season$type,
                    date = lubridate::date(game_json$header$competitions$date),
                    total_score = home_score + away_score,
                    favorite_id = ifelse(homeTeamOdds.favorite == 'TRUE', home_id, away_id),
                    favorite_margin_of_victory = ifelse(homeTeamOdds.favorite == 'TRUE',
                                                        home_score - away_score,
                                                        away_score - home_score),
                    spread_gap = -1 * spread_line - favorite_margin_of_victory,
                    beat_spread = ifelse(-1 * spread_line - favorite_margin_of_victory < spread_line, 'TRUE', 'FALSE'),
                    over_under_gap = total_score - overUnder,
                    beat_over_under = dplyr::case_when(total_score > overUnder ~ 'OVER',
                                                       total_score < overUnder ~ 'UNDER',
                                                       TRUE ~ 'TIE')) %>%
      dplyr::select(game_id, date, season, favorite_id, spread_line, favorite_margin_of_victory,
                    spread_gap, beat_spread, overUnder, total_score, over_under_gap,
                    beat_over_under, home_score, away_score, dplyr::everything())

    return(df)
    },
    error=function(cond) {
      message(paste("No odds data available for gameId : ", espn_game_id))
    }
  )
}


