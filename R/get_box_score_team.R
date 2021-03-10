################################### Get Game Boxscore Data from ESPN ####################
#' Get box score data for the teams, given a gameId
#'
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename bind_cols
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON
#' @import janitor lubridate
#'
#' @param espn_game_id (str) The eight digit gameId for the desired game
#' @return A dataframe with team-level box-score information
#'

get_box_score_team <- function(espn_game_id) {

  game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={espn_game_id}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  teams_box_score_df = game_json[['boxscore']][['teams']]

  teams_box_score_df_2 <- teams_box_score_df[[1]][[2]] %>%
    dplyr::select(.data$displayValue, .data$label) %>%
    dplyr::rename(Home = .data$displayValue)
  teams_box_score_df_1 <- teams_box_score_df[[1]][[1]] %>%
    dplyr::select(.data$displayValue) %>%
    dplyr::rename(Away = .data$displayValue)
  team_box_score = dplyr::bind_cols(teams_box_score_df_2, teams_box_score_df_1) %>%
    tidyr::pivot_wider(names_from = label, values_from = c('Home','Away')) %>%
    dplyr::mutate(game_id = game_json$header$id,
                  season = game_json$header$season$year,
                  season_type = game_json$header$season$type,
                  date = lubridate::date(game_json$header$competitions$date))

  # add team info to the data
  comps = game_json$header$competitions$competitors[[1]] %>%
    dplyr::select(id, homeAway, winner, score, matches("rank"),
                  team.location, team.name)

  comps[1,] %>% dplyr::as_tibble() -> home
  colnames(home) <- paste("home", colnames(home), sep = "_")

  comps[2,] %>% dplyr::as_tibble() -> away
  colnames(away) <- paste("away", colnames(away), sep = "_")

  # combine datasets and add game data calculations to odds
  box_score = dplyr::bind_cols(home, away, team_box_score) %>%
    janitor::clean_names() %>%
    dplyr::select(game_id, date, season, dplyr::everything())

  return(box_score)

}
