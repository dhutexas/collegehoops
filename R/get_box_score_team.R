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
  team_box_score = dplyr::bind_cols(teams_box_score_df_2, teams_box_score_df_1)

  return(team_box_score)

}
