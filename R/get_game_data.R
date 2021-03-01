################################### Get Game Data from ESPN ####################
#' Get game data (play-by-play, odds, and game details) for a given gameId
#'
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @import dplyr tidyr stringr stats httr jsonlite glue
#' @importFrom magrittr %>%
#'
#' @param espn_game_id (str) The eight digit gameId for the desired game
#' @return A dataframe with complete game play-by-play data, along with season and game info
#'

get_game_data <- function(espn_game_id) {
  tryCatch(
    {

      # Pull the JSon
      game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      #### extract the data from the json ####

      # game info for the teams
      comps = game_json$header$competitions$competitors[[1]] %>%
        dplyr::select(id, homeAway, winner, score, matches("rank"),
               team.location, team.name)

      # create new columns with home team data
      comps %>%
        dplyr::filter(homeAway == 'home') %>%
        dplyr::select(-homeAway) %>%
        stats::setNames(paste0('home_', names(.))) -> home

      # create new columns with away team data
      comps %>%
        dplyr::filter(homeAway == 'away') %>%
        dplyr::select(-homeAway) %>%
        stats::setNames(paste0('away_', names(.))) -> away

      # join home and away data into single df, remove previous dfs
      cbind(home, away) %>%
        dplyr::mutate(game_id = game_json$header$id,
               season = game_json$header$season$year,
               season_type = game_json$header$season$type) %>%
        dplyr::select(game_id, season, season_type, dplyr::everything()) -> comps

      rm(away, home)

      # pull a df of all the plays and shots
      game_plays = game_json$plays %>%
        dplyr::mutate(game_id = game_json$header$id,
               season = game_json$header$season$year,
               season_type = game_json$header$season$type) %>%
        tidyr::separate(participants,
                        into=c('player1','player2'),
                        sep = ',',
                        fill = 'right') %>%
        dplyr::mutate(player1 = as.numeric(stringr::str_extract_all(player1, "\\d{7}")),
               player2 = as.numeric(stringr::str_extract_all(player2, "\\d{7}")))

      # join the data and return a dataframe with everything
      game_plays %>%
        dplyr::left_join(comps, by = c('game_id','season','season_type')) -> game_data

      return(game_data)
    },
    error=function(cond) {
      message(paste("No pbp data available for gameId : ", espn_game_id))
      return(NULL) # does not return any information about the game if an error occurs
    }
  )
}
