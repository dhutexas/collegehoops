################################### Get Full Season gameIds ####################
#' Get a full master schedule of all games in a season for a list of teams
#'
#' Can use a single team, a list of teams in a conference, or full D1 list
#' Teams must be in ESPN format, found in ids file (ids.csv)
#'
#' @import dplyr tibble
#' @importFrom magrittr %>%
#'
#' @param team (str) Team to get Play-by-Play data for
#' @param season (str) or (int) Season for which to get schedule. In form "2019-20" or "2020". Single number is season of tournament.
#' @return A data frame of the team, gameIds, and season


get_season_schedule <- function(team, season) {

  dfList <- list()  ## create empty list to hold data for each team

  for (i in team){

    get_game_ids(i, season) %>%
      tibble::enframe(name = NULL, value = 'game_ids') %>%
      dplyr::mutate(team = toString(i),
             season = toString(season)) -> team_df

    dfList[[i]] = team_df ## add team data to running list for each team

  }

  # combine teams into large df of the data
  schedule = do.call(rbind, c(dfList, make.row.names = FALSE))
  return(schedule)

}
