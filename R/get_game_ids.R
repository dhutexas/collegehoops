################################### Get Game Ids from ESPN ####################
#' Get Team gameIds By Season
#'
#' Scrapes the season schedule for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @import rvest dplyr tibble stringr
#' @importFrom magrittr %>%
#'
#' @param team (str) Team to get Play-by-Play data for
#' @param season (str) or (int) Season for which to get schedule. In form "2019-20" or "2020". Single number is season of tournament.
#' @return A data frame of the team's gameIds data for the specified season.

get_game_ids <- function(team, season) {

  # standard url for team schedule page
  base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
  url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) )

    # read the html of the site, pulling gameIds from the recap section
    game_ids <- xml2::read_html(url) %>%
      rvest::html_nodes("a") %>% # get the a classes
      rvest::html_attr("href") %>% # get only the hyperlinks on the page
      tibble::as_tibble() %>%
      dplyr::filter(grepl('gameId', value)) %>% # retain only links for games
      dplyr::mutate(value = gsub("http://www.espn.com/mens-college-basketball/game\\?gameId=", "", value)) %>%
      dplyr::filter(!stringr::str_detect(value, 'recap')) %>% # remove the extra 'recap' games that show up
      dplyr::filter(!stringr::str_detect(value, 'preview')) %>%
      dplyr::pull()

  return(game_ids)

}
