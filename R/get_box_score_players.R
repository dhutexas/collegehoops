################################### Get Game Boxscore Data from ESPN ####################
#' Get box score data for the players, given a gameId
#'
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename bind_cols
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON
#'

espn_game_id = 401169632

get_box_score_players <- function(espn_game_id) {

  game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={espn_game_id}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  players_df <- game_json[["boxscore"]][["players"]] %>%
    tidyr::unnest(.data$statistics) %>%
    tidyr::unnest(.data$athletes)
  stat_cols <- players_df$names[[1]]
  stats <- players_df$stats

  stats_df <- as.data.frame(do.call(rbind,stats))
  colnames(stats_df) <- stat_cols

  players_df <- players_df %>%
    dplyr::filter(!.data$didNotPlay)

  player_box <- dplyr::bind_cols(players_df, stats_df) %>%
    dplyr::select(one_of(c('athlete.id', 'athlete.guid', 'athlete.displayName',
                  'athlete.shortName',
                  'athlete.jersey', 'athlete.headshot.href', 'athlete.position.name',
                  'athlete.position.abbreviation', 'team.id', 'team.slug',
                  'team.location', 'team.name', 'team.abbreviation',
                  'team.color', 'team.alternateColor', 'team.logo',
                  'MIN','FG','3PT','FT','OREB','DREB','REB','AST','STL','BLK',
                  'TO','PF', 'PTS')))

  return(player_box)

}

