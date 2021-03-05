################################### Get Ken Pom Data ####################
#' Get season-level data for each D1 team from Ken Pomeroy's website
#'
#'
#' @import dplyr tidyr glue rvest janitor tibble rlang
#' @importFrom magrittr %>%
#'
#' @param season (str) The eight digit gameId for the desired game
#' @return A dataframe with complete season data for kenpom
#'

get_kenpom_data <- function(season) {

  # function to hide warnings from janitor
  # https://stackoverflow.com/questions/47475923/custom-pipe-to-silence-warnings
  `%W>%` <- function(lhs, rhs){
    w <- options()$warn
    on.exit(options(warn=w))
    options(warn=-1)
    lhs_quo = rlang::quo_name(rlang::enquo(lhs))
    rhs_quo = rlang::quo_name(rlang::enquo(rhs))
    pipe = paste(lhs_quo, "%>%", rhs_quo)
    return(rlang::eval_tidy(rlang::parse_quosure(pipe)))
  }

  # season format for kenpom website
  # current: https://kenpom.com/index.php
  # past: https://kenpom.com/index.php?y=2020
  if(season == '2021') {
    url <- 'https://kenpom.com/index.php'
  } else {
    url <- glue::glue("https://kenpom.com/index.php?y={season}")
  }

  # pull table into df
  kenpom <- xml2::read_html(url) %>%
    rvest::html_node("table") %>%
    rvest::html_table() %W>%
    janitor::row_to_names(., 1) %>%
    tibble::as_tibble(., .name_repair = janitor::make_clean_names) %>%
    dplyr::select(-c(adj_o_2, adj_d_2, adj_t_2, luck_2, adj_em_3,
              opp_o_2, opp_d_2, adj_em_5)) %>%
    dplyr::rename(sos_adj_em = adj_em_2,
           sos_opp_o = opp_o,
           sos_opp_d = opp_d,
           ncsos_adj_em = adj_em_4)

  # drop rows without data (repeated headers)
  kenpom = kenpom[!grepl("Strength of Schedule", kenpom$sos_opp_o),]
  kenpom = kenpom[!grepl("OppO", kenpom$sos_opp_o),]

  # convert char to double dtypes
  kenpom %>%
    dplyr::mutate_at(dplyr::vars(rk),
                     dplyr::funs(as.integer)) %>%
    dplyr::mutate_at(dplyr::vars(adj_em, adj_o, adj_d, adj_t, luck, sos_adj_em,
                   sos_opp_o, sos_opp_d, ncsos_adj_em),
                   dplyr::funs(as.double)) -> kenpom

  return(kenpom)

}
