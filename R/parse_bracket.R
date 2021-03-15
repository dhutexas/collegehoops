######################## Convert Predictions to Bracket Format ####################
#' Convert Kaggle predictions to bracket format
#' from zachmayer/kaggleNCAA on github
#'
#' @title Generates a dataframe of predictions ready to be printed to bracket
#'
#' @description Given a csv file of kaggle game predictions, and season, this function
#' returns data in a format which can be easily plotted
#'
#' @import data.table tidyr dplyr utils
#'
#'

parse_bracket <- function (prediction_file, year = 2021)
{
  utils::data("all_slots", package = "collegehoops", envir = environment())

  # read in kaggle submission file, return parsed bracket
  utils::read.csv(prediction_file) %>%
    tidyr::separate(ID, into = c('season', 'teamid_1', 'teamid_2'), sep = '_') %>%
    dplyr::rename(pred = Pred) %>%
    dplyr::mutate(women = 0,
                  season = as.integer(season),
                  teamid_1 = as.integer(teamid_1),
                  teamid_2 = as.integer(teamid_2)) %>%
    dplyr::select(season, teamid_1, teamid_2, women, pred) %>%
    data.table::as.data.table() -> preds

  season = year
  preds <- preds[season == year, ]
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by = c("season", "teamid_1",
                                          "teamid_2", "women"))
  stopifnot(n1 == nrow(preds))
  preds[, `:=`(seed_1_int, as.integer(substr(seed_1, 2, 3)))]
  preds[, `:=`(seed_2_int, as.integer(substr(seed_2, 2, 3)))]
  preds[, `:=`(rand, 0.5)]

  small_num <- 1e-06
  preds[, `:=`(pred, pred + stats::runif(.N, min = -1 * small_num,
                                         max = small_num))]
  preds[, `:=`(winner, ifelse(pred > rand, teamid_1, teamid_2))]
  preds[, `:=`(keep, 1L)]
  #sims <- sim_tourney_internal(preds)

  # previously sim_tourney_internal()
  all_rounds <- sort(unique(preds$round))
  if (all_rounds[1] == 0) {
    r <- 0L
    round_teamid_1 <- preds[round == r, list(slot = next_slot,
                                             teamid_1 = winner, keep_teamid_1 = 1L)]
    round_teamid_2 <- preds[round == r, list(slot = next_slot,
                                             teamid_2 = winner, keep_teamid_2 = 1L)]
    preds <- merge(preds, round_teamid_1, by = c("slot",
                                                 "teamid_1"), all.x = TRUE)
    preds <- merge(preds, round_teamid_2, by = c("slot",
                                                 "teamid_2"), all.x = TRUE)
    preds[is.na(keep_teamid_1) & teamid_1_playedin == (r +
                                                         1L) & round == 1L, `:=`(keep, 0L)]
    preds[is.na(keep_teamid_2) & teamid_2_playedin == (r +
                                                         1L) & round == 1L, `:=`(keep, 0L)]
    preds <- preds[keep == 1L, ]
    preds[, `:=`(c("keep_teamid_1", "keep_teamid_2"), NULL)]
    all_rounds <- all_rounds[2:length(all_rounds)]
  }
  for (r in 1:5) {
    round_teamid_1 <- preds[round == r, list(slot = next_slot,
                                             teamid_1 = winner, keep_teamid_1 = 1L)]
    round_teamid_2 <- preds[round == r, list(slot = next_slot,
                                             teamid_2 = winner, keep_teamid_2 = 1L)]
    preds <- merge(preds, round_teamid_1, by = c("slot",
                                                 "teamid_1"), all.x = TRUE)
    preds <- merge(preds, round_teamid_2, by = c("slot",
                                                 "teamid_2"), all.x = TRUE)
    preds[is.na(keep_teamid_1) & round == (r + 1L), `:=`(keep,
                                                         0L)]
    preds[is.na(keep_teamid_2) & round == (r + 1L), `:=`(keep,
                                                         0L)]
    preds <- preds[keep == 1L, ]
    preds[, `:=`(c("keep_teamid_1", "keep_teamid_2"), NULL)]
  }
  sims <- preds[, list(slot, round, teamid_1, teamid_2, women,
                        winner)]
  data.table::setkeyv(sims, "slot")
  # end of sim_tourney_internal()

  sims <- merge(sims, preds[, list(teamid_1, teamid_2, pred)],
                by = c("teamid_1", "teamid_2"), all.x = TRUE)
  sims[, `:=`(pred, ifelse(teamid_1 == winner, pred, 1 - pred))]
  data.table::setkeyv(sims, c("slot", "winner"))
  sims[, `:=`(count, 1)]
  data.table::setkeyv(sims, c("winner", "round"))
  sims[, `:=`(prob, cumprod(pred)), by = c("winner")]
  sims[, `:=`(season, year)]
  data.table::setkeyv(sims, c("slot", "winner"))
  return(sims)
}
