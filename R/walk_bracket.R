######################## Convert Predictions to Bracket Format ####################
#' Convert Kaggle predictions to bracket format
#' from zachmayer/kaggleNCAA on github
#'
#' @title Generate a printable NCAA bracket
#'
#' @description Given a dataframe of kaggle game predictions, this function
#' returns data in a format which can be easily plotted
#'

walkBracket <- function (preds, year = 2019, upset_bias = 0)
{
  utils::data("all_slots", package = "ncaaStats", envir = environment())
  preds <- preds[season == year, ]
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by = c("season", "teamid_1",
                                          "teamid_2", "women"))
  stopifnot(n1 == nrow(preds))
  preds[, `:=`(seed_1_int, as.integer(substr(seed_1, 2, 3)))]
  preds[, `:=`(seed_2_int, as.integer(substr(seed_2, 2, 3)))]
  preds[, `:=`(rand, 0.5)]
  if (upset_bias != 0) {
    preds[seed_1_int > seed_2_int, `:=`(rand, rand - upset_bias)]
    preds[seed_1_int < seed_2_int, `:=`(rand, rand + upset_bias)]
  }
  small_num <- 1e-06
  preds[, `:=`(pred, pred + stats::runif(.N, min = -1 * small_num,
                                         max = small_num))]
  preds[, `:=`(winner, ifelse(pred > rand, teamid_1, teamid_2))]
  preds[, `:=`(keep, 1L)]
  sims <- sim_tourney_internal(preds)
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
