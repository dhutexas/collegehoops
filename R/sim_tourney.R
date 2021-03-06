sim_tourney_internal <- function (preds) {
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
  preds <- preds[, list(slot, round, teamid_1, teamid_2, women,
                        winner)]
  data.table::setkeyv(preds, "slot")
  return(preds)
}
