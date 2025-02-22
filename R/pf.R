##  Performance Factors  #########################

model_WR.est <- function(DT, measure.perf = "WR", params = NULL, WR.est = "WR.est") {
  ## return lm() or add the col?
}

calc_WR.est <- function() {

}

pf_prep <- function(stats,
                    measure.perf = "all.wins",
                    measure.battles = "all.battles",
                    measure.relative = NULL,
                    min.battles = 30,
                    min.players = 50,
                    max.battles = NULL,
                    afk_wr = NULL) {
  if (is.null(measure.relative)) {
    if (grepl("WR", measure.perf)) {
      measure.relative <- TRUE
    } else if (measure.perf %in% c("all.wins", "all.wins.new")) {
      measure.relative <- FALSE
    } else {
      stop("'measure.relative' == NULL, but performance  measure not known. Please excplicitely set 'measure.relative'")
    }
  }

  ## Filter stats by minimum battles played
  stats <- stats[, .(region, tank_id,
    "measure.battles" = get(measure.battles),
    "measure.perf" = get(measure.perf),
    "BATTLES.LIM" = sum(get(measure.battles))
  ), by = account_id]
  stats <- subset(stats, BATTLES.LIM >= min.battles)


  if (!is.null(max.battles)) {
    if (max.battles < 1) {
      max.battles <- quantile(stats$measure.battles, probs = max.battles, na.rm = TRUE)
    }
    if (measure.relative) {
      stats[
        measure.battles > max.battles,
        "measure.battles" := max.battles
      ]
    } else {
      stats[
        measure.battles > max.battles,
        c("measure.battles", "measure.perf") := list(max.battles, round(measure.perf * max.battles / measure.battles))
      ]
    }
  }

  res.DT <- dcast.data.table(stats,
    account_id ~ tank_id,
    value.var = "measure.perf", fill = 0
  )
  btls.DT <- dcast.data.table(stats,
    account_id ~ tank_id,
    value.var = "measure.battles", fill = 0
  )

  account_ids <- res.DT[, account_id]
  account_ids <- setDT(stats[, .(region = first(region)), by = account_id], key = "account_id")[J(account_ids), .(account_id, region)]

  res.DT[, account_id := NULL]
  btls.DT[, account_id := NULL]

  tank_ids <- as.integer64(colnames(res.DT))

  played_mtrx <- btls.DT
  played_mtrx[played_mtrx > 0] <- 1

  if (measure.relative) {
    res.DT <- btls.DT * res.DT
  }
  ## Zero skill will get "AFK WR" that is ~15-25% depending on the tier
  if (!is.null(afk_wr)) {
    res.DT <- res.DT - btls.DT * afk_wr
  }

  # remove players who have not played at least 2 tanks
  account_ids <- subset(account_ids, rowSums(played_mtrx) > 1)
  res.DT <- subset(res.DT, rowSums(played_mtrx) > 1)
  btls.DT <- subset(btls.DT, rowSums(played_mtrx) > 1)

  players.tank <- colSums(played_mtrx)
  tanks.enough_players.ndx <- seq_len(length(players.tank))[players.tank >= min.players]

  tank_ids <- tank_ids[tanks.enough_players.ndx]
  res.DT <- res.DT[, ..tanks.enough_players.ndx]
  btls.DT <- btls.DT[, ..tanks.enough_players.ndx]

  return(list(
    "account_ids" = account_ids,
    "tank_ids" = tank_ids,
    "battles" = as.matrix(btls.DT),
    "results" = as.matrix(res.DT)
  ))
}


pf_est_factors <- function(btl_mtrx = NULL, res_mtrx = NULL,
                           player.skill = NULL, tank.perf = NULL,
                           mode = "default",
                           verbose = TRUE, debug = FALSE,
                           player.skill.first = TRUE,
                           rounds = NULL,
                           MSE.delta = NULL,
                           debug.convergence = NULL) {
  modes <- list(
    "default" = list("rounds" = 100, "MSE" = 1e-14),
    "quick" = list("rounds" = 20, "MSE" = 1e-10),
    "best" = list("rounds" = 5000, "MSE" = 0)
  )

  if (!is.null(mode)) {
    if (mode %in% names(modes)) {
      if (is.null(MSE.delta)) MSE.delta <- modes[[mode]][["MSE"]]
      if (is.null(rounds)) rounds <- modes[[mode]][["rounds"]]
    } else {
      stop(paste0("Invalid argument mode=", mode))
    }
  }

  if (!is.null(debug.convergence)) {
    debug <- TRUE
  }
  if (verbose || debug) {
    tic("est_tank_player_perf() total")
    if (!debug) {
      frmt <- paste("Estimating player/tank factors: [:bar] :current/:total (:percent)")
      pb <- progress_bar$new(total = rounds, format = frmt)
    }
  }

  if (player.skill.first) {
    mod_i <- 1
  } else {
    mod_i <- 0
  }

  # btl_mtrx is players * tanks
  if (is(btl_mtrx, "list")) {
    res_mtrx <- btl_mtrx[["results"]]
    btl_mtrx <- btl_mtrx[["battles"]]
  }
  ## data.table Conversion
  if (!is.matrix(btl_mtrx)) btl_mtrx <- as.matrix(btl_mtrx)
  if (!is.matrix(res_mtrx)) res_mtrx <- as.matrix(res_mtrx)

  # starting values
  ## players' average WR
  if (is.null(player.skill)) {
    player.skill <- rowSums(res_mtrx) / rowSums(btl_mtrx)
  }
  ## tanks' average WR
  if (is.null(tank.perf)) {
    tank.perf <- 2 * colSums(res_mtrx) / colSums(btl_mtrx)
  }


  N <- pf_N(btl_mtrx)
  MSE.old <- pf_MSE(player.skill, tank.perf, btl_mtrx, res_mtrx)

  res <- list()
  if (!is.null(debug.convergence)) {
    R2 <- pf_R2(player.skill, tank.perf, btl_mtrx, res_mtrx)
    res[["Start"]] <- pf_est_gen_res(player.skill, tank.perf, MSE.old, R2)
  }

  for (i in seq_len(rounds)) {
    # estimate player skill / tank performance by taking turns
    if (i %% 2 == mod_i) {
      player.skill <- pf_opt_player_skill(tank.perf, btl_mtrx, res_mtrx)
    } else {
      tank.perf <- pf_opt_tank_perf(player.skill, btl_mtrx, res_mtrx)
    }

    MSE <- pf_MSE(player.skill, tank.perf, btl_mtrx, res_mtrx)

    if (debug) {
      R2 <- pf_R2(player.skill, tank.perf, btl_mtrx, res_mtrx)
      cat(paste("Round:", i, "MSE:", MSE, "R-squared", R2, "\n"))
      if (!is.null(debug.convergence) && (i %% debug.convergence == 0)) {
        res[[as.character(i)]] <- pf_est_gen_res(player.skill, tank.perf, MSE, R2)
      }
    } else if (verbose) {
      pb$tick(1)
    }

    if (abs(MSE - MSE.old) / MSE <= MSE.delta) {
      break
    }
    MSE.old <- MSE
  } # for round

  R2 <- pf_R2(player.skill, tank.perf, btl_mtrx, res_mtrx)

  if (verbose || debug) {
    if (!debug) {
      pb$terminate()
    }
    toc_()
    cat(paste("\nFinal R-squared:", R2, "MSE:", MSE, "\n"))
  }
  # normalization to mean(tank.perf) == 1
  tank.avg <- mean(tank.perf)
  tank.perf <- tank.perf / tank.avg
  player.skill <- player.skill * tank.avg

  res.final <- pf_est_gen_res(player.skill, tank.perf, MSE, R2)

  if (is.null(debug.convergence)) {
    return(res.final)
  } else {
    res[["Final"]] <- res.final
    return(res)
  }
}

pf_est_gen_res <- function(player.skill, tank.perf, MSE, R2) {
  return(list(
    "player.skill" = player.skill,
    "tank.perf" = tank.perf,
    "MSE" = MSE,
    "R-squared" = R2
  ))
}


pf_get_tank_perf <- function(res.perf, topN = 0) {
  tank.perf <- res.perf[["tank.perf"]]
  tank_ids <- as.integer(names(tank.perf))
  tank_names <- get_tank_name(tank_ids)

  dat <- data.table("tank_id" = tank_ids, "name" = tank_names, "tank.perf" = tank.perf)

  if (topN > 0) {
    return(dat[order(-tank.perf)][seq(topN)])
  } else if (topN < 0) {
    return(dat[order(tank.perf)][seq(-topN)])
  } else {
    return(dat)
  }
}


pf_get_player_skill <- function(params, perf, topN = 0, min.battles = NULL, only_region = NULL) {
  account_ids <- params[["account_ids"]][, account_id]
  regions <- params[["account_ids"]][, region]
  player_skill <- perf[["player.skill"]]

  dat <- data.table(
    "account_id" = account_ids,
    "region" = regions,
    "player.skill" = player_skill
  )

  if (!is.null(min.battles)) {
    battles_played <- params[["battles"]]
    enough_battles <- rowSums(battles_played) > min.battles #  bool vector
    dat <- subset(dat, enough_battles)
  }

  if (is.null(only_region)) {
    if (topN > 0) {
      return(dat[order(-player.skill)][seq(topN)])
    } else if (topN < 0) {
      return(dat[order(player.skill)][seq(-topN)])
    } else {
      return(dat)
    }
  } else {
    if (topN > 0) {
      return(dat[region == only_region][order(-player.skill)][seq(topN)])
    } else if (topN < 0) {
      return(dat[region == only_region][order(player.skill)][seq(-topN)])
    } else {
      return(dat[region == only_region])
    }
  }
}


pf_pft_mtrx <- function(player.skill, tank.perf) {
  return(player.skill %*% t(tank.perf))
}


pf_estimated_res <- function(player.skill, tank.perf, btl_mtrx) {
  return(pf_pft_mtrx(player.skill, tank.perf) * btl_mtrx)
}


pf_error_mtrx <- function(player.skill, tank.perf, btl_mtrx, res_mtrx) {
  return(pf_estimate_res(player.skill, tank.perf, btl_mtrx) - res_mtrx)
}


pf_opt_player_skill <- function(tank.perf, btl_mtrx, res_mtrx) {
  return(rowSums(t(t(btl_mtrx * res_mtrx) * tank.perf)) /
    rowSums(t(t(btl_mtrx) * tank.perf)^2))
}


pf_opt_tank_perf <- function(player.skill, btl_mtrx, res_mtrx) {
  return(colSums(btl_mtrx * res_mtrx * player.skill) /
    colSums((btl_mtrx * player.skill)^2))
}


pf_MSE <- function(player.skill, tank.perf, btl_mtrx, res_mtrx) {
  N <- pf_N(btl_mtrx)
  sse <- pf_SSE(player.skill, tank.perf, btl_mtrx, res_mtrx)
  return(sse / N)
}


pf_N <- function(btl_mtrx) {
  return(sum(btl_mtrx > 0))
}


pf_SSE <- function(player.skill, tank.perf, btl_mtrx, res_mtrx) {
  sse <- sum((player.skill %*% t(tank.perf) * btl_mtrx - res_mtrx)^2)
  return(sse)
}


pf_R2 <- function(player.skill, tank.perf, btl_mtrx, res_mtrx) {
  sse <- pf_SSE(player.skill, tank.perf, btl_mtrx, res_mtrx)
  mean_res <- sum(res_mtrx) / sum(btl_mtrx > 0)
  return(1 - sse / sum((res_mtrx - mean_res)^2, na.rm = TRUE))
}


pf_R2.adj <- function(player.skill, tank.perf, btl_mtrx, res_mtrx) {
  R2 <- pf_R2(player.skill, tank.perf, btl_mtrx, res_mtrx)
  p <- sum(dim(btl_mtrx))
  n <- sum(btl_mtrx > 0)
  return(1 - (1 - R2) * (n - 1) / (n - p - 1))
}

#
#
# mtrx_row_product <- function(mtrx, vec) {
# 	if (! is.data.table(mtrx)) {
# 	  res <- as.data.table(mtrx)
# 	} else {
# 	  res <- copy(mtrx)
# 	}
#   for (col in seq_along(res)) {
#     set(res, i=NULL, j=col, value=res[[col]] * vec[col])
#     # res[, (i):= res[[i]] * vec[i]]
#   }
# 	return(res)
# }
#
#
# mtrx_row_product.old <- function(mtrx, vec) {
#   if (! is.data.table(mtrx)) {
#     res <- as.data.table(mtrx)
#   } else {
#     res <- copy(mtrx)
#   }
#   for (col in seq_along(res)) {
#     res[, (col):= res[[col]] * vec[col]]
#   }
#   return(res)
# }
#
