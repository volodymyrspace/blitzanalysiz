## Blitz update, stats collection date (last stats date for release X)
source("R/utils.R")
source("R/utils_knitr.R")
source("R/utils_plot.R")
## R functions

library("primes")

get_stats.update.tier <- function(tierN = NULL, DT = NULL) {
  stopifnot(!is.null(tierN))
  if (!is.null(DT)) {
    stats.tier <- subset(DT, tier == tierN)
  } else if ((!exists("stats.tier")) ||
    (stats.tier[, first(tier)] != tierN)) {
    if (exists("stats.list")) {
      stats.tier <- stats.list[[tierN]]
    } else if (exists("stats.update")) {
      stats.tier <- subset(stats.update, tier == tierN)
    } else {
      stop("stats.tier not found")
    }
  }
  return(stats.tier)
}


get_stats.update.tier.type <- function(tierN = NULL, tank_type = NULL,
                                       tank_id = NULL, DT = NULL) {
  stopifnot(!(is.null(tierN) && is.null(tank_id)))
  stopifnot(!(is.null(tank_type) && is.null(tank_id)))

  if (is.null(tierN)) {
    tierN <- get_tank_tier(tank_id)
  }
  if (is.null(tank_type)) {
    tank_type <- get_tank_type(tank_id)
  }
  DT <- get_stats.update.tier(tierN = tierN, DT = DT)
  DT <- subset(DT, type == tank_type)

  return(DT)
}

# returns tanks that are maxed, have enough battles during the update
# and the player has enough career battles to be out of the 5k MM
# get_stats_tank_perf <- function(stats = NULL) {
# 	stopifnot(! is.null(stats))
# 	stats <- subset(stats, (! is.na(battles.tier.maxed)) &
# 													(battles > min_battles.tanks[tier]) &
# 													((region %in% names(regions)[c(3,4)]) | (battles.career > min_battles.career.5k_mm) ))
#
# 	return(stats)
# }

get_stats_tank_perf <- function(stats = NULL, sub5k = FALSE) {
  stopifnot(!is.null(stats))
  if (!sub5k) {
    stats <- subset(stats, (battles.career > min_battles.grinds[tier]) &
      (battles > min_battles.tanks[tier]))
  } else {
    stats <- subset(stats, (battles.career > min_battles.grinds[tier]) &
      (battles > min_battles.tanks[tier]) &
      ((region %in% names(regions)[c(3, 4)]) | (battles.career.all > 5e3)))
  }
  return(stats)
}

get_stats_tank_perf.rWR <- function(stats = NULL) {
  stopifnot(!is.null(stats))
  stats <- subset(stats, !is.na(rWR))
  return(stats)
}


get_stats_tank <- function(tank_ID, DT = NULL) {
  stopifnot(!is.null(tank_ID))
  tierN <- get_tank_tier(tank_ID)

  if (is.null(DT)) {
    found <- FALSE
    statDTs <- c(
      "stats.update.tank.perf", "stats.update.tank",
      "stats.tier.type.perf", "stats.tier.type",
      "stats.tier.perf", "stats.tier.perf",
      "stats.update.perf", "stats.update"
    )
    for (stats in statDTs) {
      if (exists(stats) && (get(stats)[tank_id == tank_ID, .N] > 0)) {
        found <- TRUE
        if ((stats != "stats.update.tank") && (stats != "stats.update.tank.perf")) {
          stats.update.tank <- subset(get(stats), tank_id == tank_ID)
        }
        if (!endsWith(stats, ".perf")) {
          stats.update.tank <- get_stats_tank_perf(stats.update.tank)
        }
        break
      }
    } # for
    if (!found) {
      stop(paste("no stats found for tank_id=", tank_ID))
    }
  } else {
    stats.update.tank <- subset(DT, tank_id == tank_ID)
  }
  return(stats.update.tank)
}


get_path_elem <- function(page, ndx) {
  return(strsplit(page, "/")[[1]][[ndx]])
}


get_path_elem_int <- function(page, ndx) {
  return(as.integer(strsplit(page, "/")[[1]][[ndx]]))
}


path2tank_type <- function(tank_type) {
  path.tank_types <- paste0(Tank_Types, "s")
  ndx <- match(tank_type, path.tank_types)
  return(Tank_Types[[ndx]])
}


## Load stats #########################################


load_stats <- function(filename, do_rbind = FALSE,
                       only_active_since = NULL, active_players = NULL) {
  stats <- NULL
  if (file.exists(filename)) {
    cat(paste("Reading file:", basename(filename), "\n"))
    stats <- qread(filename)
  } else if (file.exists(filename_part(filename, 0))) {
    stats <- linkedList()
    ndx <- 0
    filename.tmp <- filename_part(filename, ndx)
    while (file.exists(filename.tmp)) {
      cat(paste("Reading filepart:", basename(filename.tmp), "\n"))
      stats.tmp <- qread(filename.tmp)

      if (!is.null(active_players)) {
        stats.tmp <- subset(stats.tmp, account_id %in% active_players)
      } else if (!is.null(only_active_since)) {
        stats.tmp <- accounts_active_since(stats.tmp, last_battle_time = only_active_since)
      }
      stats$add(stats.tmp)

      stats.tmp <- NULL
      ndx <- ndx + 1
      filename.tmp <- filename_part(filename, ndx)
    }
    stats <- stats$as.list()
  }
  if (is(stats, "list") && do_rbind) {
    stats <- rbindlist(stats, use.names = TRUE, fill = TRUE)
  }
  return(stats)
}


load_tankopedia <- function(update = get_latest_update(preliminary)) {
  filename <- filename_tankopedia(update)
  cat(paste("Reading tankopedia for", update, "\n"))
  stats <- qread(filename)
  return(stats)
}



load_tank_stats_update <- function(update = get_latest_update(preliminary),
                                   do_rbind = TRUE) {
  filename <- filename_update(update)
  cat(paste("Reading tank stats for", update, "\n"))
  stats <- load_stats(filename, do_rbind = do_rbind)
  return(stats)
}


load_tank_stats_update_totals <- function(update = get_latest_update(),
                                          do_rbind = FALSE) {
  filename <- filename_update_totals(update)
  cat(paste("Reading tank stats (update totals) for", update, "\n"))
  stats <- load_stats(filename, do_rbind = do_rbind)
  return(stats)
}


load_tank_stats_period <- function(update.start, update.end,
                                   do_rbind = FALSE) {
  filename <- filename_period(update.start, update.end)
  cat(paste("Reading tank stats for period", update.start, "-", update.end, "\n"))
  stats <- load_stats(filename, do_rbind = do_rbind)
  return(stats)
}


load_tank_stats_period_totals <- function(update.start, update.end,
                                          do_rbind = FALSE) {
  filename <- filename_period_totals(update.start, update.end)
  cat(paste("Reading tank stats for period (totals)", update.start, "-", update.end, "\n"))
  stats <- load_stats(filename, do_rbind = do_rbind)
  return(stats)
}


# load_tank_stats_cum <- function(update = get_latest_update(),
#                 only_active = FALSE, active_players=NULL,
#                 do_rbind=TRUE, src=FALSE) {
#   if (src) {
#     filename <- filename_cumulative_src(update)
#   } else {
#     filename <- filename_cumulative(update)
#   }
#   cat(paste('Reading cumulative tank stats for' , update, '\n'))

#   if (only_active) {
#     update_start <- get_update_start(update)
#   } else {
#     update_start <- NULL
#   }

#   stats <- load_stats(filename, do_rbind = do_rbind,
#             only_active_since = update_start,
#             active_players = active_players)

#   return(stats)
# }


load_tank_stats_career <- function(update = get_latest_update(),
                                   only_active = FALSE,
                                   active_players = NULL,
                                   do_rbind = TRUE) {
  filename <- filename_career(update)

  if (only_active) {
    update_start <- get_update_start(update)
  } else {
    update_start <- NULL
  }

  stats <- load_stats(filename,
    do_rbind = do_rbind,
    only_active_since = update_start,
    active_players = active_players
  )

  return(stats)
}


load_tank_stats_cum_before <- function(update = get_latest_update(),
                                       only_active = FALSE,
                                       active_players = NULL,
                                       do_rbind = TRUE) {
  filename <- filename_cumulative_before(update)

  if (only_active) {
    update_start <- get_update_start(update)
  } else {
    update_start <- NULL
  }

  stats <- load_stats(filename,
    do_rbind = do_rbind,
    only_active_since = update_start,
    active_players = active_players
  )

  return(stats)
}


load_tank_stats_cum_after <- function(update = get_latest_update(),
                                      only_active = FALSE,
                                      active_players = NULL,
                                      do_rbind = TRUE) {
  filename <- filename_cumulative_after(update)

  if (only_active) {
    update_start <- get_update_start(update)
  } else {
    update_start <- NULL
  }

  stats <- load_stats(filename,
    do_rbind = do_rbind,
    only_active_since = update_start,
    active_players = active_players
  )

  return(stats)
}


load_tank_stats_summary <- function(update = get_latest_pdate(), do_rbind = TRUE) {
  filename <- filename_tank_summary_stats(update)
  return(load_stats(filename, do_rbind = do_rbind))
}


stats.exists <- function(filename, file_part = NULL) {
  if (is.null(file_part)) {
    if (file.exists(filename)) {
      return(TRUE)
    } else {
      return(file.exists(filename_part(filename, 0)))
    }
  } else {
    return(file.exists(filename_part(filename, file_part)))
  }
}



## Process tank stats ##########################################################


clean_tank_stats <- function(DT) {
  if (nrow(DT) == 0) {
    return(DT)
  }
  # Remove unneeded cols
  DT <- drop_cols.clean(DT)

  ## remove stats with zero last_battle_time (most of these are empty stats anyways)
  del.ndx <- DT[, .I[(last_battle_time < 1) | (battles < 1) |
    (battles < all.wins) | (battles < all.losses)]]
  DT <- delete(DT, del.ndx)

  ## Set account_id 64bit (it is double)
  DT[, account_id := as.integer64(account_id)]

  ## China server IDs seem to be >31e8 JP 2020-05-09
  DT[(account_id >= 5e8 & (account_id < 1e9)), region := as.factor("eu")]
  DT[(account_id < 5e8), region := as.factor("ru")]
  DT[(account_id >= 1e9) & (account_id < 2e9), region := as.factor("com")]
  DT[(account_id >= 2e9) & (account_id < 31e8), region := as.factor("asia")]
  DT[(account_id >= 31e8), region := as.factor("china")]
  DT[, region := as.factor(region)]

  ## Fix tank_ids
  # E25
  DT[tank_id == 55569, tank_id := 64529]

  return(DT)
} # function


calc_tank_stats <- function(DT, career_stats = FALSE) {
  ## career_stats : DT is career_stats, not update stats.
  # Tier
  # setattr(DT$tier, 'levels', seq(1,10))

  DT[, WR := all.wins / battles]
  DT[, spot_rate := all.spotted / battles]
  DT[, hit_rate := all.hits / all.shots]
  DT[, avg_hits := all.hits / battles]
  DT[, avg_dmg := all.damage_dealt / battles]
  DT[, avg_kills := all.frags / battles]
  DT[, KDR := all.frags / (battles - all.survived_battles)]
  DT[, DR := all.damage_dealt / all.damage_received]
  DT[, survival_rate := all.survived_battles / battles]
  DT[, survival_win_rate := all.win_and_survived / battles]
  DT[, avg_xp := all.xp / battles]

  ## Calculate Tier stats of all same tier tanks
  DT[, battles.tier.all := sum(battles), by = .(account_id, tier)]

  DT[, c(
    "WR.tier.all",
    "avg_dmg.tier.all",
    "spot_rate.tier.all"
  ) := list(
    sum(all.wins) / first(battles.tier.all),
    sum(all.damage_dealt) / first(battles.tier.all),
    sum(all.spotted) / first(battles.tier.all)
  ),
  by = .(account_id, tier)
  ]

  ## Tier stats for non-stock tanks
  if (career_stats) {
    DT[DT_filter_tank_is_maxed2(battles, tier, is_premium),
      battles.tier.maxed := sum(battles),
      by = .(account_id, tier)
    ]
  } else {
    DT[DT_filter_tank_is_maxed2(battles.new, tier, is_premium),
      battles.tier.maxed := sum(battles),
      by = .(account_id, tier)
    ]
  }

  DT[
    (!is.na(battles.tier.maxed)) &
      DT_filter_battles_tier(battles.tier.maxed, tier),
    c(
      "WR.tier.maxed",
      "avg_dmg.tier.maxed",
      "spot_rate.tier.maxed",
      "tanks.tier.maxed"
    ) := list(
      sum(all.wins) / first(battles.tier.maxed),
      sum(all.damage_dealt) / first(battles.tier.maxed),
      sum(all.spotted) / first(battles.tier.maxed),
      .N
    ),
    by = .(account_id, tier)
  ]

  # DT[ tanks.tier > 1, rWR := WR - (battles.tier*WR.tier - all.wins)/(battles.tier - battles)];
  # 2020-06-03: changed to comparison to all same tier tank's WR

  DT[
    (!is.na(tanks.tier.maxed)) & (tanks.tier.maxed > 1) &
      DT_filter_battles_tank(battles, tier),
    rWR := WR - WR.tier.maxed
  ]

  DT[, WR.global := sum(all.wins) / sum(battles), by = account_id]
  DT[, battles.global := sum(battles), by = account_id]
  DT[, avg_dmg.global := sum(all.damage_dealt) / first(battles.global), by = account_id]
  DT[, spot_rate.global := sum(all.spotted) / first(battles.global), by = account_id]

  if (!career_stats) {
    DT[, WR.career := sum(all.wins.new) / sum(battles.new), by = account_id]
    DT[, battles.career := sum(battles.new), by = account_id]
  }

  return(DT)
} # function


calc_tank_stats_cum <- function(DT) {
  if ("battles.new" %in% colnames(DT)) {
    DT[, battles.career := sum(battles.new), by = account_id]
    DT[, WR.career := sum(all.wins.new) / first(battles.career), by = account_id]
  } else {
    DT[, battles.career := sum(battles), by = account_id]
    DT[, WR.career := sum(all.wins) / first(battles.career), by = account_id]
  }
  return(DT)
}


drop_cols.tankopedia <- function(DT) {
  cols.drop <- c("name", "tier", "nation", "type", "is_premium")
  return(drop_cols(DT, cols.drop))
} # function


drop_cols.old <- function(DT) {
  for (col in colnames(DT)) {
    if (grepl("\\.old$", col)) {
      DT[, (col) := NULL]
    }
  } # for
  return(DT)
} # function

drop_cols.clean <- function(DT) {
  cols.drop <- c(
    "max_xp", "in_garage_updated", "max_frags",
    "frags", "mark_of_mastery", "in_garage",
    "all.max_xp", "all.frags8p"
  )
  return(drop_cols(DT, cols.drop))
}


drop_cols.slim <- function(DT) {
  drop.cols.diff <- c("all.max_frags", "all.frags8p", "all.xp")
  drop.cols.oldnew <- c("all.capture_points", "all.dropped_capture_points")
  return(drop_cols(DT, c(
    drop.cols.diff,
    paste0(drop.cols.diff, ".new"),
    paste0(drop.cols.diff, ".old"),
    paste0(drop.cols.oldnew, ".new"),
    paste0(drop.cols.oldnew, ".old")
  )))
}


## Merge stats with Tankopedia
merge_tankopedia <- function(DT, tanks = tanks) {
  stopifnot(exists("tanks"))
  setkey(DT, tank_id)
  for (col in colnames(tanks)) {
    if (col == "tank_id") next
    if (col %in% colnames(DT)) DT[, (col) := NULL]
  } # for
  DT <- merge(DT, tanks)
  setkey(DT, account_id, tank_id)
  return(DT)
}


calc_diff_tank_stats <- function(stats.start, stats.period, career = FALSE, drop.old = TRUE) {
  stopifnot(all(is.data.table(stats.start), is.data.table(stats.period)))
  setkey(stats.start, account_id, tank_id)
  setkey(stats.period, account_id, tank_id)

  stats.period <- drop_cols.tankopedia(stats.period)
  stats.period <- drop_cols.clean(stats.period)
  stats.start <- drop_cols.clean(stats.start)

  # remove players who have not both in stats.start and stats.period
  account_ids.period <- stats.period[, unique(account_id)]
  account_ids.start <- stats.start[, unique(account_id)]
  stats.period <- subset(stats.period, account_id %in% account_ids.start)
  stats.start <- subset(stats.start, account_id %in% account_ids.period)

  if (career) {
    stats.diff <- merge(stats.start, stats.period, all = TRUE, suffixes = c(".old", ".new"))
  } else {
    stats.diff <- merge(stats.start, stats.period, all.y = TRUE, suffixes = c(".old", ".new"))
  }

  stats.diff[, c("region.old", "last_battle_time.old") := NULL]
  setnames(stats.diff, c("region.new", "last_battle_time.new"), c("region", "last_battle_time"))

  # add zero stats to players who have bought the tank during the update
  # (i.e. players have been captured in cum stats, but not the particular tanks)

  for (n in names(stats.diff)) {
    if (grepl("\\.old$", n) || grepl("\\.new$", n)) {
      stats.diff[is.na(get(n)), (n) := 0]
    }
  }

  ## Remove players with only new tanks.
  # This happens when stats are being regenerated after new players have been fetched

  account_ids.old_tanks <- as.character(stats.diff[
    (battles.old > 0) &
      (battles.new > battles.old),
    unique(account_id)
  ])
  account_ids.new_tanks <- as.character(stats.diff[
    battles.old == 0,
    unique(account_id)
  ])

  accounts_ids.only_new <- as.integer64(base::setdiff(account_ids.new_tanks, account_ids.old_tanks))
  del.idx <- stats.diff[, .I[account_id %in% accounts_ids.only_new]]
  stats.diff <- delete(stats.diff, del.idx)
  # 2022-08-26 use subset() instead of delete...
  # stats.diff <- subset(stats.diff, account_id %in% account_ids.old_tanks)

  del.idx <- rep(FALSE, nrow(stats.diff))
  ## Loop through .old cols and calculate difference use: assign()
  for (col in c(
    "battles", "all.capture_points", "all.damage_dealt", "all.damage_received",
    "all.dropped_capture_points", "all.frags", "all.hits", "all.losses",
    "all.shots", "all.spotted", "all.survived_battles", "all.win_and_survived",
    "all.wins", "battle_life_time", "all.xp"
  )) {
    col.new <- paste0(col, ".new")
    col.old <- paste0(col, ".old")
    if (col.new %nin% colnames(stats.diff)) {
      next
    }
    if (career) {
      stats.diff[get(col.new) == 0, (col.new) := get(col.old)]
    }

    stats.diff[, (col) := get(col.new) - get(col.old)]
    del.idx <- del.idx | stats.diff[, get(col) < 0] ## clean out errors
  } # for

  del.idx <- stats.diff[, .I[del.idx]]
  stats.diff <- delete(stats.diff, del.idx)

  # memory optimization
  if (drop.old) {
    stats.diff <- drop_cols.old(stats.diff)
  }

  if (!career) {
    # remove stats with zero battles
    del.ndx <- stats.diff[, .I[(battles < 1)]]
    stats.diff <- delete(stats.diff, del.ndx)
  }

  return(stats.diff)
} # function


## get_tank_funcs()  #####################


get_tier_roman <- function(tier) {
  stopifnot(exists("tiers.roman"))
  return(tiers.roman[as.integer(tier)])
}


get_tank_name <- function(id = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(id))
  return(as.character(tanks[tank_id %in% id][match(id, tank_id), name]))
}


get_tank_id <- function(tank_name = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(tank_name))
  return(tanks[name %in% tank_name][match(tank_name, name), tank_id])
}


get_tank_type <- function(id = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(id))
  return(as.factor(tanks[tank_id %in% id][match(id, tank_id), type]))
}


get_tank_nation <- function(id = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(id))
  return(as.factor(tanks[tank_id %in% id][match(id, tank_id), nation]))
}


get_tank_tier <- function(id = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(id))
  return(tanks[tank_id %in% id][match(id, tank_id), tier])
}


get_tank_is_premium <- function(id = NULL) {
  stopifnot(exists("tanks"))
  stopifnot(!is.null(id))
  return(tanks[tank_id %in% id][match(id, tank_id), is_premium])
}


# is_too_rare <- function(DT, tank_ID) {
# 	stopifnot(exists('min_players.tank'))
# 	return(DT[tank_id == tank_ID, .N] < min_players.tank)
# }


has_enough_players <- function(DT, tank_ID, min_players = min_players.tank) {
  return(DT[tank_id == tank_ID, .N] >= min_players.tank)
}


get_tanks_enough_players <- function(DT, min_players = min_players.tank) {
  stopifnot(exists("min_players.tank"))

  tanks.enough_players <- DT[, .N, by = tank_id][N >= min_players, tank_id]
  if (length(tanks.enough_players) > 0) {
    return(tanks.enough_players)
  } else {
    return(NULL)
  }
}


get_tank_list <- function(stats = NULL, tank_nation = NA, tank_type = NA, tank_tier = NA, premium = NA) {
  if (is.null(stats)) {
    stopifnot(exists("tanks"))
    tanks2list <- tanks[(is.na(tank_nation) | (nation == tank_nation)) &
      (is.na(tank_type) | (type == tank_type)) &
      (is.na(tank_tier) | (tier == tank_tier)) &
      (is.na(premium) | (is_premium == premium))][order(tier, nation, type, name)]
  } else {
    tanks2list <- unique(stats[
      (is.na(tank_nation) | (nation == tank_nation)) &
        (is.na(tank_type) | (type == tank_type)) &
        (is.na(tank_tier) | (tier == tank_tier)) &
        (is.na(premium) | (is_premium == premium)),
      .(tier = tier, nation = nation, type = type, is_premium = is_premium, name = name),
      by = tank_id
    ])[order(tier, nation, type, name)]
  }
  setcolorder(tanks2list, c("tier", "nation", "type", "is_premium", "name", "tank_id"))

  if (!is.na(tank_nation)) tanks2list[, nation := NULL]
  if (!is.na(tank_type)) tanks2list[, type := NULL]
  if (!is.na(tank_tier)) tanks2list[, tier := NULL]
  if (!is.na(premium)) tanks2list[, is_premium := NULL]


  setnames(tanks2list,
    c("tier", "nation", "type", "is_premium", "name"),
    c("Tier", "Nation", "Vehicle Class", "Premium", "Tank"),
    skip_absent = TRUE
  )

  return(copy(tanks2list))
}


## Return mid-points for
get_mid_points <- function(x) {
  mid_pts <- c()
  for (i in 1:(length(x) - 1)) {
    mid_pts[i] <- mean(c(x[i], x[i + 1]))
  }
  return(mid_pts)
} # function


get_Nations <- function(x) {
  x <- sort(x)
  Nats <- c()
  for (nat in x) {
    Nat <- nations_db[nation == nat, Nation]
    Nats <- append(Nats, Nat)
  }
  return(Nats)
} # function


get_grind_len <- function(tier) {
  stopifnot(exists("min_battles.grinds"))
  return(min_battles.grinds[tier][[1]])
}


get_min_battles_tier <- function(tier) {
  stopifnot(exists("min_battles.tiers"))
  return(min_battles.tiers[tier][[1]])
}


get_data_dir <- function() {
  if (exists("data_dir")) {
    return(data_dir)
  } else {
    stopifnot(exists("base_dir"))
    return(file.path(base_dir, "..", "blitz-data"))
  }
}


## filename funcs ####################################


## TODO:
## * Update filename funcs to: filename_tank_stats.summary(), filename_tanks_stats.update(), filename_tanks_stats.career()
## * Rename stats files?

## Filename: QS format
filename_career <- function(update) {
  return(filename_data(paste0("tank_stats_career_", update, ".qs")))
}


filename_cumulative_after <- function(update) {
  return(filename_data(paste0("tank_stats_cumulative_after_", update, ".qs")))
}


filename_cumulative_before <- function(update) {
  return(filename_data(paste0("tank_stats_cumulative_before_", update, ".qs")))
}


filename_period <- function(update.start, update.end) {
  return(filename_data(paste0(
    "tank_stats_period_", update.start, "-",
    update.end, ".qs"
  )))
}


filename_period_totals <- function(update.start, update.end) {
  return(filename_data(paste0(
    "tank_stats_period_totals_", update.start, "-",
    update.end, ".qs"
  )))
}

## Diff Stats for an update
filename_update <- function(update) {
  return(filename_data(paste0("tank_stats_update_", update, ".qs")))
}


## Raw stats for an update
filename_update_totals <- function(update.end) {
  return(filename_data(paste0("tank_stats_update_total_", update.end, ".qs")))
}


## Diff Stats for an update
filename_tankopedia <- function(update.ver) {
  stopifnot(exists(("update.ver")))
  # return(filename_data(paste0("tankopedia_", update.ver, ".qs")));
  return(filename_data(file.path(update.ver, "tankopedia.qs")))
}


## Summary tank stats for an update
filename_tank_summary_stats <- function(update) {
  stopifnot(exists(("update")))
  return(filename_data(paste0("tank_stats_summary_", update, ".qs")))
}


## Get filename for a data to be saved on data dir
filename_data <- function(fn = NULL) {
  stopifnot(!is.null(fn))
  return(file.path(get_data_dir(), fn))
}


filename_part <- function(filename, ndx) {
  return(paste0(filename, ".", ndx))
}


get_blitz_updates <- function() {
  # get Blitz updates
  # updates <- as.data.table(WG_releases$find(sort = '{"Date":1}'));
  updates <- as.data.table(Releases$find(fields = "{}", sort = '{"cut_off":1}'))
  colnames(updates) <- c("Release", "Launch_date", "Cut-off")
  # updates[, Release := gsub('v', '', Release)]
  return(updates)
}


update_has_stats <- function(update) {
  # get Blitz updates
  stopifnot(exists("updates"))
  return(as.logical(updates[(Release == update), `Cut-off` > 0]))
}


get_cut_off_date <- function(update) {
  stopifnot(exists("updates"))
  cut_off <- as.integer(updates[Release == update, "Cut-off"])
  return(cut_off[[1]][1])
}


get_update_start <- function(update) {
  prev_update <- get_prev_update(update)
  return(get_cut_off_date(prev_update))
}


get_prev_update <- function(update) {
  stopifnot(exists("updates"))
  row <- updates[Release == update, which = TRUE]
  prev_update <- updates[row - 1, "Release"][[1]][1]
  if (is.na(prev_update)) {
    return(NULL)
  } else {
    return(prev_update)
  }
}

MAX_EPOCH <- 6e10

get_next_update <- function(update, preliminary = FALSE) {
  stopifnot(exists("updates"))
  row <- updates[Release == update, which = TRUE]
  if (is.na(updates[row + 1, `Cut-off`])) {
    return(NULL)
  }
  if ((updates[row + 1, `Cut-off`] < MAX_EPOCH) || (preliminary && updates[row, `Cut-off`] < MAX_EPOCH)) {
    return(updates[row + 1, "Release"][[1]][1])
  } else {
    return(NULL)
  }
}


get_latest_update <- function(preliminary = FALSE) {
  stopifnot(exists("updates"))
  latest_row <- tail(updates[`Cut-off` < MAX_EPOCH, which = TRUE], n = 1)
  if (preliminary) {
    latest_row <- latest_row + 1
  }
  return(as.character(updates[latest_row, "Release"][[1]][1]))
}


# get_updates_since <- function(update = '6.0', preliminary = FALSE) {
#   stopifnot(exists('updates'));
#   updates_ <- c()
#   while (! is.null(update)) {

#     updates_ <- append(updates_, update)
#     update <- get_next_update(update, preliminary);
#   }
#   return(updates_);
# }


get_updates <- function(since = "6.0", until = get_latest_update(), preliminary = FALSE) {
  stopifnot(exists("updates"))
  updates_ <- c()
  update <- since
  while (!is.null(update)) {
    updates_ <- append(updates_, update)
    if (update == until) {
      break
    }
    update <- get_next_update(update, preliminary)
  }
  return(updates_)
}


is_preliminary <- function(update = "None") {
  stopifnot(exists("updates"))
  if (updates[, update %in% Release] &&
    (updates[Release == update, `Cut-off`] > MAX_EPOCH)) {
    return(TRUE)
  }
  return(FALSE)
}


## Stats fetching & generation #################################################


get_tankopedia <- function() {
  tanks <- as.data.table(jsonlite::flatten(Tankopedia$find("{}", fields = "{}")), encoding = "UTF-8")
  colnames(tanks) <- c("tank_id", "name", "code", "nation", "type", "tier", "is_premium", "next_tanks")
  tanks$code <- NULL
  setkey(tanks, tank_id)
  tanks[, nation := factor(nation, ordered = TRUE)]
  levels(tanks$nation) <- Nations
  tanks[, type := factor(type, ordered = TRUE)]
  levels(tanks$type) <- Tank_Types
  tanks[, name := as.factor(name)]
  tanks[, tier := as.integer(tier)]
  tanks[is.na(is_premium), is_premium := FALSE]
  tanks[, next_tanks := NULL]

  return(tanks)
}

## REFACTOR: This could get refactored using cursor and loop. Add status monitoring
get_accounts <- function() {
  find_str <- '{ "$and" : [ { "_id": { "$lt": 31e8 } },{ "$or": [ { "inactive": false }, { "inactive": { "$exists": false } } ] }, { "invalid": { "$exists": false} } ] }'
  project_str <- '{ "_id": 1 }'
  accounts <- as.data.table(jsonlite::flatten(WG_Accounts$find(find_str, project_str)), encoding = "UTF-8")
  colnames(accounts) <- c("account_id")
  return(accounts)
}


get_account_nicknames <- function(account_ids) {
  ## from WG API
  tmp <- list()
  for (region in names(regions)) {
    tmp[[region]] <- NULL
  }
  nicknames <- list()

  for (account_id in paste(account_ids)) { ## R is 32-BIT in its core..."#¤%"#%#"¤%
    region <- get_region(account_id)
    tmp[[region]] <- c(tmp[[region]], account_id)

    if (length(tmp[[region]]) == 100) {
      ## FIX different queues for different servers
      accounts <- get_wg_api_account_info(region, tmp[[region]])
      nicknames[names(accounts)] <- as.vector(accounts)
      tmp[[region]] <- NULL
    }
  }
  for (region in names(tmp)) {
    if (length(tmp[[region]]) > 0) {
      accounts <- get_wg_api_account_info(region, tmp[[region]])
      nicknames[names(accounts)] <- as.vector(accounts)
    }
  } # for
  return(nicknames)
}

json_parse_nicknames <- function(accounts.json) {
  if ("data" %in% names(accounts.json)) {
    res <- list()
    for (account_id.str in names(accounts.json[["data"]])) {
      res[[account_id.str]] <- accounts.json[["data"]][[account_id.str]]$nickname
    }
    return(res)
  } else {
    return(NULL)
  }
}


DT_merge_nicknames <- function(DT, nicknames) {
  nicks <- as.data.table(list("account_id" = as.integer64(names(nicknames)), "nickname" = as.vector(nicknames)))
  setkey(DT, "account_id")
  setkey(nicks, "account_id")
  DT <- merge(DT, nicks, by = "account_id")
  return(DT)
}

get_url_json <- function(url, max_tries = 3) {
  for (try in seq_len(max_tries)) {
    resp <- GET(url)
    if (http_status(resp)$category == "Success") {
      return(jsonlite::fromJSON(content(resp, "text"), flatten = TRUE))
    }
  } # for
  return(NULL)
}


get_url_wg_account_info <- function(region, account_ids) {
  stopifnot(exists("WG_AppID"))
  stopifnot(exists("WG_API_base"))
  base <- paste0("/wotb/account/info/?application_id=", WG_AppID, "&fields=nickname&account_id=")
  url <- paste0(WG_API_base, region, base, paste(account_ids, collapse = "%2C"))
  return(url)
}


get_wg_api_account_info <- function(region, account_ids) {
  url <- get_url_wg_account_info(region, account_ids)
  accounts.json <- get_url_json(url)
  return(json_parse_nicknames(accounts.json))
}


get_region <- function(account_id) {
  account_id <- as.integer64(account_id)
  if (account_id >= 1e9) {
    if (account_id >= 31e8) {
      ## China server, no stats available
      return(NULL)
    } else if (account_id >= 2e9) {
      return("asia")
    } else {
      return("com")
    }
  } else {
    if (account_id < 5e8) {
      return("ru")
    } else {
      return("eu")
    }
  }
}


get_WG_API_server <- function(region) {
  base <- "https://api.wotblitz."
  if (is.null(region)) {
    return(NULL)
  } else {
    return(paste0(base, region))
  }
}


## Return tank summary stats
get_tank_summary_stats <- function(update_list = get_updates()) {
  if (is.null(update_list)) {
    update_list <- get_blitz_updates()$Release
  }
  res <- list()
  for (curr_update in update_list) {
    fn <- filename_tank_summary_stats(curr_update)
    if (!stats.exists(fn)) {
      stop(cat(paste("update", curr_update, "file not found: ", fn)))
    }
    cat(paste("Update", curr_update, "reading data"))
    res[[curr_update]] <- qread(fn)
  } ## for
  res <- rbindlist(res, use.names = TRUE, fill = TRUE)
  setorder(res, -last_battle_time)
  return(res)
}


fetch_tank_stats_update_total <- function(update = get_latest_update(), force = FALSE, preliminary = FALSE) {
  # if 'update' is not given gets all the updates' stats
  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

  prev_update <- get_prev_update(update)
  date_min <- get_cut_off_date(prev_update)
  date_max <- get_cut_off_date(update)

  if ((date_max == 0) && preliminary) {
    date_max <- now()
  }
  if ((date_min == 0) || (date_max == 0)) {
    cat(paste("No cut-off date defined for update ", update, "skipping...\n"))
    return(FALSE)
  }
  filename <- filename_update_totals(update)
  if ((!force) && stats.exists(filename)) {
    cat(paste("Release", update, ": stats exist\n"))
    return(FALSE)
  }

  tic(paste("Update", update))

  pipeline.template <- paste0('[{ "$match" : { "$and" : [ { "account_id" : _ACCOUNT_ID_ },
                                  {"last_battle_time" : { "$lte": ', date_max, ' } },
                                  {"last_battle_time" : { "$gt": ', date_min, ' } } ] }},
                                { "$sort": { "last_battle_time" : -1}},
                                { "$group" : { "_id" :  "$tank_id", "doc": { "$first": "$$ROOT" }} },
                                { "$replaceWith":  "$doc" },
                                { "$project" : { "_id": 0} } ]')

  progress_txt <- paste("Fetching stats for Update", update)

  ## Fetch & save stats
  fetch_tank_stats_helper(pipeline.template, filename, progress_txt)

  toc_() # update
}


fetch_tank_stats_period_totals <- function(update.start, update.end, force = FALSE, preliminary = FALSE) {
  # if 'update' is not given gets all the updates' stats
  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

  prev_update <- get_prev_update(update.start)
  date_min <- get_cut_off_date(prev_update)
  date_max <- get_cut_off_date(update.end)

  if ((date_max == 0) && preliminary) {
    date_max <- now()
  }
  if ((date_min == 0) || (date_max == 0)) {
    cat(paste("No cut-off date defined for update ", update.tmp, "skipping...\n"))
    return(NULL)
  }
  filename <- filename_period_totals(update.start, update.end)
  if ((!force) && stats.exists(filename)) {
    cat(paste("Stats for period", update.start, "-", update.end, "exist\n"))
    return(NULL)
  }

  tic("Total time")

  pipeline.template <- paste0('[{ "$match" : { "$and" : [ { "account_id" : _ACCOUNT_ID_ },
                                               {"last_battle_time" : { "$lte": ', date_max, ' } },
                                               {"last_battle_time" : { "$gt": ', date_min, ' } } ] }},
                              { "$sort": { "last_battle_time" : -1}},
                              { "$group" : { "_id" :  "$tank_id", "doc": { "$first": "$$ROOT" }} },
                              { "$replaceWith":  "$doc" },
                              { "$project" : { "_id": 0} } ]')

  progress_txt <- paste("Fetching stats for period", update.start, "-", update.end)

  ## Fetch & save stats
  fetch_tank_stats_helper(pipeline.template, filename, progress_txt)

  toc_()
}

fetch_update_stats <- function(update, force = TRUE, preliminary = FALSE) {
  tic(paste("Fetching stats for update ", update))

  # fetch update totals
  fetch_tank_stats_update_total(update = update.latest, preliminary = preliminary, force = TRUE)

  gc(verbose = FALSE)

  # fetch career stats
  fetch_tank_stats_career(update = update.latest, start_stats = TRUE, preliminary = preliminary, active_only = TRUE, force = TRUE)

  gc(verbose = FALSE)

  # generate update stats
  gen_tank_stats_update(update = update.latest, force = TRUE)

  gc(verbose = FALSE)
  # gen_careers stats (REQUIRED)
  gen_tank_stats_career(update.latest)

  gc(verbose = FALSE)
  # generate summary stats
  fetch_tank_summary_stats(update.latest)

  # Save Tankopedia
  save_tankopedia(tanks, update.latest)

  filename_tankopedia()
}


fetch_tank_stats_career <- function(update = get_latest_update(), force = FALSE, preliminary = FALSE,
                                    active_only = FALSE, start_stats = FALSE, N_splits = NULL, split_I = NULL) {
  ## Get cumulative tank stats. V2 fetching accounts user by user.

  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))


  if (start_stats) {
    date_max <- get_cut_off_date(get_prev_update(update))
    filename <- filename_cumulative_before(update)
    tic(paste("Fetching career tank stats before update", update))
    msg.str <- paste("Fetching career tanks stats before Update", update)
  } else {
    date_max <- get_cut_off_date(update)
    filename <- filename_cumulative_after(update)
    tic(paste("Fetching career tank stats for update", update))
    msg.str <- paste("Fetching career tanks stats for Update", update)
  }

  if ((date_max == 0) && preliminary) {
    date_max <- now()
  }

  if (stats.exists(filename, split_I)) {
    cat(paste("Release", update, ": career stats exist", "\n"))
    if (!force) {
      return(invisible(NULL))
    } else {
      message("Update forced")
    }
  } # if



  pipeline.template <- paste0('[{ "$match" : { "$and" : [ { "account_id" : _ACCOUNT_ID_ },
                            {"last_battle_time" : { "$lte": ', date_max, ' } } ] }},
                { "$sort": { "last_battle_time" : -1}},
                { "$group" : { "_id" :  "$tank_id", "doc": { "$first": "$$ROOT" }} },
                { "$replaceWith":  "$doc" },
                { "$project" : { "_id": 0} } ]')
  progress_txt <- paste("Fetching career stats: Update", update)

  ## Print status message
  msg.str <- paste("Fetching career tanks stats: Update", update)
  if (!is.null(split_I)) {
    msg.str <- paste0(msg.str, " (", split_I, "/", N_splits, ")\n")
  } else {
    msg.str <- paste0(msg.str, "\n")
  }

  if (active_only) {
    stats.tmp <- load_tank_stats_update_totals(update)
    accounts <- stats.tmp[, .(account_id = unique(account_id))]
  } else {
    accounts <- NULL
  }
  cat(msg.str)

  fetch_tank_stats_helper(pipeline.template, filename, progress_txt,
    accounts = accounts, N_splits = N_splits, split_I = split_I
  )
  toc_()
} # function


## Helper function to fetch stats iterating over accounts.
## place '_ACCOUNT_ID_' in place of the account_id variable.


fetch_tank_stats_helper <- function(pipeline.template, filename, progress_txt,
                                    accounts = NULL, N_splits = NULL, split_I = NULL,
                                    rows.lim.part = row.lim.split, nticks = 100) {
  if (is.null(accounts)) {
    cat("Fetching account_id list from DB...\n")
    accounts <- get_accounts()
  }

  ## Progress bar
  frmt <- paste(progress_txt, ": [:bar] :current/:total (:percent) ETA: :eta")
  pb <- progress_bar$new(total = nrow(accounts), format = frmt, show_after = 0)
  pb$tick(0)

  if (is.null(N_splits) || is.null(split_I)) {
    auto_split <- TRUE
  } else {
    auto_split <- FALSE
  }

  ## FIX partial fetch
  rows.count <- 0
  filepart.ndx <- 0
  i <- 0

  dat <- linkedList()

  for (account_id in paste(accounts$account_id)) { # R is 32-bit in its core ¤#%¤"#%¤"#
    i <- i + 1
    if (i %% nticks == 0) {
      pb$tick(nticks)
    }

    if (!auto_split) {
      account_id.int <- as.integer64(account_id)
      if (account_id.int %% N_splits != split_I) {
        next
      }
    } ## if (! auto_split)

    pipeline.account_id <- gsub("_ACCOUNT_ID_", account_id, pipeline.template)

    dat$add(as.data.table(jsonlite::flatten(WG_tank_stats$aggregate(
      pipeline = pipeline.account_id,
      options = '{"allowDiskUse":true}', pagesize = 10000
    ))))
    if (auto_split) {
      rows.count <- rows.count + nrow(dat$last())
      if ((rows.count %/% rows.lim.part > filepart.ndx) && ((nrow(accounts) - i) * (filepart.ndx + 1) / i > 0.1)) {
        # save filepart
        save_tank_stats(dat, filename, filepart.ndx, split = "no", clean = TRUE)
        filepart.ndx <- filepart.ndx + 1
        dat <- linkedList()
      }
    }
  } # for account_id

  ## No need to remove the progress bar
  # pb$terminate()
  cat("\n")

  if (filepart.ndx == 0) {
    filepart.ndx <- NULL
  }
  if (!auto_split) {
    filepart.ndx <- split_I
  }
  save_tank_stats(dat, filename, filepart.ndx, split = "no", clean = TRUE)
}


## Fetch summary stats from all the data during an update
fetch_tank_summary_stats <- function(update_list = get_updates(),
                                     force = FALSE, fields = c("battles", "wins")) {
  stopifnot(exists("WG_tank_stats"))
  tic("Total processing time")
  res <- list()

  field_str <- paste0('{"_id" : 0, "account_id": 1, "tank_id": 1, "last_battle_time": 1')
  if (!is.null(fields)) {
    for (field in fields) {
      field_str <- paste0(field_str, ',"all.', field, '":1')
    }
  }
  field_str <- paste0(field_str, "}")

  if (is.null(update_list)) {
    update_list <- get_blitz_updates()$Release
  }
  # if (length(update_list) > 0) {
  #   cat(paste('Processing stats. Update ', update_list[[1]]), fill= TRUE)
  # }

  for (curr_update in update_list) {
    if (stats.exists(filename_tank_summary_stats(curr_update)) && (!force)) {
      cat(paste("Update ", curr_update, "summary tank stats exits. Skipping..."), fill = TRUE)
      next
    }

    tic(paste("Update ", curr_update))
    prev_update <- get_prev_update(curr_update)
    if (is.null(prev_update)) {
      time_start <- 0
    } else {
      time_start <- get_cut_off_date(prev_update)
    }

    time_end <- get_cut_off_date(curr_update)
    if (time_end == 0) {
      time_end <- now()
    }

    #
    #   find_str <- paste0('{ "$and" : [ {"last_battle_time" : { "$lte": ',time_end , ' } },
    # 														{"last_battle_time" : { "$gt" :', time_start, ' }}]}')
    #

    frmt <- paste("Fetching stats Update", curr_update, ": [:bar] :current/:total (:percent) ETA: :eta")
    pb <- progress_bar$new(total = nrow(tanks), format = frmt)
    pb$tick(0)
    dat.summary_stats <- list()
    for (tank_id in tanks$tank_id) {
      find_str <- paste0('{ "$and" : [ { "tank_id" : ', tank_id, ' },
                            {"last_battle_time" : { "$lte": ', time_end, ' } },
                            {"last_battle_time" : { "$gt" :', time_start, " }}]}")
      name <- paste0("t", tank_id)
      dat.summary_stats[[name]] <- as.data.table(jsonlite::flatten(WG_tank_stats$find(find_str, fields = field_str)), encoding = "UTF-8")
      pb$tick()
    } # for tank_id
    pb$terminate()
    summary_stats <- rbindlist(dat.summary_stats, use.names = TRUE, fill = TRUE)

    # stats <- as.data.table(jsonlite::flatten(WG_tank_stats$find(find_str, fields = field_str)), encoding="UTF-8")
    qsave(summary_stats, filename_tank_summary_stats(curr_update))
    rm(summary_stats)

    toc_()
  }
  toc_()
}


save_tankopedia <- function(tanks = tanks, update = get_latest_update()) {
  fn <- filename_tankopedia(update)
  qsave(tanks, fn)
}


## Helper func
save_tank_stats <- function(stats, filename, ndx = NULL, split = "keep", clean = FALSE) {
  ## splits = [ 'auto', 'no', 'keep']
  stopifnot(!is.null(filename))
  if (!is.null(ndx)) {
    filename <- paste0(filename, ".", ndx)
  }
  if (is(stats, "list") && ("as.list" %in% names(stats))) {
    stats <- stats$as.list()
  }

  if (split == "auto") {
    N <- get_DT_splits(stats)
    if (!(is(stats, "list") && (N == length(stats)))) {
      stats <- DT_split(stats, N)
    }
  } else if (split == "no") {
    if (is(stats, "list")) {
      stats <- rbindlist(stats, use.names = TRUE, fill = TRUE)
    }
  }

  ## clean tank stats
  if (clean) {
    stats <- run_parts(stats, function(DT) {
      return(clean_tank_stats(DT))
    })
  }

  if (is(stats, "list")) {
    if (length(stats) == 1) {
      qsave(stats[[1]], filename, nthreads = n_threads)
    } else {
      for (i in seq_len(length(stats))) {
        save_tank_stats(stats[[1]], filename, ndx = i - 1, clean = FALSE)
        stats[[1]] <- NULL
      } # for
    }
  } else {
    ## not list
    qsave(stats, filename, nthreads = n_threads)
  }
}


# gen_tank_stats_cum <- function(update) {
#   stats.cum <- load_tank_stats_cum_src(update, only_active = FALSE,
#                                    do_rbind = FALSE)
#   stats.cum <- run_parts(stats.cum, function(DT) { DT <- calc_tank_stats_cum(DT);
#                                                   DT <- merge_tankopedia(DT);})
#   filename <- filename_cumulative(update)
#   save_tank_stats(stats.cum, filename, clean = FALSE, split = 'keep')
# }


gen_tank_stats_career <- function(update, force = FALSE) {
  tic("gen_tank_stats_career(): ")

  fn_cum_after <- filename_career(update)

  stats.before <- load_tank_stats_cum_before(update, do_rbind = FALSE)
  stats.update_totals <- load_tank_stats_update_totals(update)

  message("Merging career stats: Update ", update)
  stats.after <- run_parts(stats.before,
    func = function(DT, DT.update) {
      accounts.tmp <- DT[, unique(account_id)]
      DT.tmp <- subset(DT.update, account_id %in% accounts.tmp)
      return(merge_stats(DT, DT.tmp))
    },
    stats.update_totals
  )

  rm(stats.before)
  rm(stats.update_totals)
  tanks.update <- load_tankopedia(update)
  message("Processing career stats: Update ", update)
  stats.after <- run_parts(stats.after, func = function(DT) {
    DT <- calc_tank_stats_cum(DT)
    DT <- merge_tankopedia(DT, tanks.update)
    return(DT)
  })
  message("Saving career stats: Update ", update)
  save_tank_stats(stats.after, fn_cum_after)
  # process_parts(fn_cum_src, fn_cum, force, function(DT) { DT <- calc_tank_stats_cum(DT);
  #                         DT <- merge_tankopedia(DT);})
  toc_()
}


##  Piecewise stats generation to deal with mem consumption
gen_tank_stats_summary <- function(update = get_latest_update(),
                                   force = FALSE, fields = c("battles", "wins")) {
  # generate update (delta) stats
  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

  tic(paste("Generating summary tank stats: Update", update))
  print(paste("Generating summary tank stats: Update", update))

  filename_stats.update_totals <- filename_update_totals(update)
  filename_stats.update_summary <- filename_tank_summary_stats(update)

  stopifnot(stats.exists(filename_stats.update_totals))
  if (stats.exists(filename_stats.update_summary) && (!force)) {
    print(paste("Update", update, ": Summary tank stats exists. Skipping..."))
    return()
  }

  fields.keep <- c("account_id", "tank_id", "last_battle_time")
  for (field in fields) {
    fields.keep <- append(fields.keep, paste0("all.", field))
  }

  print(paste("Update", update, ": Reading update totals"))
  stats <- load_stats(filename_stats.update_totals)

  cols <- names(stats)
  for (i in seq(length(cols), 1)) {
    if (!cols[i] %in% fields.keep) {
      stats[, (i) := NULL]
    }
  }
  qsave(stats, filename_stats.update_summary, nthreads = n_threads)

  toc_() # whole run
} # function

##  generate cumulative tank stats in piece-wise manner to lower RAM usage


build_tank_stats_cum <- function(base_update = NULL, target_update = get_latest_update(),
                                 force = FALSE, preliminary = FALSE) {
  # generate cumulative stats

  stopifnot(FALSE)

  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))
  if (exists("stats.cum")) {
    rm(stats.cum)
  }
  if (is.null(base_update)) {
    base_update <- get_prev_update(target_update)
  }
  curr_update <- base_update
  next_update <- get_next_update(curr_update, preliminary)

  tic("Calculating cumulative stats")
  while (!is.null(next_update)) {
    print(paste("Generating cumulative stats for update", next_update))
    filename_stats.cum <- filename_career(curr_update)
    filename_stats.update_totals <- filename_update_totals(next_update)
    stopifnot(stats.exists(filename_stats.cum), stats.exists(filename_stats.update_totals))

    tic(paste("Generating cumulative stats for update", next_update))
    filename_stats.cum.new <- filename_career(next_update)

    if ((force) || (!stats.exists(filename_stats.cum.new))) {
      if (!exists("stats.cum")) {
        print(paste("Update", curr_update, ": Reading cumulative stats"))
        stats.cum <- load_stats(filename_stats.cum)
      }
      split_N <- get_DT_splits(stats.cum)
      stats.cum <- DT_split(stats.cum, split_N)

      print(paste("Update", next_update, ": Reading update totals"))
      stats.update_totals <- qread(filename_stats.update_totals)
      stats.update_totals <- DT_split(stats.update_totals, split_N)

      for (i in seq(length(stats.cum), 1)) {
        stats.cum[[i]] <- rbindlist(list(stats.cum[[i]], stats.update_totals[[i]]), use.names = TRUE, fill = TRUE)
        stats.update_totals[[i]] <- NULL
        setorder(stats.cum[[i]], -last_battle_time)
        # Select the latest stats only
        stats.cum[[i]] <- stats.cum[[i]][, head(.SD, 1), by = .(account_id, tank_id)]
        stats.cum[[i]] <- calc_tank_stats_cum(stats.cum[[i]])
        stats.cum[[i]] <- merge_tankopedia(stats.cum[[i]])
      } # for

      stats.cum <- DTL_rebind(stats.cum)
      print(paste("Saving cumulative stats for update", next_update))
      qsave(stats.cum, filename_stats.cum.new, nthreads = n_threads)
    } else {
      print(paste("Release", next_update, ": cumulative stats exist. Skipping..."))
      if (exists("stats.cum")) {
        rm(stats.cum)
      }
    }

    if (next_update == target_update) {
      toc_()
      if (exists("stats.cum")) {
        rm(stats.cum)
      }
      break ## while
    } else {
      curr_update <- next_update
      next_update <- get_next_update(curr_update, preliminary)
      toc_()
      next
    }
  } ## while
  toc_()
} # function


##  Piecewise stats generation to deal with mem consumption
gen_tank_stats_update <- function(update = get_latest_update(),
                                  force = FALSE) {
  # generate update (delta) stats
  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

  tic(paste("Generating update stats for update", update))
  print(paste("Generating update stats for update", update))

  filename_stats.cum <- filename_cumulative_before(update)
  filename_stats.update_totals <- filename_update_totals(update)
  filename_stats.update <- filename_update(update)

  stopifnot(stats.exists(filename_stats.cum), stats.exists(filename_stats.update_totals))

  if (stats.exists(filename_stats.update) && (!force)) {
    print(paste("Update", update, ": Update stats exists. Skipping..."))
    return()
  }

  print(paste("Update", update, ": Reading update totals"))
  stats.update_totals <- load_stats(filename_stats.update_totals)
  account_ids.update <- stats.update_totals[, unique(account_id)]
  tanks.update <- load_tankopedia(update)
  process_parts(filename_stats.cum, filename_stats.update, force, func = function(DT) {
    DT <- subset(DT, account_id %in% account_ids.update)
    accounts_ids.old <- DT[, unique(account_id)]
    stats.update_totals.tmp <- subset(stats.update_totals, account_id %in% accounts_ids.old)
    DT <- calc_diff_tank_stats(DT, stats.update_totals.tmp, career = TRUE)
    stats.update_totals.tmp <- NULL
    DT <- merge_tankopedia(DT, tanks.update) ##
    DT <- calc_tank_stats(DT) ##
    DT <- subset(DT, battles > 0) ##  since career = TRUE in calc_diff_tank_stats()
    return(copy(DT))
  })
  toc_() # whole run
} # function


# gen_tank_stats_update.old <- function(base_update = NULL, update = get_latest_update(),
#                                       force = FALSE, preliminary = FALSE) {
#     # generate update (delta) stats
#     stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

#     if (is.null(base_update)) {
#         base_update <- get_prev_update(update)
#     }

#     curr_update <- base_update
#     next_update <- get_next_update(curr_update, preliminary)

#     tic("Generating update stats from totals")
#     while (!is.null(next_update)) {
#         print(paste("Generating update stats for update", next_update))
#         tic(paste("Update", next_update, ": Generating cumulative stats"))

#         filename_stats.cum <- filename_career(curr_update)
#         filename_stats.update_totals <- filename_update_totals(next_update)
#         filename_stats.update <- filename_update(next_update)
#         stopifnot(stats.exists(filename_stats.cum), stats.exists(filename_stats.update_totals))

#         if ((force) || (!stats.exists(filename_stats.update))) {
#             print(paste("Update", next_update, ": Reading update totals"))
#             stats.update_totals <- load_stats(filename_stats.update_totals)
#             account_ids.next <- stats.update_totals[, unique(account_id)]

#             print(paste("Update", curr_update, ": Reading cumulative stats"))
#             stats.cum <- load_tank_stats_cum(curr_update, active_players = account_ids.next)
#             account_ids.curr <- stats.cum[, unique(account_id)]
#             split_N <- get_DT_splits(stats.cum)
#             stats.cum <- DT_split(stats.cum, split_N)



#             stats.update_totals <- DT_split(stats.update_totals, split_N)

#             for (i in seq(1, length(stats.cum))) {
#                 stats.update_totals[[i]] <- subset(
#                     stats.update_totals[[i]],
#                     account_id %in% account_ids.curr
#                 )
#                 stats.cum[[i]] <- subset(
#                     stats.cum[[i]],
#                     account_id %in% account_ids.next
#                 )
#             }

#             stats.update <- list()
#             for (i in seq(1, length(stats.cum))) {
#                 stats.update[[i]] <- calc_diff_tank_stats(stats.cum[[1]],
#                     stats.update_totals[[1]],
#                     career = TRUE
#                 )
#                 stats.cum[[1]] <- NULL
#                 stats.update_totals[[1]] <- NULL
#             }
#             if (length(stats.update) > 1) {
#                 stats.update <- rbindlist(stats.update, use.names = TRUE, fill = TRUE)
#             } else {
#                 stats.update <- stats.update[[1]]
#             }

#             stats.update <- merge_tankopedia(stats.update) ##
#             stats.update <- calc_tank_stats(stats.update) ##
#             stats.update <- subset(stats.update, battles > 0) ##  since career = TRUE in calc_diff_tank_stats()
#             print(paste("Update", next_update, ": Saving Update stats"))
#             qsave(stats.update, filename_stats.update, nthreads = n_threads)
#             rm(stats.update)
#         } else {
#             print(paste("Update", next_update, ": Update stats exists. Skipping..."))
#         }
#         toc_() # release
#         if (next_update == update) break

#         curr_update <- next_update
#         next_update <- get_next_update(curr_update, preliminary)
#     } # while
#     toc_() # whole run
# } # function


gen_tank_stats_period <- function(update.start, update.end, force = FALSE, verbose = TRUE) {
  filename <- filename_period(update.start, update.end)
  period.str <- paste(update.start, "-", update.end)
  if ((!force) && file.exists(filename)) {
    if (verbose) {
      cat(paste("Stats for period", period.str, "exists\n"))
    }
    return()
  }

  message("gen_tank_stats_period(): Likely broken code. INSPECTG BEFORE RUN")
  stop()
  filename.cum <- filename_career(get_prev_update(update.start))
  filename.period_total <- filename_period_totals(update.start, update.end)
  stopifnot(file.exists(filename.period_total), file.exists(filename.cum))

  tic(paste("Generating tank stats for period", period.str))
  # load data
  stats.cum <- load_tank_stats_cum(get_prev_update(update.start))
  stats.period_totals <- load_tank_stats_period_totals(update.start, update.end)

  # to lower RAM usage
  parts <- get_DT_splits(stats.cum)
  stats.cum <- DT_split(stats.cum, parts)
  stats.period_totals <- DT_split(stats.period_totals, parts)
  stats.period <- list()

  for (i in seq(parts)) {
    stats.period[[i]] <- calc_diff_tank_stats(stats.cum[[1]],
      stats.period_totals[[1]],
      career = TRUE
    )
    stats.cum[[1]] <- NULL
    stats.period_totals[[1]] <- NULL
    stats.period[[i]] <- merge_tankopedia(stats.period[[i]])
    stats.period[[i]] <- calc_tank_stats(stats.period[[i]])
    stats.period[[i]] <- subset(stats.period[[i]], battles > 0)
    stats.period[[i]] <- clean_tank_stats(stats.period[[i]])
  }
  rm(stats.cum)
  rm(stats.period_totals)

  save_tank_stats(stats.period, filename)
  toc_()
}


merge_stats <- function(DT1, DT2) {
  # merge two data.table stats together
  DT.merged <- rbindlist(list(DT1, DT2), use.names = TRUE, fill = TRUE)
  setorder(DT.merged, -last_battle_time)
  DT.merged <- DT.merged[, head(.SD, 1), by = .(account_id, tank_id)]
  return(DT.merged)
}


merge_update_totals <- function(update_start, update_end, preliminary = FALSE) {
  # merge several update-stats together over multiple Blitz releases
  stopifnot(exists("tanks"), exists("updates"), exists("WG_tank_stats"))

  filename_stats.update_totals <- filename_update_totals(update_start)
  stopifnot(file.exists(filename_stats.update_totals))
  stats.update_totals <- qread(filename_stats.update_totals)

  curr_update <- update_start
  next_update <- get_next_update(curr_update, preliminary)

  tic("Merging stats of several updates")
  while (!is.null(next_update)) {
    tic(paste("Merging update stats with update", next_update))

    # read new update
    filename_stats.update_totals <- filename_update_totals(next_update)
    stopifnot(file.exists(filename_stats.update_totals))
    stats.update_totals.new <- qread(filename_stats.update_totals)

    # merge
    stats.update_totals <- merge_stats(stats.update_totals.new, stats.update_totals)

    if (next_update == update_end) {
      toc_()
      toc_()
      return(stats.update_totals)
    }
    curr_update <- next_update
    next_update <- get_next_update(curr_update, preliminary)
    toc_()
  } # for
  toc_()
} # function


# Get only stats for accounts active since update X.Y
accounts_active_since <- function(DT, update_ver = get_latest_update(), last_battle_time = NULL) {
  stopifnot("last_battle_time" %in% colnames(DT))

  if (is.null(last_battle_time)) {
    prev_update <- get_prev_update(update_ver)
    update_start <- get_cut_off_date(prev_update)
  } else {
    update_start <- last_battle_time
  }

  account_ids <- DT[last_battle_time > update_start, unique(account_id)]
  return(subset(DT, account_id %in% account_ids))
}

# Anonymize tank-stats
anonymize_accounts <- function(DT) {
  regions <- DT[, unique(region)]
  primes_vec <- primes::generate_primes(min = 13, max = 11e8)
  for (r in regions) {
    salt <- sample(primes_vec, 1)
    id_min <- regions_account_ids[[r]][1]
    id_max <- regions_account_ids[[r]][2]
    id_range <- id_max - id_min
    DT[region == r, account_id := ((account_id - id_min) * salt) %% id_range + id_min]
  } # for regions

  return(DT)
}

## data.table functions ----------------------------------------------------

DT_filter_battles_tank <- function(btl_period, tier) {
  # return((premium | (btl_total > min_battles.grinds[tier] + min_battles.tiers[tier])) &
  #         (btl_period > min_battles.tiers[tier]))
  # DECISION NOT to consider the number  of battles per tank, but assume the results to even out over larger number of players
  # Filters out 17% of players vs 86% of the above.
  return(btl_period > min_battles.tanks[tier])
}

DT_filter_battles_tank2 <- function(premium, btl_total, btl_period, tier) {
  return(premium | (btl_total > min_battles.grinds[tier]) &
    (btl_period > min_battles.tanks[tier]))
}


DT_filter_tank_is_maxed2 <- function(btl_total, tier, premium) {
  return(premium | (btl_total > min_battles.grinds[tier]))
}


DT_filter_tank_is_maxed <- function(DT) {
  return(DT[, !is.na(battles.tier.maxed)])
}


DT_filter_enough_battles <- function(DT) {
  return(DT[, (!is.na(battles.tier.maxed)) & (battles > min_battles.tanks[tier])])
}


DT_filter_enough_battles.tier <- function(DT) {
  return(DT[, !is.na(tanks.tier.maxed)])
}


DT_filter_enough_battles.rWR <- function(DT) {
  return(DT[, !is.na(rWR)])
}


DT_filter_sub5k.in <- function(DT) {
  return(DT[, (region %in% names(regions)[c(1, 2)]) & (battles.career < 5e3)])
}


DT_filter_sub5k.out <- function(DT) {
  return(DT[, (region %in% names(regions)[c(3, 4)]) | (battles.career > 5e3)])
}


DT_filter_battles_tier <- function(btl_tier, tier) {
  return(btl_tier > min_battles.tiers[tier])
}


DT_ratio <- function(x, y) {
  return(sum(x) / sum(y))
}


DT_mean <- function(x) {
  return(sum(x) / length(x))
}
