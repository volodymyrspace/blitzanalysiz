# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# Functions for Arrow datasets
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library("arrow")
source("R/utils_blitz.R")

ds_filename_career_before <- function(update) {
  return(file.path(get_data_dir(), update, "career_before"))
}

ds_filename_career_after <- function(update) {
  return(file.path(get_data_dir(), update, "career_after"))
}


ds_filename_update_totals <- function(update) {
  return(file.path(get_data_dir(), update, "update_total"))
}

ds_filename_update <- function(update) {
  return(file.path(get_data_dir(), update, "update"))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ds_calc_tank_stats_diff()
#
# calculate different stats of a merged and zeroed DT
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ds_calc_tank_stats_diff <- function(DT, cols.diff = NULL) {
  ## Loop through .old cols and calculate difference use: assign()
  del.idx <- rep(FALSE, nrow(DT))
  if (is.null(cols.diff)) {
    cols.diff <- c(
      "all.battles", "all.capture_points", "all.damage_dealt", "all.damage_received",
      "all.dropped_capture_points", "all.frags", "all.hits", "all.losses",
      "all.shots", "all.spotted", "all.survived_battles", "all.win_and_survived",
      "all.wins", "battle_life_time"
    )
  }
  for (col in cols.diff) {
    col.new <- paste0(col, ".career")
    col.old <- paste0(col, ".old")
    # remove 'all.
    if (grepl("^all\\.", col)) {
      col <- gsub("all\\.", "", col)
    }
    if (col.new %nin% colnames(DT)) {
      next
    }
    DT[get(col.new) == 0, (col.new) := get(col.old)]
    DT[, (col) := get(col.new) - get(col.old)]
    # delete old stats (not needed)
    if (col %nin% c("battles")) {
      DT[, (col.old) := NULL]
    }
    ## clean out bad stats, should this be done by account?
    del.idx <- del.idx | DT[, get(col) < 0]
  } # for

  # rename columns
  colnames(DT) <- gsub("^all\\.", "", colnames(DT))

  del.idx <- del.idx | DT[, battle_life_time.career == 0]
  # accounts_bad_stats <- DT[del.idx, unique(account_id)]
  # del.idx <- DT[, account_id %in% accounts_bad_stats ]
  del.idx <- DT[, .I[del.idx]]
  return(delete(DT, del.idx))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ds_calc_tank_stats()
#
# Calculate tank stats for a dataset chunk. Always career stats + update stats
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ds_calc_tank_stats <- function(DT) {
  ## career_stats : DT is career_stats, not update stats.
  # Tier
  # setattr(DT$tier, 'levels', seq(1,10))

  DT[, WR := wins / battles]
  DT[, spot_rate := spotted / battles]
  DT[, hit_rate := hits / shots]
  DT[, avg_hits := hits / battles]
  DT[, avg_dmg := damage_dealt / battles]
  DT[, avg_kills := frags / battles]
  DT[, KDR := frags / (battles - survived_battles)]
  DT[, DR := damage_dealt / damage_received]
  DT[, survival_rate := survived_battles / battles]
  DT[, survival_win_rate := win_and_survived / battles]

  # update totals

  DT[, battles.update := sum(battles), by = account_id]

  # remove accounts with bad stats
  del.idx <- DT[, battles.update == 0]
  del.idx <- del.idx | DT[, avg_kills > 7 | spot_rate > 7 | hit_rate > 1 | survival_rate > 1]
  del.idx <- DT[, .I[del.idx]]
  DT <- delete(DT, del.idx)

  DT[, WR.update := sum(wins) / first(battles.update), by = account_id]

  ## Calculate Tier stats of all same tier tanks
  DT[, battles.tier.all := sum(battles),
    by = .(account_id, tier)
  ]

  DT[, c(
    "WR.tier.all",
    "avg_dmg.tier.all",
    "spot_rate.tier.all"
  ) := list(
    sum(wins) / first(battles.tier.all),
    sum(damage_dealt) / first(battles.tier.all),
    sum(spotted) / first(battles.tier.all)
  ),
  by = .(account_id, tier)
  ]

  ## Tier stats for non-stock tanks
  DT[DT_filter_tank_is_maxed2(battles.career, tier, is_premium) & (battles > 0),
    battles.tier.maxed := sum(battles),
    by = .(account_id, tier)
  ]

  # DT[ , enough_battles := (! is.na(battles.tier.maxed)) &
  #       DT_filter_battles_tank(battles, tier) ]

  # DT[ (enough_battles ==TRUE),
  #     c('WR.tier.maxed',
  #       'avg_dmg.tier.maxed',
  #       'spot_rate.tier.maxed',
  #       'tanks.tier.maxed') := list(sum(wins) / sum(battles),
  #                                   sum(damage_dealt) / sum(battles),
  #                                   sum(spotted) / sum(battles),
  #                                   .N),
  #     by=.(account_id, tier)];

  DT[
    ((!is.na(battles.tier.maxed)) &
      (battles.tier.maxed > min_battles.tiers[tier])),
    c(
      "WR.tier.maxed",
      "avg_dmg.tier.maxed",
      "spot_rate.tier.maxed",
      "tanks.tier.maxed"
    ) := list(
      sum(wins) / sum(battles),
      sum(damage_dealt) / sum(battles),
      sum(spotted) / sum(battles),
      .N
    ),
    by = .(account_id, tier)
  ]

  # Relative WR

  # DT[ tanks.tier > 1, rWR := WR - (battles.tier*WR.tier - wins)/(battles.tier - battles)];
  # 2020-06-03: changed to comparison to all same tier tank's WR

  DT[
    (!is.na(tanks.tier.maxed)) & (tanks.tier.maxed > 1) &
      DT_filter_battles_tank(battles, tier),
    rWR := WR - WR.tier.maxed
  ]

  # DT[, avg_dmg.update := sum(damage_dealt) / first(battles.update), by=account_id];
  # DT[, spot_rate.update := sum(spotted) / first(battles.update), by=account_id];
  #

  # career

  DT[, WR.career := wins.career / battles.career]
  DT[, spot_rate.career := spotted.career / battles.career]
  DT[, hit_rate.career := hits.career / shots.career]
  DT[, avg_hits.career := hits.career / battles.career]
  DT[, avg_dmg.career := damage_dealt.career / battles.career]
  DT[, avg_kills.career := frags.career / battles.career]
  DT[, KDR.career := frags.career / (battles.career - survived_battles.career)]
  DT[, DR.career := damage_dealt.career / damage_received.career]
  DT[, survival_rate.career := survived_battles.career / battles.career]
  DT[, survival_win_rate.career := win_and_survived.career / battles.career]

  ## Calculate Tier stats of all same tier tanks
  DT[, battles.career.tier := sum(battles.career), by = .(account_id, tier)]

  DT[, c(
    "WR.career.tier",
    "avg_dmg.career.tier",
    "spot_rate.career.tier"
  ) := list(
    sum(wins.career) / first(battles.career.tier),
    sum(damage_dealt.career) / first(battles.career.tier),
    sum(spotted.career) / first(battles.career.tier)
  ),
  by = .(account_id, tier)
  ]

  DT[, battles.career.all := sum(battles.career), by = .(account_id)]
  DT[battles.career.all > 0,
    c(
      "WR.career.all",
      "avg_dmg.career.all"
    ) := list(
      sum(wins.career) / first(battles.career.all),
      sum(damage_dealt.career) / first(battles.career.all)
    ),
    by = .(account_id)
  ]

  return(DT)
} # function

ds_export <- function(DT, filename, ds_format = "parquet") {
  write_dataset(DT, filename,
    format = ds_format,
    partitioning = c("tier"),
  )
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ds_gen_tank_stats_update()
#
# Piecewise stats generation to deal with mem consumption
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ds_gen_tank_stats_update <- function(update = get_latest_update(),
                                     ds_format = "parquet") {
  # generate update (delta) stats
  stopifnot(exists("tanks"))

  tic(paste("Generating update stats for update", update))
  message(paste("Generating update stats for update", update))

  filename.ds.before <- ds_filename_career_before(update)
  filename.ds.update_totals <- ds_filename_update_totals(update)
  filename.ds.update <- ds_filename_update(update)

  # stopifnot(stats.exists(filename_stats.cum), stats.exists(filename_stats.update_totals))

  # if (stats.exists(filename_stats.update) &&  (! force) ) {
  # print(paste('Update', update, ': Update stats exists. Skipping...'))
  # return()
  # }

  message(paste("Update", update, ": Reading update totals"))

  ds.update_totals <- open_dataset(filename.ds.update_totals, partitioning = c("region"))
  ds.career_before <- open_dataset(filename.ds.before, partitioning = c("region"))

  N_rows <- nrow(ds.career_before)
  N_split <- ceiling(nrow(ds.career_before) / row.lim.generate)

  cols.drop <- c("mark_of_mastery", "all.max_frags", "release", "last_battle_time")
  ## Process in chunks
  for (I_split in seq(0, N_split - 1)) {
    ## Calculate account_ids found in both udpate and career datasets
    tic(paste("processing chunk", I_split + 1, "/", N_split))

    message("selecting chunk ", I_split + 1, "/", N_split)
    accounts.update <- as.data.table(ds.update_totals %>%
      filter((account_id %% N_split == I_split) &
        (all.battles > 0)) %>%
      select(account_id) %>%
      collect())[, .(account_id = unique(account_id))]
    setkey(accounts.update, "account_id")

    accounts.career <- as.data.table(ds.career_before %>%
      filter((account_id %% N_split == I_split) &
        (all.battles > 0)) %>%
      select(account_id) %>%
      collect())[, .(account_id = unique(account_id))]
    setkey(accounts.career, "account_id")

    accounts_active <- as.data.frame(merge(accounts.career, accounts.update, all = TRUE))
    rm(accounts.update, accounts.career)
    message("reading chunk ", I_split + 1, "/", N_split)
    ## Calculate datasets to merge
    DT.update <- as.data.table(ds.update_totals %>%
      filter(account_id %% N_split == I_split) %>%
      select(!all_of(cols.drop)) %>%
      inner_join(accounts_active, by = "account_id") %>%
      collect())

    DT.career <- as.data.table(ds.career_before %>%
      filter(account_id %% N_split == I_split) %>%
      select(!all_of(cols.drop)) %>%
      inner_join(accounts_active, by = "account_id") %>%
      collect())
    message("processing chunk ", I_split + 1, "/", N_split)
    setkey(DT.update, "account_id", "tank_id", "region")
    setkey(DT.career, "account_id", "tank_id", "region")
    DT.diff <- merge(DT.career, DT.update, all = TRUE, suffixes = c(".old", ".career"))
    rm(DT.career)
    rm(DT.update)
    rm(accounts_active)

    ## Change NA => 0 to enable calculations
    for (n in names(DT.diff)) {
      if (grepl("\\.old$", n) || grepl("\\.career$", n)) {
        DT.diff[is.na(get(n)), (n) := 0]
      }
    }

    # calculate diff stats
    DT.diff <- ds_calc_tank_stats_diff(DT.diff)

    # merge Tankopedia
    DT.diff <- merge_tankopedia(DT.diff, tanks)

    # Calculate tank stats
    DT.diff <- ds_calc_tank_stats(DT.diff)

    message("writing chunk ", I_split + 1, "/", N_split)

    write_dataset(DT.diff, filename.ds.update,
      format = ds_format,
      partitioning = c("tier"),
      basename_template = paste0("part-", I_split, "-{i}.", ds_format)
    )
    rm(DT.diff)
    toc_()
  } # for N_split

  toc_() # whole run
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ds_load_ts()
#
# Piecewise stats generation to deal with mem consumption
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ds_load_ts <- function(update = get_latest_update(),
                       stats_type,
                       cols = NULL,
                       tank_tier = NULL,
                       tank_type = NULL,
                       tank_nation = NULL,
                       premium = NULL,
                       tank_ID = NULL,
                       server = NULL,
                       modN = NULL) {
  filename.update <- ds_filename_update(update)
  DS.update <- open_dataset(filename.update, partitioning = c("tier"))

  row.lim <- row.lim.use
  if (is.null(cols)) {
    sel_ <- . %>% select(everything())
  } else {
    row.lim <- row.lim * ncol(DS.update) / length(cols)
    sel_ <- . %>% select(all_of(cols))
  }

  if (stats_type == "career") {
    filter_mode <- . %>% filter()
  } else if (stats_type == "update") {
    filter_mode <- . %>% filter(battles > 0)
  } else if (stats_type == "maxed") {
    filter_mode <- . %>% filter(!is.na(battles.tier.maxed))
  } else if (stats_type == "rWR") {
    filter_mode <- . %>% filter(!is.na(rWR))
  }

  filter_ <- . %>% filter((is.null(tank_tier) | tier == tank_tier) &
    (is.null(tank_type) | type == tank_type) &
    (is.null(tank_nation) | nation == tank_nation) &
    (is.null(premium) | is_premium == premium) &
    (is.null(tank_ID) | tank_id == tank_ID) &
    (is.null(server) | region == server)) # filter()

  rows_total <- DS.update %>%
    filter_mode() %>%
    filter_() %>%
    nrow()

  # automatic setting of sample size
  if (is.null(modN)) {
    modN <- ceiling(rows_total / row.lim)
  }
  if (modN <= 1) {
    modN <- NULL
  } else {
    message(rows_total, " total rows, taking 1/", modN)
  }

  DT <- as.data.table(DS.update %>%
    filter_mode() %>%
    filter_() %>%
    filter(is.null(modN) | account_id %% modN == 0) %>%
    sel_() %>%
    collect()) # as.data.table
  return(DT)
} # function


ds_load_ts_update <- function(update = get_latest_update(),
                              cols = NULL,
                              tank_tier = NULL,
                              tank_type = NULL,
                              tank_nation = NULL,
                              premium = NULL,
                              tank_ID = NULL,
                              server = NULL,
                              modN = NULL) {
  return(ds_load_ts(
    update = update,
    stats_type = "update",
    cols = cols,
    tank_tier = tank_tier,
    tank_type = tank_type,
    tank_nation = tank_nation,
    premium = premium,
    tank_ID = tank_ID,
    server = server,
    modN = modN
  ))
}

ds_load_ts_career <- function(update = get_latest_update(),
                              cols = NULL,
                              tank_tier = NULL,
                              tank_type = NULL,
                              tank_nation = NULL,
                              premium = NULL,
                              tank_ID = NULL,
                              server = NULL,
                              modN = NULL) {
  return(ds_load_ts(
    update = update,
    stats_type = "career",
    cols = cols,
    tank_tier = tank_tier,
    tank_type = tank_type,
    tank_nation = tank_nation,
    premium = premium,
    tank_ID = tank_ID,
    server = server,
    modN = modN
  ))
}


ds_load_ts_rWR <- function(update = get_latest_update(),
                           cols = NULL,
                           tank_tier = NULL,
                           tank_type = NULL,
                           tank_nation = NULL,
                           premium = NULL,
                           tank_ID = NULL,
                           server = NULL,
                           modN = NULL) {
  return(ds_load_ts(
    update = update,
    stats_type = "rWR",
    cols = cols,
    tank_tier = tank_tier,
    tank_type = tank_type,
    tank_nation = tank_nation,
    premium = premium,
    tank_ID = tank_ID,
    server = server,
    modN = modN
  ))
}


ds_load_ts_maxed <- function(update = get_latest_update(),
                             cols = NULL,
                             tank_tier = NULL,
                             tank_type = NULL,
                             tank_nation = NULL,
                             premium = NULL,
                             tank_ID = NULL,
                             server = NULL,
                             modN = NULL) {
  return(ds_load_ts(
    update = update,
    stats_type = "maxed",
    cols = cols,
    tank_tier = tank_tier,
    tank_type = tank_type,
    tank_nation = tank_nation,
    premium = premium,
    tank_ID = tank_ID,
    server = server,
    modN = modN
  ))
}
