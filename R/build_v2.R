## Load utils if not loaded
# if (!exists("pkgs")) {
#   source("R/init.R")
# }

stopifnot(!is.null(build.version) && build.version == 2)

##   T  E  S  T       B  U  I  L  D     ############################

test_build <- FALSE

if (test_build) cat("\n## TEST BUILD     ####################\n\n")

####################################################################


## Parameters -------------------------------------------------------
tic("Total rebuild time")

if (is.null(updates.build)) {
  if (build.latest_only) {
    updates.build <- get_latest_update(preliminary)
  } else {
    updates.build <- get_updates(since = "8.0", preliminary)
  }
}

updates.no_build <- NULL
updates.build <- updates.build[!updates.build %in% updates.no_build]
updates.build <- intersect(updates.build, updates.all)
disable_rebuild.career <- FALSE

## This is needed to override default "update" that is function in "stats" package
update <- updates.build[[1]]

tiers <- seq(1, 10)
if (test_build) tiers <- seq(6, 7)
if (!exists("stats.update_ver")) stats.update_ver <- ""

## Main pages
pl.main <- pl_make_main()
pl_build(pl.main, update.latest, force = force.update)

for (update in rev(updates.build)) {
  tic(paste("Update", update))
  message("Building update ", update)

  # career
  pl.career <- pl_make_career(update)
  pl_build(pl.career, update, force = force.update)

  # update
  pl.update.main <- pl_make_update_main(update)
  pl_build(pl.update.main, update, force = force.update)

  for (tierN in seq(10)) {
    message("Building update ", update, " tier ", get_tier_roman(tierN))
    tic(paste("Update", update, "tier", get_tier_roman(tierN)))
    # Tier main
    pl <- pl_make_tiers_main(update, tiers = tierN)
    pl <- pl_merge(pl, pl_make_tank_list(update, tiers = tierN))
    pl <- pl_requires_rebuild(pl, force = force.update, update = update)

    if (length(pl) > 0) pl_build(pl, update, force = force.update, sorted = TRUE)

    # Tier tank types
    for (tank_type in Tank_Types) {
      pl.type <- pl_make_tiers_types(update, tiers = tierN, types = tank_type)
      pl.tanks <- pl_make_tanks(update, tiers = tierN, types = tank_type)
      pl <- pl_merge(pl.type, pl.tanks)
      pl <- pl_requires_rebuild(pl, force = force.update, update = update)
      if (length(pl) > 0) {
        ## Update tiers --------------------------------------------------
        cols.type <- c(
          "account_id", "tank_id", "battles", "battles.career",
          "region", "name", "tier", "nation", "type", "is_premium", "WR",
          "rWR", "WR.tier.maxed", "battles.tier.maxed", "avg_dmg", "avg_kills",
          "spot_rate", "hit_rate"
        )
        cols.tank <- c(
          "account_id", "tank_id", "battles", "battles.career",
          "region", "name", "tier", "nation", "type", "is_premium", "WR",
          "rWR", "WR.tier.maxed", "battles.tier.maxed", "avg_dmg", "avg_kills",
          "spot_rate", "hit_rate", "shots", "survival_rate", "battle_life_time"
        )

        # cols.tier <- c('account_id', 'tank_id', 'battles', 'battles.career',
        #            'battles.career.all', 'battles.tier.maxed', 'battles.tier.all',
        #            'battle_life_time',
        #            'region', 'name', 'tier', 'nation', 'type', 'is_premium',
        #            'WR', 'rWR', 'WR.tier.maxed', 'WR.update', 'WR.tier.all',
        #            'avg_dmg', 'avg_dmg.tier.maxed', 'avg_kills', 'spot_rate',
        #            'hit_rate', 'shots', 'survival_rate')
        stats.tier.type <- ds_load_ts_update(update,
          tank_tier = tierN,
          tank_type = tank_type,
          cols = cols.type
        )
        stats.tier.type.perf <- get_stats_tank_perf(ds_load_ts_maxed(update,
          tank_tier = tierN,
          tank_type = tank_type,
          cols = cols.tank
        ))
        pl_build_auto(pl, update,
          build.type = build.type,
          force = force.update,
          sorted = TRUE,
          n_cores = n_cores.default,
          ex.vars = c("stats.tier.type", "stats.tier.type.perf")
        )
        rm(stats.tier.type.perf, stats.tier.type)
        gc(verbose = FALSE)
      } # if
    } # for tank type

    # Tier premiums
    pl <- pl_make_tiers_premiums(update, tiers = tierN)
    pl <- pl_requires_rebuild(pl, force = force.update, update = update)
    if (length(pl) > 0) {
      pl_build(pl, update, force = force.update)
    }
    toc_()
  } # for tier

  # blog posts
  pl.blog <- pl_make_blog_posts(update)
  pl_build(pl.blog, update, force = force.update, sorted = TRUE)
  toc_()
} # for update

dir.create(file.path("content", "tags"), showWarnings = FALSE)
toc_()
