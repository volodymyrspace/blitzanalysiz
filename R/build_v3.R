## Load utils if not loaded
# if (!exists("pkgs")) {
#   source("R/init.R")
# }

stopifnot(!is.null(build.version) && build.version == 3)

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
    updates.build <- get_updates(since = "11.0", preliminary)
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

  if (update %in% updates.build.updates) {
    # update
    pl.update.main <- pl_make_update_main(update, career = TRUE)
    pl_build(pl.update.main, update, force = force.update)

    for (tierN in seq(10)) {
      message("Building update ", update, " tier ", get_tier_roman(tierN))
      tic(paste("Update", update, "tier", get_tier_roman(tierN)))
      # Tier main
      pl <- pl_make_tiers_main(update, tiers = tierN)
      pl <- pl_merge(pl, pl_make_tank_list(update, tiers = tierN))
      pl <- pl_merge(pl, pl_make_tiers_types(update, tiers = tierN))
      pl <- pl_merge(pl, pl_make_tiers_premiums(update, tiers = tierN))
      pl <- pl_requires_rebuild(pl, force = force.update, update = update)

      if (length(pl) > 0) {
        # add tank pages only after update test since some tanks always fail the requires_rebuild test
        pl <- pl_merge(pl, pl_make_tanks(update, tiers = tierN))

        cols <- c(
          "account_id", "tank_id", "battles", "battles.career",
          "region", "name", "tier", "nation", "type", "is_premium", "WR",
          "rWR", "WR.tier.maxed", "battles.tier.maxed", "avg_dmg", "avg_kills",
          "spot_rate", "hit_rate", "shots", "survival_rate", "battle_life_time"
        )

        stats.tier <- ds_load_ts_update(update,
          tank_tier = tierN,
          cols = cols
        )
        stats.tier.perf <- get_stats_tank_perf(stats.tier)

        pl_build_auto(pl, update,
          build.type = build.type,
          force = force.update,
          sorted = TRUE,
          n_cores = n_cores.default,
          ex.vars = c("stats.tier", "stats.tier.perf")
        )
        rm("stats.tier.perf", "stats.tier")
        gc(verbose = FALSE)
      }
      # players
      pl_build(pl_make_tiers_players(update, tiers = tierN))

      toc_()
    } # for tier
  } # if (update %in% updates.build.updates)

  # blog posts
  pl.blog <- pl_make_blog_posts(update)
  pl_build(pl.blog, update, force = force.update, sorted = TRUE)
  toc_()
} # for update

dir.create(file.path("content", "tags"), showWarnings = FALSE)
toc_()
