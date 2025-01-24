tiers <- 1:10
tic("Total time")
clear.stats_in_mem <- TRUE

tic("Cluster creation")
cl <- makeCluster(n_cores, type = cluster_type)
registerDoParallel(cl, cores = n_cores)
toc_()

tic("DT split")
stats.list <- split(stats.update, by = c("tier"))
rm(stats.update)
gc()
toc_()

foreach(stats.tier = rev(stats.list), tier = rev(tiers), .packages = pkgs.loop, .options.snow = snow_options) %dopar% {
  if (OS == "Windows") source("R/utils_blitz.R")

  min_battles.tier <- get_min_battles_tier(tier)
  min_battles.tank <- floor(min_battles.tier / 2)
  min_battles.grind <- get_grind_len(tier)

  tier.roman <- get_tier_roman(tier)
  tierN <- tier
  # stats.tier <- subset(stats.update, (tier == tierN));

  page_weight <- 11 - tier

  build_one("tier.Rmd", file.path("update", update, "tiers", tier), force.update)
  build_one("tanks_per_tier.Rmd", file.path("update", update, "tanks", tier), force.update)

  page_weight.tank_type <- 1
  for (tank_type in c("Heavy Tank", "Medium Tank", "Light Tank", "Tank Destroyer")) {
    page_weight <- page_weight.tank_type
    page_weight.tank_type <- page_weight.tank_type + 1
    stats.tier.type <- subset(stats.tier, type == tank_type)
    build_one("tanks_type.Rmd", file.path("update", update, "tanks", tier, paste0(tank_type, "s")), force.update)
  }

  # page_weight.tanks = 1
  # for (tank_id in tank_ids) {
  #   page_weight = page_weight.tanks
  #   page_weight.tanks = page_weight.tanks +1
  #
  #   build_one('tank.Rmd', file.path('update', update, 'tanks', tier, tank_id), force.update);
  # } # for tank_id
} # for tier

# Free up memory
if (clear.stats_in_mem) {
  if (exists("stats.tier.type")) rm(stats.tier.type)
  if (exists("stats.tier")) rm(stats.tier)
  if (exists("stats.update")) rm(stats.update)
}
gc(full = TRUE, verbose = FALSE)

toc_()

# parallel
stopCluster(cl)
