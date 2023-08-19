source('R/init.R'); 

preliminary = FALSE

# fetch update totals
fetch_tank_stats_update_total(update_list = update.latest, preliminary = preliminary, force = TRUE)

gc()
# generate update stats
gen_tank_stats_update(update = update.latest, force = TRUE)

gc()
# fetch career stats
fetch_tank_stats_career(update = update.latest, preliminary = preliminary, force = TRUE)

gc()
# gen_careers stats (REQUIRED)
gen_tank_stats_cum(update.latest)

gc()
# fetch summary stats 
fetch_tank_summary_stats(update.latest)

# build
source('R/build.R'); 