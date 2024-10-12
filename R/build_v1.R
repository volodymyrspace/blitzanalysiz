## Load utils if not loaded
# if (!exists("pkgs")) {
#   source("R/init.R")
# }

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
    updates.build <- get_updates(since="8.0", preliminary)
  }
}

updates.no_build  <- NULL
updates.build <- updates.build[! updates.build %in% updates.no_build]
updates.build <- intersect(updates.build, updates.all)

disable_rebuild.career <- FALSE

## This is needed to override default "update" that is function in "stats" package
update <- updates.build[[1]]

tiers <- seq(1, 10)
if (test_build) tiers <- seq(6, 7)

if (! exists("stats.update_ver")) stats.update_ver <- ""

## Main pages
pl.main <- pl_make_main()
pl_build(pl.main, update.latest, force = force.update)

for (update in rev(updates.build)) {
  tic(paste("Update", update))
  message("Building update ", update)

  ### Loading data --------------------------------------------------------------------
  ## Load Career stats first ----------------------------------------------------------

  require_rebuild.update <- update %in% updates.build.updates
  require_rebuild.career <- update %in% updates.build.updates
  require_rebuild.blog   <- update %in% updates.build.blog

  pl.career <- pl_make_career(update)

  if ((! exists("stats.update")) || (stats.update_ver != update)) {
	  tic("Loading update data")
    stats.update <- load_tank_stats_update(update)
    stats.update_ver <- update
	  toc_()
  }  # if

  if (! force.update) {
    if (update %in% updates.build.updates) {
      # career stats
      if ( ( ! disable_rebuild.career) &&
           require_rebuilds(pl.career, FALSE, update, stats_career = TRUE)) {
        require_rebuild.career <- TRUE
      } else {
        require_rebuild.career <- FALSE
      }
      # other updates stats
      pl <- pl_make_update_all(update, tiers = tiers, tank_pages = FALSE, stats = stats.update)
      if ( require_rebuild.update || require_rebuilds(pl, FALSE, update)) {
        require_rebuild.update <- TRUE
      } else {
        require_rebuild.update <- FALSE
      }
      require_rebuild.blog <- TRUE
    } else {
      pl.blog <- pl_make_blog_posts(update)
      require_rebuild.blog <- (update %in% updates.build.blog) && require_rebuilds(pl.blog, FALSE, update)
      require_rebuild.update <- FALSE
      require_rebuild.career <- FALSE
    }
  }

  ## Skip update if no update needed
  if (! (require_rebuild.update || require_rebuild.career || require_rebuild.blog )) {
    cat(paste("No rebuild needed for Update ", update), fill = TRUE)
    if (clear.stats_in_mem)  {
	    rm("stats.update")
    }
    gc()
    toc_()
    next
  }

  if (require_rebuild.career) {
	  tic("Loading career data")
	  active_players <- stats.update[, unique(account_id)]
    stats.career <- load_tank_stats_career(update, active_players=active_players)
    toc_()
  }

  #### Build pages  ----------------------------------------------------

  cat("Processing Update stats", fill = TRUE)

  # Build career stats. It takes lot of memory. Therefore Process first and discard
  if (require_rebuild.career) {
    pl_build(pl.career, update, force = force.update, use_career_stats = TRUE)
  }
  if (exists("stats.career")) {
    rm("stats.career")
    gc()
  }
  # update
  if (require_rebuild.update) {
    pl.update.main <- pl_make_update_main(update)
    pl_build(pl.update.main)

    ## Update Main pages --------------------------------------------------
    pl.update <- pl_make_update_all(update, main_pages = FALSE)
		pl.update <- pl_requires_rebuild(pl.update, force = force.update, update = update, stats_career = FALSE)
    if (build.type == "parallel") {
      pl_build_parallel(pl.update, update, force = force.update, sorted = TRUE)
    } else if (build.type == "future") {
      pl_build_future(pl.update, update, force = force.update,
                          sorted = TRUE, n_cores = n_cores.default)
    } else {
      pl_build(pl.update, update, force = force.update, sorted = TRUE)
    }
  }
  # blog posts
  if (require_rebuild.blog) {
    pl.blog <- pl_make_blog_posts(update)
    pl_build(pl.blog, update, force = force.update, sorted = TRUE)
  }
  if (clear.stats_in_mem)  {
	  rm("stats.update")
  }
  gc()
  toc_()
} # for update

dir.create(file.path("content", "tags"), showWarnings = FALSE);
toc_()
