## utils_knitr.R


## Page build tools --------------------------------------------------------------


chop <- function(string, last = 1) {
    return(gsub(paste0(".{", last, "}$"), "", string))
}

# return target page filename
filename_target <- function(target) {
    if (target == "/") {
        fn_target <- file.path("content", "_index.md")
    } else {
        fn_target <- file.path("content", target, "_index.md")
    }
    return(tolower(gsub(" ", "-", fn_target, fixed = TRUE)))
}


older_than <- function(file1, file2) {
    return(!file.exists(file1) || file_test("-ot", file1, file2))
}


rebuild_needed <- function(file1, file2) {
    return(!file.exists(file1) || file_test("-ot", file1, file2) || (file.size(file1) == 0))
}


# Check whether a page rebuild is needed
require_rebuild <- function(r_script,
                            target,
                            force = FALSE,
                            update.curr = NULL,
                            build_ver = build.version) {
    r_script <- get_script_path(r_script, build_ver)
    target <- filename_target(target)
    stats_filename <- ds_filename_update(update.curr)

    return(force || rebuild_needed(target, stats_filename) || rebuild_needed(target, r_script))
}


## Check directory tree for rebuilds.
require_rebuilds_legacy <- function(r_script, targets, force = FALSE,
                                    update.curr = NULL) {
    for (target in targets) {
        # print(paste('Checking for rebuild:', target))
        if (require_rebuild(r_script, target, force, update.curr)) {
            cat(paste("Requires rebuild:", target), fill = TRUE)
            return(TRUE)
        }
    }
    return(FALSE)
}

## Check directory tree for rebuilds.
require_rebuilds <- function(pagelist, force = FALSE, update = NULL) {
    if (is.null(pagelist)) {
        return(FALSE)
    }
    for (r_script in names(pagelist)) {
        for (target in pagelist[[r_script]]) {
            if (require_rebuild(r_script, target, force, update)) {
                cat(paste("Requires rebuild:", target), fill = TRUE)
                return(TRUE)
            }
        }
    } # for
    return(FALSE)
}


rebuild_updates <- function(update_list,
                            pages = NULL,
                            parallel = TRUE,
                            career = FALSE,
                            main_pages = FALSE,
                            tiers = NULL,
                            tank_pages = FALSE,
                            blog_posts = FALSE,
                            n_cores = NULL) {
    for (update in update_list) {
        # Career stats
        tic(paste("Rebuilding update", update))
        stats.update <<- load_tank_stats_update(update) # global assignment with <<-

        if (career) {
            stats.career <<- load_tank_stats_career(update)
            pl <- pl_make_career(update)
            pl_build(pl, update, force = TRUE, use_career_stats = TRUE)
            if (exists("stats.career")) {
                rm("stats.career")
                gc(verbose = FALSE)
            }
        }

        pl <- pl_make_update_all(update,
            main_pages = main_pages,
            tiers = tiers,
            tank_pages = tank_pages,
            blog_posts = blog_posts,
            stats = stats.update
        )
        # Filter out pages if needed
        if (!is.null(pages)) {
            for (page in names(pl)) {
                if (!(page %in% pages)) {
                    pl[page] <- NULL
                }
            } # for (page)
        }

        # build pagelist
        if (parallel) {
            pl_build_future(pl, update = update, force = TRUE, n_cores = n_cores)
        } else {
            pl_build(pl, update = update, force = TRUE)
        }

        toc_()
    } # for (update)
}


pagemap_add_prefix <- function(pagemap, prefix, r_scripts = NULL) {
    if (is.null(r_scripts)) {
        r_scripts <- names(pagemap)
    }
    for (r_script in r_scripts) {
        pagemap[[r_script]] <- append(prefix, pagemap[[r_script]])
    } # for
    return(pagemap)
}


pagemap2list <- function(pagemap, r_scripts = NULL) {
    if (is.null(r_scripts)) {
        # process all
        r_scripts <- names(pagemap)
    }
    pagelist <- list()
    for (r_script in r_scripts) {
        target_list <- pagemap[[r_script]]
        targets <- target_list[[length(target_list)]]
        if (length(target_list) > 1) {
            for (i in (length(target_list) - 1):1) {
                tmp_targets <- c()
                # print(paste('target_list[[',i,']]', target_list[[i]]))
                for (j in 1:length(target_list[[i]])) {
                    # print(paste('target_list[[',i,']][[',j,']]', target_list[[i]][[j]]))
                    for (t in targets) {
                        tmp_targets <- append(tmp_targets, file.path(target_list[[i]][[j]], t))
                    } # for targets
                } # for
                targets <- tmp_targets
            } # for
        } #  if
        pagelist[[r_script]] <- paste0(targets) # Is this needed?
    } # for r_scripts

    return(pagelist)
}


pl_prune <- function(page_list, prune_list = NULL) {
    if (!is.null(prune_list)) {
        for (r_script in names(prune_list)) {
            if (r_script %in% names(page_list)) {
                page_list[[r_script]] <- base::setdiff(page_list[[r_script]], prune_list[[r_script]])
            }
        }
    } # if prune_list
    return(page_list)
}


pl_filter <- function(pagelist, filter = NULL, r_scripts = NULL) {
    if (is.null(filter)) {
        return(pagelist)
    } else {
        if (is.null(r_scripts)) {
            r_scripts <- names(pagelist)
        }
        res_list <- list()
        for (r_script in r_scripts) {
            p_list <- pagelist[[r_script]]
            tmp <- p_list[grepl(filter, p_list)]
            if (length(tmp) > 0) {
                res_list[[r_script]] <- tmp
            }
        }
        return(res_list)
    }
}


pl_flatten <- function(pagelist, sorted = FALSE) {
    page_dict <- list()

    for (r_script in names(pagelist)) {
        for (page in pagelist[[r_script]]) {
            page_dict[[page]] <- r_script
        }
    }

    pages <- unlist(pagelist, use.names = FALSE)
    if (sorted) {
        pages <- stringr::str_sort(pages, numeric = TRUE)
    }

    r_scripts <- c()

    for (page in pages) {
        # pages <- append(pages, page)
        r_scripts <- append(r_scripts, page_dict[[page]])
    }

    # for (r_script in names(pagelist)) {
    #   for (page in pagelist[[r_script]]) {
    #     pages <- append(pages, page)
    #     r_scripts <- append(r_scripts, r_script)
    #   }
    # }
    return(list("pages" = pages, "r_scripts" = r_scripts))
}


pl_merge <- function(pl.1, pl.2) {
    n1 <- names(pl.1)
    n2 <- names(pl.2)

    n.common <- intersect(n1, n2)
    n1.u <- setdiff(n1, n2)
    n2.u <- setdiff(n2, n1)

    pl <- list()

    for (n in n.common) {
        pl[[n]] <- append(pl.1[[n]], pl.2[[n]])
    }
    pl <- append(pl, pl.1[n1.u])
    pl <- append(pl, pl.2[n2.u])

    return(pl)
}

## Make pagelist  --------------------------------------------------------------


pl_make_test <- function(update = get_latest_update(), tiers = c(6)) {
    pl <- list()
    stopifnot(exists("stats.update"))
    for (tierN in tiers) {
        tank_ids <- as.integer(stats.update[(tier == tierN), unique(tank_id)])
        pm <- list("test.Rmd" = list("test", tierN, tank_ids))
        pl[["test.Rmd"]] <- append(pl[["test.Rmd"]], pagemap2list(pm)[["test.Rmd"]])
    }
    return(pl)
}


pl_make_main <- function() {
    pagemap <- list(
        "home.Rmd" = list("/"),
        "updates.Rmd" = list("update"),
        "blog.Rmd" = list("blog"),
        "changelog.Rmd" = list("changelog"),
        "TODO.Rmd" = list("TODO"),
        "FAQ.Rmd" = list("FAQ"),
        "about.Rmd" = list("about")
    )

    return(pagemap2list(pagemap))
}


pl_make_career <- function(update = get_latest_update()) {
    pm <- list("career.Rmd" = list("update", update, "career"))
    return(pagemap2list(pm))
}


pl_make_update_all <- function(update = get_latest_update(),
                               main_pages = TRUE,
                               tiers = seq(1, 10),
                               tank_pages = TRUE,
                               blog_posts = TRUE,
                               stats = NULL) {
    pl <- list()
    if (main_pages) {
        pl <- pl_make_update_main(update)
    }
    pl <- append(pl, pl_make_tiers(update, tiers = tiers))
    if (tank_pages) {
        pl <- append(pl, pl_make_tanks(update, tiers = tiers, stats = stats))
    }
    if (blog_posts) {
        pl <- append(pl, pl_make_blog_posts(update))
    }
    return(pl)
}

pl_make_update_main <- function(update = get_latest_update(), career = FALSE) {
    pagemap <- list(
        "update.Rmd" = list(update),
        "players.Rmd" = list(update, "players")
    )

    pagemap <- pagemap_add_prefix(pagemap, "update")
    pl <- pagemap2list(pagemap)

    if (career) {
        pl.career <- pl_make_career(update = update)
        pl <- pl_merge(pl, pl.career)
    }
    return(pl)
}


pl_make_blog_posts <- function(update = get_latest_update()) {
    # Blog Posts
    posts <- list()

    post_srcs <- list.files(file.path("R", "posts", update), pattern = "\\.Rmd$")
    for (post in rev(post_srcs)) {
        posts[[file.path("posts", update, post)]] <- file.path("blog", sub(" ", "_", sub("\\.Rmd$", "", post)))
    } # for

    return(posts)
} # function


pl_make_tiers_main <- function(update = get_latest_update(),
                               tiers = seq(1, 10)) {
    if (is.null(tiers) || (length(tiers) == 0)) {
        return(list())
    }
    pagemap <- list(
        "tier.Rmd" = list(update, tiers)
    )

    pagemap <- pagemap_add_prefix(pagemap, "update")
    pl <- pagemap2list(pagemap)

    return(pl)
}

pl_make_tiers_players <- function(update = get_latest_update(),
                                  tiers = seq(1, 10)) {
    if (is.null(tiers) || (length(tiers) == 0)) {
        return(list())
    }
    pagemap <- list(
        "tier_players.Rmd" = list(update, tiers, "players")
    )

    pagemap <- pagemap_add_prefix(pagemap, "update")
    pl <- pagemap2list(pagemap)

    return(pl)
}

pl_make_tiers_types <- function(update = get_latest_update(),
                                tiers = seq(1, 10),
                                types = Tank_Types) {
    if (is.null(tiers) || (length(tiers) == 0)) {
        return(list())
    }
    pagemap <- list("tanks_type.Rmd" = list(update, tiers, paste0(types, "s")))

    pagemap <- pagemap_add_prefix(pagemap, "update")
    pagelist <- pagemap2list(pagemap)

    ## Remove non-existent tank type pages from tiers I & II
    prunemap <- list("tanks_type.Rmd" = list("update", update, 1, paste0(Tank_Types[c(3, 4)], "s")))
    prunelist <- pagemap2list(prunemap)
    prunemap <- list("tanks_type.Rmd" = list("update", update, 2, "Heavy Tanks"))
    prunelist <- Map(c, prunelist, pagemap2list(prunemap))
    pagelist <- pl_prune(pagelist, prunelist)

    return(pagelist)
}


pl_make_tiers_premiums <- function(update = get_latest_update(),
                                   tiers = seq(1, 10)) {
    if (is.null(tiers) || (length(tiers) == 0)) {
        return(list())
    }
    pagemap <- list()

    if (length(intersect(tiers, seq(2, 10))) > 0) {
        pagemap[["tanks_premiums.Rmd"]] <- list(
            update,
            intersect(tiers, seq(2, 10)),
            "premium tanks"
        )
        pagemap <- pagemap_add_prefix(pagemap, "update")
    }

    return(pagemap2list(pagemap))
}


pl_make_tiers <- function(update = get_latest_update(), tiers = seq(1, 10)) {
    if (is.null(tiers) || (length(tiers) == 0)) {
        return(list())
    }
    pl <- pl_make_tiers_main(update, tiers)
    pl <- pl_merge(pl, pl_make_tiers_types(update, tiers))
    pl <- pl_merge(pl, pl_make_tiers_premiums(update, tiers))
    pl <- pl_merge(pl, pl_make_tank_list(update, tiers))
    pl <- pl_merge(pl, pl_make_tiers_players(update, tiers))
    return(pl)
}


# pl_make_tiers.OLD <- function(update   = get_latest_update(), tiers = seq(1, 10)) {
#   if (is.null(tiers) || (length(tiers) == 0)) {
#     return(list())
#   }
#   pagemap <- list('tier.Rmd'  = list(update, tiers),
#                   'players_tier.Rmd'   = list(update, tiers, "players"),
#                   'tanks_type.Rmd'  = list(update, tiers, paste0(Tank_Types, 's'))
#                   ) # list

#   if (length(intersect(tiers, seq(2,10))) > 0)  {
#     pagemap[['tanks_premiums.Rmd']]  = list(update, intersect(tiers, seq(2,10)), 'premium tanks')
#   }

#   pagemap  <- pagemap_add_prefix(pagemap, 'update')
#   pagelist <- pagemap2list(pagemap)

#   ## Remove non-existent tank type pages from tiers I & II
#   prunemap  <- list('tanks_type.Rmd' = list('update', update, 1, paste0(Tank_Types[c(3,4)], 's')))
#   prunelist <- pagemap2list(prunemap)
#   prunemap  <- list('tanks_type.Rmd' = list('update', update, 2, 'Heavy Tanks'))
#   prunelist <- Map(c, prunelist, pagemap2list(prunemap))
#   pagelist  <- pl_prune(pagelist, prunelist)

#   return(pagelist)
# }

pl_make_tank_list <- function(update = get_latest_update(),
                              tiers = seq(1, 10)) {
    pm <- list("tank_list_tier.Rmd" = list("update", update, tiers, "tanks"))
    return(pagemap2list(pm))
}

pl_make_tanks <- function(update = get_latest_update(),
                          tiers = seq(1, 10),
                          types = Tank_Types,
                          tank_ids = NULL,
                          stats = NULL) {
    stopifnot(exists("tanks"))
    pl <- list()
    if (is.null(tank_ids)) {
        for (tierN in tiers) {
            if (is.null(stats)) {
                tank_ids <- as.integer(tanks[(tier == tierN) & (type %in% types), sort(tank_id)])
            } else {
                tank_ids <- stats[tier == tierN, .(tank_id = unique(tank_id))][order(tank_id)]
            }
            pm <- list("tank.Rmd" = list("update", update, tierN, "tanks", tank_ids))
            pl[["tank.Rmd"]] <- append(pl[["tank.Rmd"]], pagemap2list(pm)[["tank.Rmd"]])
        }
    } else {
        for (tank_id in tank_ids) {
            tier <- get_tank_tier(tank_id)
            pm <- list("tank.Rmd" = list("update", update, tier, "tanks", tank_id))
            pl[["tank.Rmd"]] <- append(pl[["tank.Rmd"]], pagemap2list(pm)[["tank.Rmd"]])
        }
    }
    return(pl)
}


pl_make_updates <- function(update.list, pl_make_func, r_scripts) {
    pl <- list()
    for (u in update.list) {
        pm <- pl_make_func(u)
        for (r_script in r_scripts) {
            pl[[r_script]] <- append(pl[[r_script]], pagemap2list(pm, r_script)[[r_script]])
        }
    }
    return(pl)
}


pl_requires_rebuild <- function(pagelist, force = FALSE, update = NULL) {
    pl <- list()
    if (!is.null(pagelist)) {
        for (r_script in names(pagelist)) {
            plt <- c()
            for (target in pagelist[[r_script]]) {
                if (require_rebuild(r_script, target, force, update)) {
                    plt <- c(plt, target)
                }
            } # for target
            if (length(plt) > 0) {
                pl[[r_script]] <- plt
            }
        } # for r_script
    }
    return(pl)
}


knit_chkfile <- function(outfile) {
    content <- readLines(outfile)
    if (length(content) < 10) {
        return(FALSE)
    }
    ret <- grep(DELETE_PAGE, content, fixed = TRUE)
    return(length(ret) == 0)
}

## Build pages ########################

get_script_path <- function(r_script, build_ver = build.version) {
    return(file.path("R", paste0("v", build_ver), r_script))
}


build_one <- function(r_script, target,
                      update.curr = NULL,
                      force = FALSE,
                      multi_process = FALSE,
                      build_ver = build.version) {
    if (!require_rebuild(r_script, target, force, update.curr)) {
        message(target, ": OK")
        return(FALSE)
    }
    tic(target)
    if (!multi_process) message(target, "  : Processing")

    # needed for the pages
    r_script <- get_script_path(r_script, build_ver)
    target_file <- filename_target(target)
    dir_name <- dirname(target_file)
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
    unlink(file.path(dir_name, "figure"), recursive = TRUE)

    knitr::opts_knit$set(width = 80, base.dir = dir_name)
    ## debug
    #  trace(recover, sink)
    # if (dir_name == 'content')
    #   target_str <- '/'
    # else
    #   target_str <- str_remove(dir_name, 'content/')



    outfile <- knitr::knit(r_script, target_file,
        quiet = TRUE,
        encoding = "UTF-8",
        envir = parent.frame()
    )

    if (!knit_chkfile(outfile)) {
        # if error, delete the knitted file and dir
        unlink(dir_name, recursive = TRUE)
        message(target, "  : REMOVED")
        toc_(quiet = TRUE)
    } else {
        toc_()
    }
    return(outfile)
}


pl_build_updates <- function(pagelist, force = FALSE,
                             use_career_stats = FALSE) {
    tic("pl_build_updates()")

    update.loaded <- NULL
    i <- 1
    for (r_script in names(pagelist)) {
        for (page in pagelist[[r_script]]) {
            if (grepl("posts/", r_script)) {
                update.page <- get_path_elem(r_script, 2)
            } else {
                update.page <- get_path_elem(page, 2)
            }

            if (is.null(update.loaded) || (update.page != update.loaded)) {
                tic(paste("loading stats for update", update.page))
                stats.update <- load_tank_stats_update(update.page)
                update.loaded <- update.page
                toc_()
            }
            tic(paste(page))
            target_page <- page
            build_one(r_script, page, update.curr = update.page, force = force)
            i <- i + 1
            toc_()
        } # for
    } # for
    toc_()
}


pl_build <- function(pagelist,
                     update = get_latest_update(),
                     force = FALSE,
                     sorted = FALSE) {
    tic("pl_build() total")
    page_weight <- 1

    flatlist <- pl_flatten(pagelist, sorted)
    n_pages <- length(flatlist[["pages"]])
    threads.max <- parallel::detectCores() - 1
    DT.threads <- setDTthreads(threads.max)
    blas_set_num_threads(threads.max)

    if (n_pages > 0) {
        for (i in seq(1, n_pages)) {
            page <- flatlist[["pages"]][[i]]
            r_script <- flatlist[["r_scripts"]][[i]]
            target_page <- page
            build_one(r_script, page,
                update.curr = update,
                force = force
            )
        } # foreach
    }
    toc_()
}


pl_build_future <- function(pagelist,
                            update = get_latest_update(),
                            force = FALSE,
                            sorted = TRUE,
                            pkgs = pkgs.loop,
                            ex.vars = ex.vars.future,
                            f.plan = "multicore",
                            n_cores = NULL) {
    tic("pl_build_future() Total")
    OS <- Sys.info()[["sysname"]]
    if (OS == "Windows") {
        f.plan <- "multisession"
    } else {
        # Cairo::CairoFonts(regular="sans:style=Light",
        #                   bold="sans:style=Semibold",
        #                   italic="sans:style=LightItalic",
        #                   bolditalic="sans:style=SemiboldItalic,Semibold Italic")
    }

    DT.threads <- setDTthreads(1) # BLAS will fail without this
    blas_set_num_threads(1)

    flatlist <- pl_flatten(pagelist, sorted)
    pages <- flatlist[["pages"]]
    r_scripts <- flatlist[["r_scripts"]]
    n_pages <- length(pages)
    n_cores <- estimate_cores(n_cores = n_cores)

    res <- list()
    done <- rep(FALSE, n_pages)
    tasks <- seq(1:n_pages)

     plan(get(f.plan), workers = n_cores)
    message(paste("Plan =", f.plan, "using", n_cores, "cores"))

    if (n_pages > 0) {
        for (i in 1:n_pages) {
            r_script <- r_scripts[[i]]
            page <- pages[[i]]

            if (require_rebuild(r_script, page, force = force, update.curr = update)) {
                res[[i]] <- future(
                    {
                        tryCatch(
                            {
                                if (f.plan == "multisession") source("R/init.R")
                                target_page <- page
                                build_one(r_script, page,
                                    update.curr = update,
                                    force = force,
                                    multi_process = TRUE
                                )
                            },
                            error = function(e) {
                                message(conditionMessage(e))
                            },
                            warning = function(w) {
                                message(conditionMessage(w))
                            }
                        )
                    },
                    packages = pkgs,
                    globals = ex.vars
                )
                message(paste(page, " : Processing"))
            } else {
                res[[i]] <- future({
                    TRUE
                })
                message(paste(page, " : OK"))
            }
            # Update status of finished futures
            ready <- resolved(res)
            change <- xor(ready, done[1:i])
            for (task in (1:i)[change]) {
                value(res[[task]])
                done[[task]] <- TRUE
            }
        } # for

        while (TRUE) {
            ready <- resolved(res)
            change <- xor(ready, done)
            done <- ready
            for (task in tasks[change]) {
                value(res[[task]])
            }
            if (sum(done == TRUE) == n_pages) break
            Sys.sleep(1)
        }
    }
    toc_()
    setDTthreads(DT.threads)
    blas_set_num_threads(DT.threads)
}


pl_build_parallel <- function(pagelist,
                              update = get_latest_update(),
                              force = FALSE,
                              sorted = FALSE,
                              ex.vars = NULL,
                              n_cores = NULL) {
    tic("pl_build_parallel() Total")
    OS <- Sys.info()[["sysname"]]
    snow_options <- NULL
    if (OS == "Linux") {
        cluster_type <- "FORK"
    } else if (OS == "Windows") {
        cluster_type <- "PSOCK"
    }

    flatlist <- pl_flatten(pagelist, sorted)
    n_pages <- length(flatlist[["pages"]])

    n_cores <- estimate_cores(n_cores = n_cores)

    print(paste("Using", n_cores, "cores"))
    ## funcs using OpenBLAS will fail without this
    n_threads <- set_threads(1)

    cl <- parallel::makeCluster(n_cores, type = cluster_type, outfile = "")
    registerDoParallel(cl, cores = n_cores)

    foreach(
        page = flatlist[["pages"]], r_script = flatlist[["r_scripts"]],
        .export = ex.vars, .packages = pkgs.loop, .options.snow = snow_options
    ) %dopar% {
        if (OS == "Windows") {
            source("R/utils_blitz.R")
        }
        target_page <- page
        build_one(r_script, page,
            update.curr = update,
            force = force, multi_process = TRUE
        )
    } # foreach

    stopCluster(cl)
    rm(cl)
    registerDoSEQ()

    ## Reset BLAS threads setting
    set_threads(n_threads)

    toc_()
}

pl_build_parallel2 <- function(pagelist,
                               update = get_latest_update(),
                               force = FALSE,
                               ex.vars = NULL,
                               n_cores = 4,
                               ...) {
    tic("pl_build_parallel2() Total")
    OS <- Sys.info()[["sysname"]]
    snow_options <- NULL
    if (OS == "Linux") {
        cluster_type <- "FORK"
    } else if (OS == "Windows") {
        cluster_type <- "PSOCK"
    }

    flatlist <- pl_flatten(pagelist)
    n_pages <- length(flatlist[["pages"]])
    n_cores <- min(n_cores, n_pages)

    ## funcs using OpenBLAS will fail without this
    blas_set_num_threads(1)
    omp_set_num_threads(1) # MKL

    cl <- parallel::makeCluster(n_cores, type = cluster_type, outfile = "")
    registerDoParallel(cl, cores = n_cores)

    foreach(...,
        page = flatlist[["pages"]], r_script = flatlist[["r_scripts"]],
        .export = ex.vars, .packages = pkgs.loop, .options.snow = snow_options
    ) %dopar% {
        if (cluster_type == "PSOCK") {
            source("R/utils_blitz.R")
        }
        target_page <- page
        build_one(r_script, page,
            update.curr = update,
            force = force
        )
    } # foreach

    stopCluster(cl)
    rm(cl)
    registerDoSEQ()

    ## Reset BLAS threads setting
    blas_set_num_threads(1)
    omp_set_num_threads(1) # MKL

    toc_()
}


pl_build_auto <- function(pagelist,
                          update = get_latest_update(),
                          build.type = build.type,
                          force = FALSE,
                          sorted = TRUE,
                          ...) {
    if (build.type == "future") {
        pl_build_future(
            pagelist = pagelist,
            update = update,
            force = force,
            sorted = sorted,
            ...
        )
    } else if (build.type == "parallel") {
        pl_build_parallel(
            pagelist = pagelist,
            update = update,
            force = force,
            sorted = sorted,
            ...
        )
    } else {
        pl_build(
            pagelist = pagelist,
            update = update,
            force = force,
            sorted = sorted
        )
    } # if
} # function


mult_format <- function(x) {
    return(format(100 * x, digits = 2))
}


div_format <- function(x, divider = 1, suffix = "", n_digits = 0) {
    return(paste0(round(x / divider, digits = n_digits), suffix))
}


percent_format <- function(x, digits = 2, signif = 3) {
    if (is.null(signif)) {
        paste0(round(100 * x, digits = digits), "%")
    } else {
        paste0(round(signif(100 * x, digits = signif), digits), "%")
    }
}


percent_range_format <- function(x, auto_fmt = TRUE) {
    res <- c()
    if (length(x) == 1) {
        res <- sprintf("%.0f%%+", 100 * x[1])
    } else {
        for (i in 1:(length(x) - 1)) {
            if (auto_fmt && x[i + 1] == 1) {
                res <- append(res, sprintf("%.0f%%+", 100 * x[i]))
            } else {
                res <- append(res, sprintf("%.0f - %.0f%%", 100 * x[i], 100 * x[i + 1]))
            }
        }
    }
    return(res)
}

## REFACTOR. This function is garbage. digits is used twice
auto_format <- function(x, digits = 3, no_sfx = FALSE) {
    if (no_sfx) {
        return(format(signif(x, digits), digits = digits, big.mark = " ", scientific = FALSE))
    } else {
        tmp <- get_suffix(x)
        return(paste0(format(signif(x / tmp$div, digits),
            digits = digits,
            big.mark = " ", scientific = FALSE
        ), tmp$suffix))
    }
}


auto_range_format <- function(x, last_open = TRUE) {
    res <- c()
    if (last_open) {
        for (i in 1:(length(x) - 2)) {
            res <- append(res, paste(auto_format(x[i]), "-", auto_format(x[i + 1])))
        }
        res <- append(res, paste0(auto_format(x[length(x) - 1]), "+"))
    } else {
        for (i in 1:(length(x) - 1)) {
            res <- append(res, paste(auto_format(x[i]), "-", auto_format(x[i + 1])))
        }
    }
    return(res)
}


n_digits <- function(x) {
    return(min(which(x * 10^(0:20) == floor(x * 10^(0:20)))) - 1)
}


get_suffix <- function(x) {
    x.sfx <- ""
    x.div <- 1
    x.digits <- 0
    x_max <- max(x)
    if (x_max > 1e9) {
        x.sfx <- "bn"
        x.div <- 1e9
    } else if (x_max > 1e6) {
        x.sfx <- "M"
        x.div <- 1e6
    } else if (x_max > 5e3) {
        x.sfx <- "k"
        x.div <- 1e3
    } else if (x_max < 1) {
        x.digits <- n_digits(x_max)
    }
    return(list(suffix = x.sfx, div = x.div, digits = x.digits))
}


print_if <- function(test_if, txt_if, txt_if_not = "") {
    if (test_if) {
        return(txt_if)
    } else {
        return(txt_if_not)
    }
}


md_hugo_expand <- function(txt = "", close = FALSE) {
    if (close) {
        return("{{% /expand %}}")
    } else {
        return(paste0('{{% expand "', txt, '" %}}'))
    }
}

## Print funcs -------------------------------------------------------------


md_table <- function(res, update_ver = update, path = NULL,
                     update_path_lvl = NULL, enough_players = NULL,
                     cols.no_format = NULL) {
    # by default data.table changes DT by ref
    res.p <- copy(res)
    res.cols <- colnames(res.p)

    cols.remove <- c()

    for (col in setdiff(intersect(res.cols, cols.int), cols.no_format)) {
        cols.remove <- append(cols.remove, col)
        if (res.p[, max(get(col))] >= 1e6) {
            res.p[, (col) := auto_format(get(col), digits = 3)]
        } else {
            res.p[, (col) := auto_format(get(col), digits = 3, no_sfx = TRUE)]
        }
    } # for

    res.cols <- res.cols[!res.cols %in% cols.remove]
    cols.remove <- c()

    for (col in setdiff(intersect(res.cols, cols.ratio), cols.no_format)) {
        cols.remove <- append(cols.remove, col)
        res.p[, (col) := round(get(col), digits = 2)]
    } # for

    res.cols <- res.cols[!res.cols %in% cols.remove]

    for (col in setdiff(res.cols, cols.no_format)) {
        for (col.pct in cols.pct) {
            if (grepl(col.pct, col, fixed = TRUE)) {
                res.p[, (col) := percent_format(get(col), signif = 3, digits = 2)]
            } # if
        } # for
    } # for

    if ("Tank" %in% colnames(res.p)) {
        res.p <- md_tank_list(res.p,
            update = update_ver, path = path,
            update_path_lvl = update_path_lvl,
            enough_players = enough_players, print = FALSE
        )
    }
    return(kable(res.p, digits = 3, format = "markdown"))
}


md_table_histogram <- function(res, var, breaks = 20, lims = NULL) {
    if (length(range(breaks)) == 2) {
        h <- hist(res[res %between% range(breaks)], breaks = breaks, plot = FALSE)
    } else {
        h <- hist(res, breaks = breaks, plot = FALSE)
    }
    sum.counts <- sum(h$counts)
    h$density <- h$counts / sum.counts
    h$cum <- cumsum(h$density)
    len <- length(h$breaks)
    tmp <- list()
    tmp[[var]] <- h$breaks[2:len]
    tmp[["Share %"]] <- h$density
    tmp[["Cumulative share %"]] <- h$cum

    res <- as.data.table(tmp)

    if (!is.null(lims)) {
        res <- res[(get(var) >= lims[[1]]) & (get(var) <= lims[[2]])]
    }
    md_table(res)
}


md_table_quantiles <- function(res, var, breaks) {
    res <- as.data.table(quantile(res, breaks, names = FALSE, na.rm = TRUE))
    setnames(res, "V1", var)
    res[, "% of Players" := breaks]
    setcolorder(res, c("% of Players", var))
    md_table(res)
}


md_tank_list <- function(tank_list, update = NULL, path = NULL, update_path_lvl = NULL,
                         enough_players = NULL, tank_var = "Tank", print = TRUE) {
    if (!"tank_id" %in% colnames(tank_list)) {
        tank_list[, tank_id := get_tank_id(get(tank_var))]
    }
    tank_list[, Tank := md_link_tank(
        tank_IDs = tank_id, update = update,
        path = path, update_path_lvl = update_path_lvl
    )]
    if (!is.null(enough_players)) {
        tank_list[!tank_id %in% enough_players, Tank := get_tank_name(tank_id)]
    }
    tank_list[, tank_id := NULL]
    if (print) {
        return(kable(tank_list, digits = 0, format = "markdown"))
    } else {
        return(tank_list)
    }
}


md_link_tank <- function(tank_IDs = NULL,
                         tank_names = NULL,
                         update = NULL,
                         path = NULL,
                         update_path_lvl = NULL,
                         tank_str = NULL,
                         rel_link = NULL) {
    stopifnot(exists("tanks"), length(c(tank_IDs, tank_names)) > 0)

    if (is.null(tank_names)) {
        tmp <- tanks[tank_id %in% tank_IDs][match(tank_IDs, tank_id), .(name, tier)]
        tank_names <- tmp$name
        # tank_tiers <- tmp$tier
    }

    links <- md_path_tank(
        tank_IDs = tank_IDs, tank_names = tank_names,
        update = update, path = path,
        update_path_lvl = update_path_lvl
    )
    if (!is.null(rel_link)) {
        links <- paste0(links, rel_link)
    }

    if (is.null(links)) {
        if (is.null(tank_str)) {
            return(tank_names)
        } else {
            return(tank_str)
        }
    } else if (is.null(tank_str)) {
        return(paste0("[", tank_names, "](", links, ")"))
    } else {
        return(paste0("[", tank_str, "](", links, ")"))
    }
}


md_path_tank <- function(tank_IDs = NULL,
                         tank_names = NULL,
                         update = NULL,
                         path = NULL,
                         update_path_lvl = NULL) {
    stopifnot(exists("tanks"), length(c(tank_IDs, tank_names)) > 0)
    stopifnot(is.null(update_path_lvl) || (as.integer(update_path_lvl) >= 0))

    if (!is.null(tank_IDs)) {
        tmp <- tanks[tank_id %in% tank_IDs][match(tank_IDs, tank_id), .(name, tier)]
        tank_tiers <- tmp$tier
    } else if (!is.null(tank_names)) {
        tmp <- tanks[name %in% tank_names][match(tank_names, name), .(tank_id, tier)]
        tank_IDs <- tmp$tank_id
        tank_tiers <- tmp$tier
    } else {
        stop("No tank_IDs or tank_names given")
    }
    if (any(is.na(tank_tiers))) {
        stop(paste("tank_ID is N/A. tank_names:", tank_names))
    }
    if (!is.null(path)) {
        links <- file.path(path, tank_IDs)
    } else if (!is.null(update_path_lvl)) {
        links <- file.path(tank_tiers, "tanks", tank_IDs)
        for (i in seq.int(0, update_path_lvl)) {
            if (i == 0) next
            links <- file.path("..", links)
        }
    } else if (is.null(update) || (!update %in% updates.build.updates)) {
        links <- file.path("/update", "latest", tank_tiers, "tanks", tank_IDs)
    } else {
        links <- file.path("/update", update, tank_tiers, "tanks", tank_IDs)
    }
    return(links)
}


txt_default <- function(base_text = NULL, append_text = NULL, default_text = NULL) {
    if (is.null(append_text)) {
        append_text <- default_text
    }
    return(paste(base_text, append_text))
}



update_if_final <- function(update, txt_final, txt_preliminary) {
    if (is_preliminary(update)) {
        return(txt_preliminary)
    } else {
        return(txt_final)
    }
}


update_version <- function(update) {
    if (is_preliminary(update)) {
        return(paste0(update, "*"))
    } else {
        return(update)
    }
}

link_txt <- function(base_text = NULL,
                     append_text = NULL,
                     link_text = NULL,
                     capital_1st = FALSE) {
    txt <- ""
    if (is.null(link_text)) {
        stopifnot(!is.null(base_text))
        if (is.null(append_text)) {
            txt <- base_text
        } else {
            txt <- paste(base_text, append_text)
        }
    } else {
        txt <- link_text
    }
    if (capital_1st) {
        txt <- capitalize(txt)
    }
    return(txt)
}


link_txt_update <- function(update = NULL,
                            link_text = NULL,
                            append_text = NULL,
                            capital_1st = FALSE) {
    base_text <- ""
    if (is.null(update)) {
        base_txt <- "latest update"
    } else {
        base_text <- paste("update", update)
    }

    return(link_txt(
        base_text = base_text,
        link_text = link_text,
        append_text = append_text,
        capital_1st = capital_1st
    ))
}


md_link_update <- function(update = NULL,
                           link_text = NULL,
                           append_text = NULL,
                           rel_link = NULL,
                           capital_1st = FALSE) {
    stopifnot(exists("updates.build.updates"))

    txt <- link_txt_update(
        update = update,
        link_text = link_text,
        append_text = append_text,
        capital_1st = capital_1st
    )

    if (is.null(update)) {
        return(paste0("[", txt, "](/update/latest/", rel_link, ")"))
    } else if (update %in% updates.build.updates) {
        return(paste0("[", txt, "](/update/", update, "/", rel_link, ")"))
    } else {
        return(txt)
    }
}


# md_link_update <- function(update = NULL,
#                             base_text = NULL,
#                             link_text = NULL,
#                             append_text = NULL,
#                             rel_link=NULL,
#                             capital_1st=FALSE) {
#   if (! is.null(base_text)) {
# 		append_text <- txt_default(base_text = base_text,
#                                 append_text = append_text,
#                                 default_text = "stats")
# 	}
# 	return(md_link_update(update = update,
#                         link_text = link_text,
#                         append_text = append_text,
#                         rel_link = rel_link,
#                         capital_1st = capital_1st))

# }


md_link <- function(update = NULL,
                    base_text = NULL,
                    link_text = NULL,
                    append_text = NULL,
                    rel_link = NULL,
                    capital_1st = FALSE) {
    stopifnot(!all(is.null(base_text), is.null(link_text)))

    base_text <- txt_default(
        base_text = base_text,
        append_text = append_text,
        default_text = "stats"
    )

    link_text <- link_txt(
        base_text = base_text,
        link_text = link_text,
        append_text = append_text,
        capital_1st = capital_1st
    )

    return(md_link_update(
        update = update,
        link_text = link_text,
        append_text = NULL,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


md_link_update_career <- function(update = NULL,
                                  link_text = NULL,
                                  append_text = NULL,
                                  rel_link = NULL,
                                  capital_1st = FALSE) {
    rel_link <- paste0("career/", rel_link)

    return(md_link_update(
        update = update,
        base_text = "career",
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


md_link_career <- function(update = NULL,
                           link_text = NULL,
                           append_text = NULL,
                           rel_link = NULL,
                           capital_1st = FALSE) {
    rel_link <- paste0("career/", rel_link)

    return(md_link(
        update = update,
        base_text = "career",
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


md_link_update_players <- function(update = NULL,
                                   link_text = "players",
                                   append_text = NULL,
                                   rel_link = NULL,
                                   capital_1st = FALSE) {
    rel_link <- paste0("players/", rel_link)
    return(md_link_update(
        update = update,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


md_link_players <- function(update = NULL,
                            link_text = NULL,
                            append_text = NULL,
                            rel_link = NULL,
                            capital_1st = FALSE) {
    rel_link <- paste0("players/", rel_link)

    return(md_link(
        update = update,
        base_text = "players",
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


# Link to tier stats with update version
md_link_update_tier <- function(update = NULL,
                                tier = NULL,
                                link_text = NULL,
                                append_text = NULL,
                                rel_link = NULL,
                                capital_1st = FALSE) {
    stopifnot(!is.null(tier))

    rel_link <- paste0(tier, "/", rel_link)
    tier.roman <- get_tier_roman(tier)

    append_text <- link_txt(
        base_text = paste("tier", tier.roman),
        append_text = append_text,
        link_text = link_text
    )

    return(md_link_update(
        update = update,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}

# Link to tier stats w/o update version
md_link_tier <- function(update = NULL,
                         tier = NULL,
                         link_text = NULL,
                         append_text = NULL,
                         rel_link = NULL,
                         capital_1st = FALSE) {
    stopifnot(!is.null(tier))

    rel_link <- paste0(tier, "/", rel_link)
    tier.roman <- get_tier_roman(tier)

    return(md_link(
        update = update,
        base_text = paste("tier", tier.roman),
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


# Link to tier tank_list with update version
md_link_update_tier_tank_list <- function(update = NULL,
                                          tier = NULL,
                                          link_text = NULL,
                                          append_text = NULL,
                                          rel_link = NULL,
                                          capital_1st = FALSE) {
    stopifnot(!is.null(tier))

    rel_link <- paste0("tanks/", rel_link)

    if (is.null(append_text)) {
        append_text <- "tanks"
    } else {
        append_text <- paste(append_text, "tanks")
    }

    return(md_link_update_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}

# Link to tier tank_list w/o update version
md_link_tier_tank_list <- function(update = NULL,
                                   tier = NULL,
                                   link_text = NULL,
                                   append_text = NULL,
                                   rel_link = NULL,
                                   capital_1st = FALSE) {
    stopifnot(!is.null(tier))

    rel_link <- paste0("tanks/", rel_link)

    if (is.null(append_text)) {
        append_text <- "tanks"
    } else {
        append_text <- paste(append_text, "tanks")
    }

    return(md_link_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}



# Link to tier premium tanks stats with update version
md_link_update_tier_premium_tank <- function(update = NULL,
                                             tier = NULL,
                                             link_text = NULL,
                                             append_text = NULL,
                                             rel_link = NULL,
                                             capital_1st = FALSE,
                                             plural = FALSE) {
    rel_link <- paste0("premium-tanks/", rel_link)

    if (is.null(append_text)) {
        append_text <- "premium tank"
    } else {
        append_text <- paste(append_text, "premium tank")
    }
    if (plural) {
        append_text <- paste0(append_text, "s")
    }

    return(md_link_update_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}

# Link to tier premium tanks stats w/o update version
md_link_tier_premium_tank <- function(update = NULL,
                                      tier = NULL,
                                      link_text = NULL,
                                      append_text = NULL,
                                      rel_link = NULL,
                                      capital_1st = FALSE,
                                      plural = FALSE) {
    rel_link <- paste0("premium-tanks/", rel_link)

    if (is.null(append_text)) {
        append_text <- "premium tank"
    } else {
        append_text <- paste(append_text, "premium tank")
    }
    if (plural) {
        append_text <- paste0(append_text, "s")
    }

    return(md_link_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


# Link to tier premium tanks stats with update version
md_link_update_tier_tank_type <- function(update = NULL,
                                          tier = NULL,
                                          tank_type = NULL,
                                          link_text = NULL,
                                          append_text = NULL,
                                          rel_link = NULL,
                                          capital_1st = FALSE,
                                          plural = FALSE) {
    stopifnot(!is.null(tank_type))
    stopifnot(tank_type %in% Tank_Types)

    tank_type <- tolower(tank_type)
    link_str <- gsub(" ", "-", tank_type, fixed = TRUE)
    rel_link <- paste0(link_str, "s/", rel_link)
    if (plural) {
        tank_type <- paste0(tank_type, "s")
    }

    if (is.null(append_text)) {
        append_text <- tank_type
    } else {
        append_text <- paste(append_text, tank_type)
    }

    return(md_link_update_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


# Link to tier premium tanks stats w/o update version
md_link_tier_tank_type <- function(update = NULL,
                                   tier = NULL,
                                   tank_type = NULL,
                                   link_text = NULL,
                                   append_text = NULL,
                                   rel_link = NULL,
                                   capital_1st = FALSE,
                                   plural = FALSE) {
    stopifnot(!is.null(tank_type))
    stopifnot(tank_type %in% Tank_Types)

    tank_type <- tolower(tank_type)
    link_str <- gsub(" ", "-", tank_type, fixed = TRUE)
    rel_link <- paste0(link_str, "s/", rel_link)
    if (plural) {
        tank_type <- paste0(tank_type, "s")
    }

    if (is.null(append_text)) {
        append_text <- tank_type
    } else {
        append_text <- paste(append_text, tank_type)
    }

    return(md_link_tier(
        update = update,
        tier = tier,
        link_text = link_text,
        append_text = append_text,
        rel_link = rel_link,
        capital_1st = capital_1st
    ))
}


md_link_tank_type <- function(tank_IDs, path = "", plural = FALSE) {
    ## used on tank-pages only
    stopifnot(exists("tanks"))

    type_str <- as.character(tanks[tank_id %in% tank_IDs][match(tank_IDs, tank_id), type])
    link_str <- paste0(path, type_str, "s")
    link_str <- tolower(gsub(" ", "-", link_str, fixed = TRUE))
    if (plural) type_str <- paste0(type_str, "s")

    return(paste0("[", type_str, "](", link_str, ")"))
}


md_alias_latest <- function(update) {
    return(paste0("aliases:\n  - /update/latest/"))
}


# ## Figure functions

# fig_col_discrete_1Y <- function(stats, x_name = NULL, y_name= NULL, by_var=NULL, fill_var=NULL, topN=topN.default) {
#   stopifnot( ! (is.null(x_name) || is.null(y_name) || is.null(by_var) || is.null(fill_var) || is.null(y_name)  ))
#   res <- head(stats[DT_filter_enough_battles.rWR(stats),
#     .(
#     "Average WR" = mean(WR),
#     "Relative WR" = mean(rWR),
#     "Player WR at Tier" = mean(WR.tier.maxed),
#     "Players" = uniqueN(account_id),
#     "Battles/Player" = mean(all.battles),
#     "Vehicle Class" = first(type),
#     "Premium" = first(is_premium)
#     ),
#     by = name
#   ][Players >= min_players.tank][order(-(y_name))], n = topN)

#   res <- prep_plot_tank(res, y_name)
#   title <- get_plot_title_top_tanks(y_name, top = TRUE, topN = topN)
#   plot_col_discrete_1Y(res, title, update, x_name, y_name, fill_var = fill_var, y_pct = TRUE, y_step = .005, top = TRUE, tier = tier)
# }
