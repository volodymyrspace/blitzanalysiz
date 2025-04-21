# Init

# update Blitz Releases
force.update <- FALSE
preliminary <- FALSE
test_build <- FALSE
# build.latest_only <- FALSE
build.latest_only <- TRUE
clear.stats_in_mem <- TRUE
# build.type <- 'serial'    # nolint
# build.type <- 'parallel'
build.type <- "future"
build.version <- 3

# setwd('C:/development/dev/blitzanalysiz')
base_dir <- getwd()


# OS specific settings
OS <- Sys.info()[["sysname"]]

if (OS == "Linux") {
  options(rgl.useNULL = TRUE)
} else if (OS == "Windows") {
  #
} else {
  warning(paste("Unknown OS:", OS))
}
## Future
options(
  encoding = "UTF-8",
  future.globals.maxSize = 100e9,
  future.fork.multithreading.enable = FALSE,
  future.wait.timeout = 120,
  future.rng.onMisuse = "ignore",
  future.debug = FALSE
)

## Packages
# old <- c('showtext', 'Cairo')

pkgs.loop <- c(
  "rlist", "bit64", "lubridate", "matrixStats",
  "ggplot2", "ggtext", "ggrepel", "knitr", "blogdown", "foreach",
  "markdown", "rmarkdown", "svglite", "systemfonts", "extrafont", "qs",
  "tictoc", "stats", "graphics", "reshape2", "stringr", "gMOIP",
  "future", "data.table", "arrow", "dplyr"
)

pkgs <- c(
  pkgs.loop, "doParallel", "Hmisc", "mongolite", "progress",
  "RhpcBLASctl", "httr", "ndjson", "jsonlite", "primes"
)

# font_import(paths = "/usr/share/fonts/",prompt = F)

for (pkg in pkgs) {
  eval(parse(text = paste("suppressPackageStartupMessages(library(", pkg, "))")))
  print(paste("Loaded:", pkg, "Version:", packageVersion(pkg)))
}

# Load variables
source("R/config.R")
source("R/params.R")
# Load functions
source("R/utils_blitz.R")
source("R/utils_arrow.R")
source("R/pf.R")

n_cores <- n_cores.default
set_threads(n_threads)

# Mongo
URI <- sprintf(
  "mongodb://%s:%s@%s/?sockettimeoutms=3600000",
  db_user, db_passwd, db_host
)
ssl_opts <- NULL
if (db_tls) {
  URI <- paste0(URI, "&tls=true")
  if (OS == "Linux") {
    ssl_opts <- ssl_options(
      ca = db_ca, cert = db_cert, key = db_key,
      allow_invalid_hostname = allow_invalid_hostname,
      weak_cert_validation = weak_cert_validation
    )
  } else if (OS == "Windows") {
    ssl_opts <- ssl_options()
  } else {
    warning(paste("Unknown OS:", OS))
  }
}

# WG_tank_stats <- mongo('WG_TankStats', 'BlitzStats', URI, verbose = TRUE, options = ssl_opts);
# WG_tank_stats <- mongo('WG_TankStats', 'BlitzStats', URI,  options = ssl_opts);
# WG_releases   <- mongo('WG_Releases', 'BlitzStats', URI, options = ssl_opts);
Releases <- mongo("Releases", "BlitzStats", URI, options = ssl_opts)
Tankopedia <- mongo("Tankopedia", "BlitzStats", URI, options = ssl_opts)
# WG_Accounts   <- mongo('WG_Accounts', 'BlitzStats', URI, options = ssl_opts)

mongo_options(log_level = 2) # 2 = WARNING see ?mongo_options

updates <- get_blitz_updates()
updates.all <- get_updates(since = "6.0", preliminary)
updates.build.updates <- get_updates(since = "10.0", preliminary)
updates.build.blog <- updates.all
update.first_with_stats <- "5.4"
update.latest <- get_latest_update(preliminary)

pdf.options(encoding = "ISOLatin2.enc")


print(paste(OS, ":", cluster_type, ":", n_cores.default, " workers"))

# read Tankopedia
tanks <- get_tankopedia()

## Content defaults
update <- get_latest_update(preliminary)
updates.build <- NULL
