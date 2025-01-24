## Constants --------------------------------------------------


## WG API
WG_API_base <- "https://api.wotblitz."


pcs10.labels <- c("90%+", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", "0%")
pcsWN8.labels <- c("99.99%", "99.9%", "99%", "95%", "82%", "63%", "40%", "20%", "6%", "0%")
pcsWN8.pcs <- c(0, 0.06, 0.2, 0.4, 0.63, 0.82, 0.95, 0.99, 0.999, 0.9999, 1)

buckets.WR <- c(0, .30, .35, .40, .45, .50, .55, .60, .65, .7, 1)
buckets.WR.narrow <- c(0, .10, .20, .30, .35, .40, .45, .50, .55, .60, .65, .70, .80, .90, 1)

buckets.WR.10 <- c(0, .30, .40, .50, .60, .70, 1)
buckets.WR.10.narrow <- seq(0, 1, .10)
buckets.battles <- c(0, 10, 25, 50, 100, 250, 500, 750, 1000, 1500, 1e8)
buckets.battles.career <- c(0, 500, 1000, 2500, 5e3, 7500, 10e3, 15e3, 25e3, 50e3, 1e8)


tank_types <- c("lightTank", "mediumTank", "heavyTank", "AT-SPG")
Tank_Types <- c("Light Tank", "Medium Tank", "Heavy Tank", "Tank Destroyer")

tiers.roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")

nations <- c("ussr", "germany", "usa", "china", "france", "uk", "japan", "other", "europe")
Nations <- c("USSR", "Germany", "USA", "China", "France", "UK", "Japan", "Other", "Europe")
Nations.str <- c(
  "Soviet", "German", "American", "Chinese", "French",
  "British", "Japanese", "Hybrid Nation", "European"
)

nations_db <- data.table(nation = nations, Nation = Nations)

regions <- c("eu" = "EU", "ru" = "Russia", "com" = "North America", "asia" = "Asia", "china" = "China")
# Regions <- c("EU",  "Russia", "North America",  "Asia", "China")

regions <- regions[1:4]

regions_account_ids <- list("ru" = c(1, 5e8), "eu" = c(5e8, 10e8), "com" = c(10e8, 20e8), "asia" = c(20e8, 31e8))

# The palette with grey:
palette.cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette.jp <- c(
  "#23469D", "#627CBC", "#3F5CA5", "#14327E", "#0A2363", "#E11E31", "#F87380", "#EE4959",
  "#B50B1B", "#8F000E", "#159C6D", "#57BC98", "#33A57D", "#087E54", "#006340", "#E9AF1F",
  "#FFD876", "#F6C54C", "#BB890B", "#946900"
)

palette.WN8 <- rev(c(
  "#6F319E", "#A61A95", "#3972C6", "#4099BF", "#4D7326",
  "#849B24", "#CECD3B", "#FFBF01", "#C20000", "#000000"
))
# palette.WN8.11 <- c("#6F319E", "#A61A95", "#3972C6", "#4099BF", "#4D7326", "#849B24", "#CECD3B", "#FFBF01", "#C20000", "#000000", "#000000")

# palette.graphs <- c('#ACD8F1', '#7BABC4', '#497D97', '#EBE5AD', '#949556', '#ACDE9C', '#60AB54', '#DF6868', '#D43535' , '#BDBDBD', '#8D8D8D', '#5C5C5C')
## palette.graphs <- c('#ACD8F1', '#7BABC4', '#497D97', '#EBE5AD', '#949556', '#ACDE9C', '#60AB54', '#DF6868', '#D43535' , '#BDBDBD')
palette.graphs <- c(
  "#ACD8F1", "#7BABC4", "#497D97", "#EBE5AD", "#949556",
  "#ACDE9C", "#60AB54", "#BDBDBD", "#8D8D8D", "#5C5C5C"
)
## palette.lines <- c('#6EB7E0', '#568FAE', '#3E6578', '#D8CE72', '#767747', '#81C86A', '#508747', '#CE3B3B', '#AC2B2B', '#727272')
palette.lines <- c(
  "#80C3E9", "#629AB8", "#406E86", "#E1D883", "#80814A",
  "#8ED175", "#549549", "#A6A6A6", "#797979", "#525252"
)

shapes <- c("circle", "square", "triangle", "diamond", "plus", "cross", "asterisk")

# palette.nation <- palette.graphs[1:length(nations)]
# names(palette.nation) <- Nations

palette.eu.blue <- c("#003399")
palette.france.blue <- c("#318CE7")
palette.italy.green <- c("#008C45")
palette.ussr.red <- c("#CD362A")
palette.china.yellow <- c("#FCE300")

palette.nation <- list(
  "China" = palette.china.yellow, "Europe" = palette.eu.blue,
  "France" = palette.france.blue, "Germany" = "black",
  "Japan" = "white", "UK" = palette.graphs[[3]],
  "USA" = palette.graphs[[2]], "USSR" = palette.ussr.red,
  "Other" = palette.graphs[[5]]
)

palette.vehicle_class <- c(
  "Light Tank" = palette.graphs[1], "Medium Tank" = palette.graphs[2],
  "Heavy Tank" = palette.graphs[3], "Tank Destroyer" = palette.graphs[7]
)

palette.vehicle_class.lines <- c(
  "Light Tank" = palette.lines[1], "Medium Tank" = palette.lines[2],
  "Heavy Tank" = palette.lines[3], "Tank Destroyer" = palette.lines[7]
)

palette.premium <- c("Premium" = palette.lines[4], "Researchable" = palette.lines[3])


palette.region <- c(
  "eu" = palette.graphs[2], "ru" = palette.graphs[9],
  "com" = palette.graphs[6], "asia" = palette.graphs[5], "china" = palette.graphs[4]
)

palette.results <- c(
  "Win & Survived" = palette.graphs[7], "Win & Destroyed" = palette.graphs[6],
  "Lost & Destroyed" = palette.graphs[9], "Lost & Survived" = palette.graphs[8],
  "Draw" = palette.graphs[5]
)

color.1st_series <- palette.graphs[[2]]
color.2nd_series <- "#A61A95"
color.highlight <- color.2nd_series
color.line.grey <- "grey90"
color.total.graph <- palette.graphs[[8]]
color.total.line <- palette.lines[[8]]

## Columns
cols.ratio <- c("Spot Rate", "KDR", "Damage Ratio", "Hits/Battle", "Average Kills/Battle")
cols.pct <- c("%", "WR", "Hit Rate")
cols.int <- c("Battles", "Players", "Battles/Player", "Average Damage")

xp.grind <- list(
  "1" = 0, "2" = 1350, "3" = 3800, "4" = 9150, "5" = 18000,
  "6" = 41500, "7" = 69500, "8" = 125000, "9" = 227500, "10" = 270000
)

xp.median <- list(
  "1" = 309.3, "2" = 338.5, "3" = 342.8, "4" = 412.7, "5" = 435.4,
  "6" = 527.3, "7" = 595.7, "8" = 642.5, "9" = 727.0, "10" = 786.0
)

min_battles.grinds <- floor(unlist(Map("/", xp.grind, xp.median)) / 1.5^2)
min_battles.tanks <- c(10, 10, 15, 15, rep(20, 6))
min_battles.tiers <- 2 * min_battles.tanks

topN.default <- 20
topN <- topN.default

min_players.tier <- 400
min_players.tank <- 200

min_battles.tank <- 30
min_battles.tier <- 50
min_battles.update <- 100
min_battles.career <- 500
min_battles.career.5k_mm <- 5e3

# sample sizes
samples_tank <- 15e3
row.lim.loess <- 5e3
row.lim.density <- 100e3
row.lim.scatter <- 10e3
row.lim.split <- 50e6
row.lim.generate <- 50e6
row.lim.use <- 30e6

# row.lim.cores   <- 60e6
row.lim.cores <- 80e6
DELETE_PAGE <- "__DELETE_PAGE__"


# Parallel processing
OS <- Sys.info()[["sysname"]]
# options(future.globals.maxSize= 100e9, future.wait.timeout = 30, future.debug = FALSE)

# n_cores.max <- n.cores <- parallel::detectCores() - 1
n_cores.max <- 16
n_cores.min <- 2

if (OS == "Linux") {
  pkgs.loop <- NULL
  ex.vars.1 <- NULL
  ex.vars.2 <- NULL
  ex.vars.main <- TRUE
  ex.vars.tiers <- TRUE
  ex.vars.future <- c("stats.tier")
  plot_font <- "Open Sans Light"
  # plot_font <- 'Open Sans'
  # font_add_google('sans')
  # showtext_auto()
  # Cairo::CairoFonts(regular="Open Sans:style=Light",
  #                   bold="Open Sans:style=Semibold",
  #                   italic="Open Sans:style=LightItalic",
  #                   bolditalic="Open Sans:style=SemiboldItalic,Semibold Italic")

  use.parallel <- TRUE
  cluster_type <- "FORK"
  n_cores.default <- 16
  n_threads.default <- 32
} else if (OS == "Windows") {
  ex.vars.1 <- c("page_weight.update", "stats.update", "updates", "tiers", "update")
  ex.vars.2 <- c("updates", "tiers")
  ex.vars.main <- c("updates")
  ex.vars.tier <- c("stats.tier", "updates")
  ex.vars.future <- c("stats.update")

  use.parallel.1 <- TRUE
  use.parallel.2 <- TRUE
  use.parallel <- TRUE
  cluster_type <- "PSOCK"
  n_cores.default <- 4
  n_threads.default <- 32

  plot_font <- "Open Sans Light"
} else {
  warning(paste("Unknown OS:", OS))
}

n_threads <- n_threads.default

OS <- Sys.info()[["sysname"]]
snow_options <- NULL

vars.stats <- c("stats.career", "stats.list", "stats.update", "stats.tier", "stats.tier.type")
