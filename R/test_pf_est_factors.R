## performance factor test for random starting values ==> converges 100%

rows <- dim(btls.mtrx)[1]
cols <- dim(btls.mtrx)[2]

res <- list()
res[[1]] <- pf_est_factors(btls.mtrx, res.mtrx, delta_MSE = 0, rounds = 100)

rounds = 5

for (i in seq_len(rounds)) {
  # set random values
  tank.perf.in <- rnorm(cols, 1, 0.25)
  player.skill.in <- rnorm(rows, 0.5, .1)
  # estimate factors
  res[[i+1]] <- pf_est_factors(btls.mtrx, res.mtrx, 
                                    player.skill = player.skill.in, tank.perf = tank.perf.in, 
                                    delta_MSE = 0, rounds = 100)
}

res.tank_perf <- as.data.table(list('auto'=res[[1]]$tank.perf))
for (i in seq_len(rounds)) {
  res.tank_perf[, (paste0('rand', i)) := res[[i+1]]$tank.perf]
}

colSums(res.tank_perf)
###################################################

stats.8.0 <- stats.8.0[DT_filter_sub5k.out(stats.8.0)]

## Analysis of required iterations #############################################

dat.iter <- list()
res.iter <- list()

stats.tier8 <- stats.8.0[tier == 8]

dat.cv <- pf_prep(stats.tier8)
res.cv <- pf_est_factors(dat.cv, mode="best", debug.convergence = 20)

tank.perf.cv <- list()
cv.rank <- data.table("Rank"= seq_len(20))
cv.factors <- data.table("Rank"= seq_len(20))
for (iter in names(res.cv)) {
  tank.perf.cv[[iter]] <- get_tank_perf(dat.cv, res.cv[[iter]], topN = 20)
  cv.rank[[iter]] <- tank.perf.cv[[iter]]$tank_names
  cv.factors[[iter]] <- tank.perf.cv[[iter]]$tank.perf
  print(paste('Tier VIII: Iteration: ', iter))
  print(tank.perf.cv[[iter]])
  print("")
}

## Analysis of tiers  ##########################################################

dat.tiers <- list()
res.tiers <- list()

for (tierN in seq(6, 10)) {
  tier.str <- as.character(tierN)
  cat(paste("\nTier:", tiers.roman[[tierN]], "\n\n"))
  dat.tiers[[tier.str]] <- pf_prep(stats.8.0[tier == tierN])
  res.tiers[[tier.str]] <- pf_est_factors(dat.tiers[[tier.str]], mode="best")
}

tank.perf.tiers <- list()
player.skill.tiers <- list()

for (tierN in seq(6, 10)) {
  tank.perf.tiers[[tier.str]]    <- get_tank_perf(dat.tiers[[tier.str]], res.tiers[[tier.str]])
  player.skill.tiers[[tier.str]] <- get_player_skill(dat.tiers[[tier.str]], res.tiers[[tier.str]], min.battles = 100)
}


for (tierN in seq(6, 10)) {
  tier.str <- as.character(tierN)
  cat(paste("\nTier:", tiers.roman[[tierN]], "Top 20\n\n"))
  print(get_tank_perf(dat.tiers[[tier.str]], res.tiers[[tier.str]], topN = 20))
  
  cat(paste("\nTier:", tiers.roman[[tierN]], "Bottom 20\n\n"))
  print(get_tank_perf(dat.tiers[[tier.str]], res.tiers[[tier.str]], topN = -20))
}

## EU TOP 30
tier.str = "10"

N = 100
player.skill.t10 <- get_player_skill(dat.tiers[[tier.str]], res.tiers[[tier.str]], 
                                     min.battles = 100, topN = N, only_region="eu" )
nicknames <- get_account_nicknames(player.skill.t10$account_id)
player.skill.t10EU <- DT_merge_nicknames(player.skill.t10, nicknames)[order(-player.skill)]

