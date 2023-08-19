## mem test

## load data

rm(stats)
rm(stats2)
stats <- load_tank_stats_period('7.6','8.0', do_rbind = TRUE)
stats2 <- load_tank_stats_period('6.6','7.0', do_rbind = TRUE)

stats2 <- rbindlist(list(stats, stats2), use.names = TRUE, fill=TRUE)
rm(stats)

N <- stats2[, .N]
del.ndx <- sample.int(N, N %/% 5)

Sys.sleep(1)
stats <- copy(stats2)
gc()
Sys.sleep(1)

tic('delete.OLD()');
stats <- delete.OLD(stats, del.ndx)
toc_()

Sys.sleep(1)
stats <- copy(stats2)
gc()
Sys.sleep(1)

tic('delete.2()');
stats <- delete2(stats, del.ndx)
toc_()

Sys.sleep(1)
stats <- copy(stats2)
gc()
Sys.sleep(1)

tic('delete()');
stats <- delete(stats, del.ndx)
toc_()
