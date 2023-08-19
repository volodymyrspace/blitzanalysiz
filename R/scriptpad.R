
# WR by tier
stats.career[all.battles > 0, 
             .(WR = sum(all.wins)/sum(all.battles)), 
             by=.(tier, account_id)][,.(WR = mean(WR), N=.N), 
                                     by=tier][order(tier)]


res <- stats.tier[DT_filter_battles_tank(all.battles, all.battles.new, tier, is_premium) & 
                           DT_filter_battles_tier(all.battles, battles.tier, tier) & 
                           (tanks.tier > 1)
                           , .(WR.tier2 = sum(all.wins)/sum(all.battles), 
                               battles.tier2 = sum(all.battles)),
                               by=.(account_id)]

res <- res[, .('Average WR' = mean(WR),
               'Average Dmg' = mean(avg_dmg),
               'Relative WR' = mean(WR - (WR.tier2*battles.tier2 - all.wins)/(battles.tier2 - all.battles)),
               'Player WR' = mean(WR.tier), 
               'Players'=uniqueN(account_id),
               'Battles' = sum(all.battles)/uniqueN(account_id), 
               'Vehicle Class'=first(type), 
               'Premium'= first(is_premium)),
           by=name ][Players >= min_players.tank][ order(-`Relative WR`)]
x_name = 'Tank'
y_name = 'Relative WR'
res <- prep_plot_tank(res, y_name)
title <- get_plot_title_top_tanks(y_name, top=TRUE, topN = 20)
plot_col_discrete_1Y(res, title, x_name, y_name, fill_var = 'Vehicle Class', y_pct = TRUE, top = TRUE, tier = tier)

kable(res, digits = 1, format= 'markdown');

tier = 9
stats.tier <- subset(stats.update, tier == 9)

res <- stats.tier[ (tanks.tier > 1) & (WR.tier <= .55) & (WR.tier >= .45) &
                                 DT_filter_battles_tank(all.battles, all.battles.new, tier, is_premium),  
                               .('Average WR'   = mean(WR), 
                                 'Relative WR'  = mean(rWR),
                                 'Player WR'    = mean(WR.tier), 
                                 'Players'      = uniqueN(account_id),
                                 'Avg. Battles' = sum(all.battles)/uniqueN(account_id), 
                                 'Vehicle Class'= first(type), 
                                 'Premium'      = first(is_premium)),
                               by=name ][Players >= min_players.tank][ order(-`Relative WR`)]
x_name = 'Tank'
y_name = 'Relative WR'
res <- prep_plot_tank(res, y_name)
title <- get_plot_title_top_tanks(y_name, top=TRUE, topN = NULL, extra_txt = 'WR at tier 45-55%')
plot_col_discrete_1Y(res, title, x_name, y_name, fill_var = 'Vehicle Class', y_pct = TRUE, top = TRUE, tier = tier)


res <- stats.tier[ DT_filter_tank_is_maxed2(all.battles.new, tier, is_premium) &
                                 DT_filter_battles_tank(all.battles, tier) & 
                                 DT_filter_battles_tier(battles.tier, tier) &
                                 (tanks.tier > 1),  
                               .(WR, 
                                 'Relative WR'  = WR - WR.tier,
                                 WR.tier, 
                                 account_id,
                                 all.battles, 
                                 type, 
                                 is_premium, Tank = name) ]

res <- stats.tier[ DT_filter_tank_is_maxed2(all.battles.new, tier, is_premium) &
                                 DT_filter_battles_tank(all.battles, tier) & 
                                 DT_filter_battles_tier(battles.tier, tier) &
                                 (tanks.tier > 1) & (all.battles > 50),  
                               .('Average WR'   = percent_format(mean(WR), digits = 2), 
                                 'Relative WR SD' = percent_format(sd(WR-WR.tier, na.rm = TRUE), digits = 2),
                                 'Relative WR MEAN'  = percent_format(mean(WR - WR.tier), digits = 2),
                                 'Relative WR MEDIAN'  = percent_format(median(WR - WR.tier), digits = 2),
                                 'Relative WR MAD' = percent_format(mad(WR-WR.tier, na.rm = TRUE), digits = 2),
                                 'Player WR at Tier'    = percent_format(mean(WR.tier), digits = 2), 
                                 'Players'      = uniqueN(account_id),
                                 'Avg. Battles' = sum(all.battles)/uniqueN(account_id), 
                                 'Vehicle Class'= first(type), 
                                 'Premium'      = first(is_premium)), 
                          by=.(Tank = name)][Players >= min_players.tank][ order(-`Relative WR MEAN`) ]



# -----------------------------------------------------

# Plot of battle results vs. WR

x_name = 'WR at Tier'
y_name = '% of Battles'

res <- list()
results <- names(palette.results)

res[['win_survived']] <- stats.tier[ DT_filter_enough_battles.tier(stats.tier), 
                                            .('% of Battles' = survival_win_rate,
                                              'Result' = as.factor(results[[1]])
                                            ),
                                            by = .('WR at Tier' = cut(WR.tier.maxed, buckets.WR)) 
]

res[['win_dead']] <- stats.tier[ DT_filter_enough_battles.tier(stats.tier), 
                                        .('% of Battles' = WR - survival_win_rate,
                                          'Result' = as.factor(results[[2]])
                                        ),
                                        by = .('WR at Tier' = cut(WR.tier.maxed, buckets.WR)) 
]

res[['lost_dead']] <- stats.tier[ DT_filter_enough_battles.tier(stats.tier), 
                                         .('% of Battles' = (all.losses - (all.survived_battles - all.win_and_survived))/all.battles,
                                           'Result' = as.factor(results[[3]])
                                         ),
                                         by = .('WR at Tier' = cut(WR.tier.maxed, buckets.WR)) 
]

res[['lost_survived']] <- stats.tier[ DT_filter_enough_battles.tier(stats.tier), 
                                             .('% of Battles' = (all.survived_battles - all.win_and_survived)/all.battles,
                                               'Result' = as.factor(results[[4]])
                                             ),
                                             by = .('WR at Tier' = cut(WR.tier.maxed, buckets.WR)) 
]

res[['draw']] <- stats.tier[ DT_filter_enough_battles.tier(stats.tier),  
                                    .('% of Battles' = (all.battles - all.wins - all.losses)  / all.battles, 
                                      'Result' = as.factor(results[[5]])
                                    ),
                                    by = .('WR at Tier' = cut(WR.tier.maxed, buckets.WR )) 
]

res <- rbindlist(res, use.names = TRUE, fill=TRUE)

res.plot <- na.omit(res[, .('% of Battles' = mean(`% of Battles`)), by=.(Result, `WR at Tier` )])

title <- 'Battles outcomes by WR'

plot_col_discrete_1Y(res.plot, title, update, 
                     x_name, y_name, fill_var = 'Result', palette.fill = palette.results, 
                     x_ordered = FALSE, 
                     y_step = 0.1, 
                     x_labels = percent_range_format(buckets.WR), 
                     y_signif = 2, y_pct = TRUE, tier = tierN, top = TRUE)


### --------------------------------------------------------------------                               
