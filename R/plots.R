### plot functions

res <- head(stats[, .('Y_NAME' = mean(get(y_col)), 'Average WR' = mean(WR), 
                      'Player WR' = mean(WR.global), Players=uniqueN(account_id), 
                      Battles = sum(all.battles), Premium= unique(is_premium)), 
                  by=name ][Players >= min_players.tank][ order(-`Y_NAME`)],n=topN);

###----------------------
res <- head(stats.update[, .('Average Damage' = mean(avg_dmg), 'tier'= mean(tier), 
                             'Average WR' = mean(WR), Players=uniqueN(account_id), 
                             Battles = sum(all.battles), Premium= unique(is_premium), 'Vehicle Class' = unique(type), 
                             'Nation' = unique(nation)), 
                         by=name ][Players >= min_players.tank][ order(-`Average Damage`)], topN)





res <- stats.update[,.(N = sum(all.battles)), by=.(Nation=nation, 'Vehicle Class'=type)][order(Nation, `Vehicle Class`)];


## rel WR per Tier WR + player population
p <- ggplot(stats.tier[ DT_filter_battles_tank(all.battles, all.battles.new, tier, is_premium) & 
                                 DT_filter_battles_tier(all.battles, battles.tier, tier) & 
                                 (tanks.tier > 1), 
                               .('Tier WR.all' = 100*WR.tier, 
                                 'Relative WR' = 100*rWR,
                                 'Global WR' = 100*WR.global,
                                 'Tank WR' = 100*WR, 
                                 'Battles' = all.battles,
                                 type), 
                               by=.(Player= account_id, Tank = name)][type == 'Tank Destroyer'], 
            aes(x=`Tier WR.all`, y = `Tank WR` - `Tier WR.all`, color = `Tank`)) + 
  geom_smooth(se = FALSE) + 
  plot_theme(x_labels.angle = 0) + 
  scale_x_continuous('Player Tier WR other', labels = div_format(seq(0,100,5),suffix = '%') , breaks =  seq(0,100,5)) + 
  scale_y_continuous('Player WR at tier IX', labels = div_format(seq(-100,100,1),suffix = '%'), breaks = seq(-100,100,1)) +
  #  scale_color_manual('Tank', values = c('KpfPz 70'='green', 'MÃ¤uschen'='grey', 'M46 Patton' = 'blue')) +
  ggtitle('Player WR at Tier IX vs Global WR', subtitle = get_subtitle(NULL));
p


