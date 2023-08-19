library("arrow")
library('dplyr')
stats <- open_dataset('9.5', partitioning = c("region"))

stats %>% 
   filter(region == 'eu' & tank_id == 1) %>%
   select(account_id, all.battles) %>% 
   collect()
   
   
stats %>% 
	select(tank_id, all.battles, all.wins) %>%
	group_by(tank_id) %>%
	summarize( avg_wr = sum(all.wins) / sum(all.battles), N = n()) %>%
	arrange(desc(avg_wr)) %>%
	collect()