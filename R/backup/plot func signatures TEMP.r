get_breaks <- function(min, max, step = NULL, steps = 10, breaks = NULL, signif = 1, f.10 = TRUE) 

get_axis_labels_breaks <- function(min, max, step = NULL, steps = 10, breaks = NULL, 
									pct = FALSE, signif = 1, f.10 = TRUE)
									
plot_histogram <- function(res, title, update.curr, x_name, y_name, fill_var = NULL, 
                           palette.fill = color.1st_series, palette.lines = color.highlight,
                           bins = 50, binwidth = NULL, bin.breaks = NULL, x_lims = NULL, 
                           x_step = NULL, x_steps = 10, x_breaks = NULL, 
                           y_step = NULL, y_steps = 10, y_breaks = NULL, 
                           signif = NULL, x_signif = 1, y_signif = 1,
                           x_pct= FALSE, y_pct= TRUE, mean = FALSE, median = FALSE, tier = NULL)
						   
plot_average_XY <- function(res, title, update.curr, x_name, y_name, 
							x_breaks = NULL, y_breaks= NULL, 
                            x_labels = NULL, y_labels = NULL, 
							binwidth = NULL, bins = 20, 
							x_pct = FALSE, y_pct = FALSE, 
                            x_lims = NULL, y_lims = NULL, 
							x_accuracy = 5, y_accuracy = 5, 
                            h_line = NULL, v_line = NULL,  panel_top = FALSE, tier = NULL)
							
plot_prob_XY <- function(res, title, update.curr, x_name, line_var, 
                         x_breaks, x_labels, cumf = TRUE, tier = NULL) 


plot_marimekko <- function(res, title, update.curr, x_name, fill_var, fill_elem, x_labels = NULL, 
                           palette.fill = palette.vehicle_class, fill_labels = NULL, count = 'N', tier = NULL) 

						   
plot_col_share_discrete <- function(res, title, update.curr, x_name, y_name, 
                                    y_buckets = buckets.WR, color.palette = palette.WN8, 
                                    y_title = '% of Players', y_count = 'N', tier = NULL)
									

plot_col_continuous_1Y <- function(res, title, update.curr, x_name, y_name, fill_var = NULL,  
                                 y_step = NULL, y_steps = 10, y_breaks = NULL, 
                                 y_signif = 1, y_digits = NULL, y_pct = TRUE, tier = NULL, top = TRUE)
								 

plot_col_discrete_1Y <- function(res, title, update.curr, x_name, y_name, fill_var = NULL, palette.fill = NULL, x_ordered = TRUE, 
                                 y_step = NULL, y_steps = 10, y_breaks = NULL, 
                                 y_signif = 1, y_pct = TRUE, tier = NULL, top = TRUE)


plot_col_discrete_2Y <- function(res, title, update.curr, x_name, y_name, y2_name, fill_var = NULL, x_ordered = TRUE, 
                                 y_step = NULL, y_steps = 10,  y_breaks = NULL,
                                 y2_step = NULL, y2_steps = 10, y2_breaks_2 = NULL, 
								 y_signif = 1, y2_signif = 1, 
                                 y_pct = TRUE, y2_pct = TRUE, 
                                 tier = NULL, top = TRUE)
