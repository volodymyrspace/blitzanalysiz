## utils_plot.R

## UTILS  ###########################################


signif_ceiling <- function(x, signif = 1) {
  minus = (x < 0)
  x = abs(x)
  x_res <- signif(x, digits = signif)
  x_delta = 0
  if (x_res < x) {
    x_delta <- 10^ceiling(log10(x - x_res))  
    while (signif(x_res + x_delta) < x) {
      x_delta <- 10*x_delta
    } # while 
    x_res <- x_res + x_delta
  } # if
  return(x_res * (if(minus) -1 else 1))
}


get_bin_breaks <- function(stats = NULL, tank_id = NULL, limit = 100) {
  stopifnot(! is.null(stats))
  stopifnot(! is.null(tank_id))
  tank_ID <- tank_id
  if (nrow(stats[tank_id == tank_ID]) > limit) {
        bin_breaks = seq(.25,.85, 0.05)
  } else {
        bin_breaks = seq(.25,.85, 0.1)
  }
  return(bin_breaks)
}

get_breaks.OLD <- function(min = NULL, max = NULL, x=NULL, 
                       step = NULL, steps = 10, breaks = NULL, 
                       signif = 1, f.10 = TRUE) {
  ## unless min < 0 and max > 0, The other end is forced to zero
  if (is.null(breaks)) {
    if(any(is.null(min), is.null(max))) {
      if (is.null(x)) {
        stop('min, max & x==NULL')
      } else {
        min <- min(x)
        max <- max(x)
      }
    }
    if (max > 0) {
      if (f.10) {
        x_max.10 <- 10^ceiling(log10(abs(max)))
        if (max < x_max.10/2) {
          x_max.10 <- x_max.10/2
        }
      } else {
        x_max.10 <- signif_ceiling(max, signif = signif)
      }
    } else {
      x_max.10 = 0
    }
    
    # min  
    if (min < 0) {
      if (f.10) {
        x_min.10 <- - 10^ceiling(log10(abs(min)))
        if (min > x_min.10/2) {
          x_min.10 <- x_min.10/2
        }
      } else {
        x_min.10 <- signif_ceiling(min, signif = signif)
      }
    } else {
      x_min.10 = 0
    }
    
    if ((x_max.10 > 0) && (x_min.10 < 0)) {
      x_max.10 = max(abs(c(x_min.10, x_max.10)))
      x_min.10 = -x_max.10
      if (!is.null(steps)) steps = 2*steps
    }
    
    if (is.null(step)) { 
      step <- (x_max.10 - x_min.10)/steps
    } 
    breaks <- seq(x_min.10, x_max.10, step)
  } else {
    step <- mean(diff(breaks))
  } # if breaks == NULL 
  
  return(list('breaks'= breaks, 'step' = step))
}

get_breaks <- function(x_min = NULL, x_max = NULL, x = NULL, 
                       step = NULL, steps = 10, breaks = NULL, 
                       signif = 1, f.10 = TRUE) {
  if (is.null(breaks)) { 
    if(any(is.null(x_min), is.null(x_max))) {
      if (is.null(x)) {
        stop('x_min, max & x==NULL')
      } else {
        x_min <- min(x)
        x_max <- max(x)
      }
    }
    if (is.null(steps)) {
      stopifnot(! is.null(step))
      breaks  <- pretty(c(x_min, x_max))
      steps   <- ceiling((max(breaks) - min(breaks))/step)
      breaks  <- pretty(c(x_min, x_max), n=steps)
    } else {
      breaks <- pretty(c(x_min, x_max), n=steps)
    }

  } 
  step <- mean(diff(breaks))
  
  return(list('breaks'= breaks, 'step' = step))
}


get_lims <- function(lims = NULL, breaks) {
  if (!is.null(lims) && (lims==TRUE)) {
      min_ <- min(breaks)
      max_ <- max(breaks)
      lims <- c(min_, max_)
  }
  return(lims)
}

get_axis_labels_breaks <- function(min=NULL, max=NULL, x=NULL, 
                                   step = NULL, steps = 10, breaks = NULL, 
                                   signif = 3, pct = FALSE, f.10 = TRUE) {
  
  
  tmp <- get_breaks(x_min=min, x_max=max, x=x, step=step, steps=steps, 
                    breaks=breaks, signif = signif, f.10 = f.10)
  breaks <- tmp$breaks
  step <- tmp$step
  
  if (pct) {
    if (step < .05) {
      signif <- max(4, signif)
      labels = percent_format(breaks, digits = signif)
    } else {
      signif <- max(2, signif)
      labels = percent_format(breaks, signif = signif)
    }
  } else {
    labels = auto_format(breaks, signif)
  }
  
  return(list(labels=labels, breaks= breaks, step=step))
}


get_X_axis_label_angle <- function(labels) {
  if (max(nchar(labels), na.rm = TRUE) > 3) {
    return(45)
  } else {
    return(0)
  }
}

get_topN <- function(DT = NULL, topN = NULL, top = TRUE) {
  stopifnot(! is.null(DT))
  if (! is.null(topN)) {
    if (top) {
      DT <- head(DT, n=topN)
    } else {
      DT <- tail(DT, n=topN)
    }
  } 
  return(DT)
}

prep_plot_tank <- function(DT, y_name) {
  if ('name' %in% colnames(DT)) {
    setnames(DT, 'name', 'Tank')
  }
  
  if ('Premium' %in% colnames(DT)) {
    DT[Premium==TRUE, `Tank type` := 'Premium']
    DT[Premium==FALSE,  `Tank type` := 'Researchable']
    DT[, `Tank type` := as.factor(`Tank type`)]
    DT[,Premium := NULL]
  }
  
  col.order <- c('Tank', y_name, 'Average WR', 'Player WR', 'Player WR at Tier', 'Relative WR', 'Average Damage',
                 'Players', 'Avg. Battles', 'Battles/Player', 'Battles', 'Tier', 'Vehicle Class', 'Tank type')
  col.order <- intersect(col.order, colnames(DT))
  setcolorder(DT,col.order);
  return(DT)
} 


prep_plot_tier <- function(DT, y_name) {
  col.order <- c('Tier', y_name, 'Average WR','Player WR', 'Relative WR', 'Players', 'Battles')
  col.order <- intersect(col.order, colnames(DT))
  setcolorder(DT,col.order);
  return(DT)
} 

## GET PLOT TITLE funcs ------------------------------------------


get_plot_title_top_tanks <- function(var_name, top=TRUE, tier = NULL, 
                                      topN = topN.default, 
                                      extra_txt = NULL) {
  title = ''
  tanks_by_str <- 'Tanks by'
  if (! is.null(tier)) {
    tanks_by_str <- paste('Tier', get_tier_roman(tier), tanks_by_str)
  }
  if (is.null(topN)) {
    title <- paste(tanks_by_str, var_name)
  } else {
    if (top) {
      title <- paste('Top', topN, tanks_by_str, var_name)
    }else {
      title <- paste('Bottom', topN, tanks_by_str, var_name)
    }
  }
  
  if (is.null(extra_txt)) {
    return(title)
  } else {
    return(paste0(title, ' (', extra_txt, ')')) 
  }
}


get_plot_title_top_tank_type <- function(var_name, tank_type, tier = NULL, 
                                          top=TRUE, topN = topN.default, 
                                          extra_txt = NULL) {
  title = ''
  if (! is.null(tier)) {
    tank_type <- paste('Tier', get_tier_roman(tier), tank_type)
  }
  if (is.null(topN)) {
    title <- paste0(tank_type, 's by ', var_name)
  } else {
    if (top) {
      title <- paste('Top ',topN, ' ', tank_type, 's by ', var_name)
    }else {
      title <- paste('Bottom ',topN, ' ', tank_type, 's by ', var_name)
    }
  }
  
  if (is.null(extra_txt)) {
    return(title)
  } else {
    return(paste0(title, ' (', extra_txt, ')')) 
  }
}


get_plot_title_number_of_by <- function(y_name, x_name) {
  return(paste('Number of',y_name, 'by', x_name))
}


get_plot_title_share_of_by <- function(y_name, x_name) {
  return(paste('Share of',y_name, 'by', x_name))
}


get_plot_title_subtitle <- function(title, subtitle) {
  return(paste("<span style='font-size:20pt'>", title, "</span><br>", subtitle))
}


get_plot_title_perf_by <- function(name, param, by_param) {
  return(get_plot_title_subtitle(name, paste(param, 'by', by_param)))
}


get_plot_title_share_of_by_by <- function(y_name, fill_name, x_name) {
  return(paste('Share of',y_name, 'by', fill_name, 'by', x_name))
}


get_plot_title_tank_z_is_xy <- function(tank_name, z_name, x_name, y_name) {
  #  return(paste0("<span style='font-size:20pt'>", tank_name, "</span><br>", y_name, ' + ', x_name,' vs. ', z_name))
  return(get_plot_title_subtitle(tank_name, paste(y_name, '+', x_name, 'vs.', z_name)))
}


get_subtitle <- function(update.curr=NULL, tier = NULL) {
  if (is.null(update.curr)) {
    return('')
  }
  if (is.null(tier)) { 
    return(paste("Update",update.curr))
  } else {
    return(paste("Tier", get_tier_roman(tier), "Update", update.curr))
  }
}


## PLOT HELPER funcs ###########################################


plot_theme <- function(x_labels.angle = 0, panel_top = FALSE) {
  
  if (panel_top) {
    panel_grid_minor = element_blank()
  } else {
    panel_grid_minor = NULL
  }
  t <- theme(panel.background = element_rect(fill = NA), 
             text = element_text(family = plot_font),
             legend.key = element_rect(fill = NA),
             legend.position = "top",
             axis.text.x = element_text(size=9, angle=x_labels.angle, hjust=1, vjust=1),
             axis.title.y = element_text(margin = unit(c(3, 3, 3, 3), "mm")), 
             panel.grid.major = element_line(colour = "grey"),
             panel.grid.minor = panel_grid_minor,
             panel.ontop = panel_top,
             plot.title = element_markdown(hjust = 0.5),
             plot.subtitle = element_text(hjust = 1))
  return(t)
}


plot_theme_H_grid <- function(x_labels.angle = 0, panel_top = FALSE) {
  
  t <- theme(panel.background = element_rect(fill = NA), 
             text = element_text(family = plot_font),
             legend.key=element_rect(fill = NA),
             legend.position="top", 
             axis.text.x = element_text(size=9, angle=x_labels.angle, hjust=1, vjust=1),
             axis.title.y = element_text(margin = unit(c(3, 3, 3, 3), "mm")), 
             panel.grid.major.y = element_line(colour = "grey"),
             panel.grid.major.x = NULL,
             panel.ontop = panel_top,
             plot.title = element_markdown(hjust = 0.5),
             plot.subtitle = element_text(hjust = 1))
  return(t)
}


palette_update <- function(color_palette, keys=NULL, colors=NULL, 
                           total.key=NULL, total.color = NULL) {
  color_palette <- as.list(color_palette)
  
  if ( ! is.null(colors) ) {
    if (! is.null(keys)) {
      for (ndx in seq_len(length(keys))) {
        key <- keys[[ndx]]
        if (key %in% names(color_palette)) next
        color_palette[[key]] <- colors[[ndx]]        
      }
    } else if ( ! is.null(names(colors))) {
      for (key in names(colors)) {
        if (key %in% names(color_palette)) next
        color_palette[[key]] <- colors[[key]]        
      }
    # } else {
    #   l = length(color_palette)
    #   for (ndx in length(colors)) {
    #     color_palette[[l+ndx]] <- colors[[ndx]]        
    #   }
    } 
  } else if (is.null(names(color_palette))) {
    if (length(color_palette) > length(keys)) {
      color_palette <- color_palette[seq_len(length(keys))]
    }
    names(color_palette) <- keys[seq_len(length(color_palette))]
  }
  
  if (! is.null(total.key)) {
   # if (! total.key %in% names(color_palette)) {
      color_palette[[total.key]] <- total.color
  #  }
  }
  
  return(color_palette)
}

## NOT DONE###########################################


plot_average_line <- function(res, title, update.curr, x_name, y_name, group_var, area_var, 
                              col.palette = NULL, tier = NULL) {
  res <- na.omit(res)
  ## NOT DONE
  
  # d <- density(res[,get(x_name)])
  # y2.max <- max(d)
  # 
  # scaleFactor <- res[, max(abs(get(y_name)))]/y2.max;
  # 
  # # Y axis
  # y_min <- res[ ,min(get(y_name))]
  # y_max <- res[ ,max(get(y_name))]
  # 
  # tmp <- get_axis_labels_breaks(y_min, y_max, y_pct, y_signif, y_steps, y_step)
  # labels_y <- tmp$labels
  # breaks_y <- tmp$breaks
  # # 2nd Y axis
  # y2.min <- min(append(as.list(res[, sum(get(y2_name)), by= get(x_name) ])$V1, 0))
  # y2.max <- max(append(as.list(res[, sum(get(y2_name)), by= get(x_name) ])$V1, 0))
  
  p <- ggplot(res) + geom_smooth(aes(x=get(x_name), y = get(y_name) - get(x_name), 
                                     group = get(group_var), color = get(group_var)), se = FALSE,n = 10) + 
    geom_area(aes(x = get(x_name), y = after_stat(density) * 100), stat = "bin", color = 'grey', alpha = 0.2, bins = 30) + 
    plot_theme(x_labels.angle = 0) + 
    scale_x_continuous(x_name, labels = div_format(seq(0,100,5),suffix = '%') , breaks =  seq(0,100,5)) + 
    scale_y_continuous(y_name, labels = div_format(seq(-100,100,1),suffix = '%'), breaks = seq(-100,100,1)) +
    scale_color_manual('Tank', values = col.palette) +
    ggtitle(title, subtitle = get_subtitle(update.curr, tier));
  p
}


## PLOT FUNCTIONS ###########################################


# function to plot SVGZ image with chosen image name. used in knitr fig.process=
plot_svgz <- function(filename) {
  create_svgz(filename)
  filename
}


plot_contour_discrete <- function(res, title, update.curr, x_name, y_name, z_name, weights = NULL, 
                                  col.palette = NULL, col.levels = buckets.WR, 
                                  x_step = NULL, x_steps = 20, x_breaks = NULL, x_pct = FALSE, 
                                  y_step = NULL, y_steps = 20, y_breaks = NULL, y_pct = FALSE,
                                  x_reso = 800, y_reso = 600, tier = NULL, subtitle = NULL, 
                                  max_rows = row.lim.loess) {
  
  # remove NA & duplicates
  res <- unique(na.omit(res), by= c(x_name, y_name))
  if (!is.null(max_rows) && ( nrow(res) > max_rows)) {
    res <- res[sample(.N, max_rows)]
  }
  # X
  tmp = get_axis_labels_breaks(x=res[, get(x_name)], 
                               step=x_step, steps=x_steps, breaks=x_breaks, 
                               pct = x_pct, f.10 = TRUE)
  x_breaks = tmp$breaks
  x_labels = tmp$labels
  x_min <- min(x_breaks)
  x_max <- max(x_breaks)
  
  # Y
  tmp = get_axis_labels_breaks(x=res[, get(y_name)], 
                               step=y_step, steps=y_steps, breaks=y_breaks, 
                               pct = y_pct,  f.10 = TRUE)
  y_breaks = tmp$breaks
  y_labels = tmp$labels
  y_min <- min(y_breaks)
  y_max <- max(y_breaks)
  
  ## CHANGE TO gam()?
  if (is.null(weights)) {
    mod.loess <- loess(get(z_name) ~ get(x_name) + get(y_name), data=res)
  } else {
    mod.loess <- loess(get(z_name) ~ get(x_name) + get(y_name), weights=get(weights), data=res)  
  }
  
  X <- seq(x_min, x_max, length.out = x_reso)
  Y <- seq(y_min, y_max, length.out = y_reso)
  XYZ <- as.data.table(expand.grid(X,Y))
  colnames(XYZ) <- c(x_name, y_name)
  
  ## Convex Hull
  ch <- chull(res[,.(get(x_name), get(y_name))])
  ch.p <- res[ch, .(get(x_name), get(y_name))]
  XYZ[, in_hull:= inHull(XYZ, ch.p)]
  del.idx <- XYZ[ , .I[in_hull < 0]]
  XYZ <- delete(XYZ, del.idx, opt_mem = TRUE)
  
  z_smooth  <- predict(mod.loess, newdata = XYZ)
  XYZ[ ,(z_name) := as.vector(z_smooth) ]
  #XYZ <- na.omit(XYZ)
  XYZ[ ,z_levels := cut(get(z_name), col.levels, include.lowest = TRUE) ]
  z_breaks = levels(XYZ[, unique(z_levels)])
  
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  p <- ggplot(XYZ) +
    stat_contour_filled(aes(x = get(x_name),y = get(y_name), z = get(z_name), fill = z_levels), breaks = col.levels, na.rm = TRUE) +
    theme() +
    scale_fill_manual('WR', values = col.palette, breaks = z_breaks, labels = percent_range_format(col.levels)) + 
    scale_x_continuous(name = x_name, labels = x_labels, breaks = x_breaks) + 
    scale_y_continuous(name = y_name, labels = y_labels, breaks = y_breaks) +
    plot_theme() +
    ggtitle(title, subtitle = subtitle)
  
  ## Get rid of warnings
  suppressWarnings(print(p))
}


plot_quantile_Y <- function(res, title, update.curr, 
                            x_name, y_name, fill_var = NULL, fill_elems = NULL, avg_name = NULL,  
                            binwidth = NULL, bins = NULL, bin_breaks = seq(.25,.85, 0.05), extend_bins = FALSE, 
                            x_step = NULL, x_steps = NULL, x_breaks = NULL, x_pct = TRUE, 
                            y_step = NULL, y_steps = 20, y_breaks = NULL, y_pct = FALSE, 
                            quantiles = c(0.2, 0.8), min_samples = 25, mean = FALSE,
                            palette.fill = palette.graphs, 
                            palette.annotate = palette.lines,
                            subtitle = NULL, 
                            tier = NULL) {
  
  stopifnot(any(! is.null(avg_name), ! is.null(fill_elems)))

    # remove NA rows
  res <- na.omit(res)
  
  # palette.total
  if (is.null(fill_elems)) {
    palette.total <- palette.graphs[[2]]
    palette.total.mean <- color.highlight
  } else {
    palette.total <- color.total.graph
    palette.total.mean <- color.total.line
  }
  
  q.min = quantiles[[1]]
  q.max = quantiles[[2]]
  
  # X
  if ( is.null(x_step) && is.null(x_steps) && is.null(x_breaks) ) {
    x_steps = bins
    x_step = binwidth
    x_breaks = bin_breaks
  }

  tmp = get_axis_labels_breaks(x=res[, get(x_name)], step=x_step, steps=x_steps, 
                               breaks=x_breaks, pct = x_pct, f.10 = TRUE)
  x_breaks = tmp$breaks
  x_labels = tmp$labels
  x_min <- min(x_breaks)
  x_max <- max(x_breaks)
  #x.step <- (x_max - x_min)/50
  x.step = 0
  
  if (is.null(bin_breaks)) {
    tmp <- get_breaks(x_min=x_min, x_max=x_max, step=binwidth, steps=bins, breaks=bin_breaks, signif = 2)
    bin_breaks <- tmp$breaks
  }
  if (extend_bins) {
    l = length(bin_breaks)
    bin_breaks[[1]] <- min(res[, get(x_name)])
    bin_breaks[[l]] <- max(res[, get(x_name)])
  }
  
  ## calc total average
  res.total = NULL
  res.total.mean = NULL
  
  if ( ! is.null(avg_name)) {
    if (mean) {
      res.total.mean <- res[,mean(get(y_name))]
    }
    
    res.total  <- res[, .(.N, 
                          qntl.min = quantile(get(y_name), q.min, na.rm = TRUE), 
                          qntl.max = quantile(get(y_name), q.max, na.rm = TRUE),
                          y.mean = mean(get(y_name))), 
                      by=.(x_name = cut(get(x_name), breaks=bin_breaks, include.lowest = TRUE))][order(x_name)][N > min_samples,]
    res.total[, fill_var := avg_name]
    setnames(res.total, c('fill_var', 'x_name'), c(fill_var, x_name))
  }
  
  res.fill_vars = NULL
  res.fill_vars.means = NULL
  if ( ! is.null(fill_elems)) {
    if (mean) {
      res.fill_vars.means <- res[get(fill_var) %in% fill_elems, 
                                 .(.N, y_mean = mean(get(y_name))), 
                                 by=.(fill_var = get(fill_var))][order(fill_var)][N > min_samples, y_mean]
    }
    res.fill_vars <- res[get(fill_var) %in% fill_elems, 
                         .(.N, 
                           qntl.min = quantile(get(y_name), q.min, na.rm = TRUE), 
                           qntl.max = quantile(get(y_name), q.max, na.rm = TRUE),
                           y.mean = mean(get(y_name))), 
                         by=.(fill_var = get(fill_var), 
                              x_name = cut(get(x_name),
                                           breaks=bin_breaks, 
                                           include.lowest = TRUE))][order(fill_var, x_name)][N > min_samples,]
    setnames(res.fill_vars, c('fill_var', 'x_name'), c(fill_var, x_name))
  }
  
  
  ## build plot DT
  res.means <- data.table('y_mean' = c(res.total.mean, res.fill_vars.means))
  
  res.plot <- rbindlist(list(res.total, res.fill_vars), use.names = TRUE)
  del.ndx <- res.plot[, .I[ is.na(get(x_name))] ]
  res.plot <- delete(res.plot, del.ndx);
  res.plot[, 'X' := as.integer(factor(get(x_name), levels = levels(get(x_name)) )) ]
  res.plot[, c('y_min', 'y_max', 'y.mean') := list(qntl.min, qntl.max, y.mean) ]
  res.plot[, c('x_min', 'x_max') := list(bin_breaks[X], bin_breaks[X+1])]
  
  ## Colors
  fill_vars <- res.plot[is.null(avg_name) | (get(fill_var) != avg_name), 
                        unique(get(fill_var))]
  
  palette.fill <- palette_update(palette.fill, 
                                 keys = fill_vars, 
                                 total.key = avg_name, total.color = palette.total)
  
  palette.annotate <- palette_update(palette.annotate,
                                     keys = fill_vars, 
                                     total.key = avg_name, total.color = palette.total.mean)
  
  # Y
  y_min <- min(res.plot[, qntl.min])
  y_max <- max(res.plot[, qntl.max])
  y.step <- (y_max - y_min)/20
  
  tmp = get_axis_labels_breaks(y_min, y_max, 
                               step=y_step, steps=y_steps, breaks=y_breaks, 
                               pct = y_pct, f.10 = TRUE)
  y_breaks = tmp$breaks
  y_labels = tmp$labels
  
  label_band = paste(percent_range_format(quantiles), 'band')
  
  ## Sub title
  if (is.null(subtitle)) {
    subtitle <- paste0(get_subtitle(update.curr, tier), "\n", label_band)
  } else {
    subtitle <- paste0(subtitle, "\n", label_band)
  }
  
  ## Plot
  p <- ggplot(res.plot) + 
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax= y_max, fill = get(fill_var)), 
              alpha = 0.5, linewidth = 0) +
    scale_fill_manual(fill_var, values = palette.fill)  + 
    geom_segment(aes(x = x_min, xend = x_max, y = y.mean, yend = y.mean, 
                     colour = get(fill_var)), linewidth = 1, na.rm = TRUE, show.legend = FALSE) + 
    scale_color_manual(paste('Mean', y_name), values = palette.annotate) +
    scale_x_continuous(x_name, breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(y_name, breaks = y_breaks, labels = y_labels) +
    plot_theme_H_grid() +
    ggtitle(title, subtitle = subtitle)
  
  if (mean) {
    #palette.annotate <- palette.annotate[seq(1, length(fill_vars))]
    
    p <- p + geom_hline(data = res.means, aes(yintercept=get(y_mean), colour=get(fill_var)), 
                        colour=palette.annotate, linetype="dashed", linewidth = 1, show.legend = FALSE)
    i = 1 
    for (var in fill_vars) {
      y.mean <- res.means[i, y_mean]
      if (y_pct) { 
        label_str = paste(var, 'mean', percent_format(y.mean))
      } else {
        label_str = paste(var, 'mean', auto_format(y.mean))
      }
      p <- p + annotate("text", label = label_str, 
                        x = x_min + x.step, y = y.mean + y.step, 
                        hjust = 0, vjust = 0, colour=palette.annotate[[i]])
      i <- i + 1
    }  # for
  }
  p
}


plot_average_Y <- function(res, title, update.curr, 
                           x_name, y_name, fill_var = NULL, fill_elems = NULL, avg_name = NULL,  
                           binwidth = NULL, bins = NULL, bin_breaks = seq(.25,.85, 0.05), extend_bins = FALSE, 
                           x_step = NULL, x_steps = NULL, x_breaks = NULL, x_pct = TRUE, 
                           y_step = NULL, y_steps = 20, y_breaks = NULL, 
                           y_pct = FALSE, y_mode = 'mean', y_relative=FALSE,
                           min_samples = 15, mean = FALSE,
                           palette.fill = palette.graphs, 
                           palette.annotate = palette.lines,
                           tier = NULL, subtitle = NULL) {
  # remove NA rows
  res <- na.omit(res)
  
  # palette.total
  if (is.null(fill_elems)) {
    palette.total <- palette.graphs[[2]]
    palette.total.mean <- color.highlight
  } else {
    palette.total <- color.total.graph
    palette.total.mean <- color.total.line
  }
  
  # X
  if ( all(is.null(x_step), is.null(x_steps), is.null(x_breaks)) ) {
    x_steps = bins
    x_step = binwidth
    x_breaks = bin_breaks
  }
  tmp = get_axis_labels_breaks(x=res[, get(x_name)], step=x_step, steps=x_steps, 
                               breaks=x_breaks, pct = x_pct, f.10 = TRUE)
  x_breaks = tmp$breaks
  x_labels = tmp$labels
  x_min <- min(x_breaks)
  x_max <- max(x_breaks)
  x.step = 0
  
  if (is.null(bin_breaks)) {
    tmp <- get_breaks(x_min=x_min, x_max=x_max, step=binwidth, steps=bins, breaks=bin_breaks, signif = 2)
    bin_breaks <- tmp$breaks
  }
  if (extend_bins) {
    l = length(bin_breaks)
    bin_breaks[[1]] <- min(res[, get(x_name)])
    bin_breaks[[l]] <- max(res[, get(x_name)])
  }
  
  ## calc total average
  res.total = NULL
  res.total.mean = NULL
  if ( ! is.null(avg_name)) {
    if (mean) {
      res.total.mean <- res[,.(fill_var = avg_name, y_name= mean(get(y_name)))]
    }
    res.total.sum <- 1
    if (y_relative) {
      res.total.sum <- res[,sum(get(y_name))]
    }
    
    ## TODO: Re-think the logic to handle low data cases. 
    # - Analyze the N / bucket in pieces? 1+ buckets options (as a list)
    
    res.total  <- res[, .(.N, y_name = mean(get(y_name)) / res.total.sum), 
                      by=.(x_name = cut(get(x_name), 
                                        breaks=bin_breaks, 
                                        include.lowest = TRUE))][order(x_name)][N > min_samples,]
    res.total[, fill_var := avg_name]
    setnames(res.total, c('fill_var', 'x_name', 'y_name'), c(fill_var, x_name, y_name))

  }
  
  res.fill_vars = NULL
  res.fill_vars.means = NULL
  if ( ! is.null(fill_elems)) {
    if (y_mode == 'mean') {
      res.fill_vars <- res[get(fill_var) %in% fill_elems, 
                           .(.N, y_name = mean(get(y_name))), 
                           by=.('fill_var' = get(fill_var), 
                                x_name = cut(get(x_name), 
                                             breaks=bin_breaks, 
                                             include.lowest = TRUE))][order(fill_var, x_name)][N > min_samples,]
      
    } else {
      min_samples = 0
      res.fill_vars <- res[get(fill_var) %in% fill_elems, 
                           .(y_name = sum(get(y_name))), 
                           by=.('fill_var' = get(fill_var), 
                                x_name = cut(get(x_name), 
                                             breaks=bin_breaks, 
                                             include.lowest = TRUE))][order(fill_var, x_name)]
      
      if (y_mode == 'share') {
        # y_mode == 'share'  
        res.fill_vars.totals <- res[get(fill_var) %in% fill_elems, 
                                    .(y_total = sum(get(y_name))), 
                                    by=.('fill_var' = get(fill_var))][order(fill_var)]
        
        setkey(res.fill_vars.totals, fill_var)
        setkey(res.fill_vars, fill_var)
        res.fill_vars <- merge(res.fill_vars, res.fill_vars.totals, by = "fill_var", all.x = TRUE)
        res.fill_vars[, 'y_name' := y_name / y_total]
        res.fill_vars[, 'y_total' := NULL]
        
      } # if (y_mode)
      
    }
    if (mean) {
      res.fill_vars.means <- res[get(fill_var) %in% fill_elems, 
                                 .(.N, y_name = mean(get(y_name))), 
                                 by=.(fill_var = get(fill_var))][order(fill_var)][N > min_samples, .(fill_var, y_name)]
    }
    setnames(res.fill_vars, c('fill_var', 'x_name', 'y_name'), c(fill_var, x_name, y_name))
  }
  
  
  ## build plot DT
  res.plot <- rbindlist(list(res.total, res.fill_vars), use.names = TRUE)
  del.ndx <- res.plot[, .I[ is.na(get(x_name))] ]
  res.plot <- delete(res.plot, del.ndx);
  res.plot[, 'X' := as.integer(factor(get(x_name), levels = levels(get(x_name)) )) ]
  res.plot[get(y_name) >= 0, c('y_min', 'y_max') := list(0, get(y_name)) ]
  res.plot[get(y_name) < 0, c('y_min', 'y_max') := list(get(y_name), 0) ]
  res.plot[, c('x_min', 'x_max') := list(bin_breaks[X], bin_breaks[X+1])]
  
  ## Colors
  if (is.null(avg_name)) {
    fill_vars <- res.plot[ ,as.character(unique(get(fill_var)))]
  } else {
    fill_vars <- res.plot[(get(fill_var) != avg_name), 
                          as.character(unique(get(fill_var)))]
  }
  
  palette.fill <- palette_update(palette.fill, 
                                 keys = fill_vars, 
                                 total.key = avg_name, total.color = palette.total)
  
  palette.annotate <- palette_update(palette.annotate,
                                     keys = fill_vars, 
                                     total.key = avg_name, total.color = palette.total.mean)
  
  # Y
  tmp = get_axis_labels_breaks(x=res.plot[, get(y_name)], 
                               step=y_step, steps=y_steps, 
                               breaks=y_breaks, 
                               pct = y_pct, f.10 = TRUE)
  y.step <- tmp$step
  y_breaks = tmp$breaks
  y_labels = tmp$labels
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  } 
  
  ## plot
  p <- ggplot(res.plot) + 
    geom_rect(aes(ymin = y_min, ymax= y_max, xmin = x_min, xmax = x_max, 
                  fill = get(fill_var)), alpha = 0.5, linewidth = 0) +
    scale_fill_manual(fill_var, values = palette.fill)  + 
    scale_x_continuous(x_name, breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(y_name, breaks = y_breaks, labels = y_labels) +
    plot_theme_H_grid() +
    ggtitle(title, subtitle = subtitle)
  
  if (mean) {
    # palette.annotate <- palette.annotate[seq(1, length(fill_vars))]
    res.means <- rbindlist(list(res.total.mean, res.fill_vars.means), use.names = TRUE)
    setnames(res.means, c('fill_var', 'y_name'), c(fill_var, y_name))    
    p <- p + geom_hline(data = res.means, aes(yintercept=get(y_name), colour=get(fill_var)), 
                        linetype="dashed", linewidth = 1, show.legend = FALSE) +
      scale_color_manual(fill_var, values=palette.annotate)
      
    # for (var in res.means[, as.character(unique(get(fill_var)))]) {
	for (var in res.means[, unique(get(fill_var))]) {	
      y.mean <- res.means[ get(fill_var) == var, get(y_name)]
      if (y_pct) {
        label_str = paste(var, 'mean:', percent_format(y.mean))
      } else {
        label_str = paste(var, 'mean:', auto_format(y.mean))
      }
      p <- p + annotate("text", label = label_str,
                        x = x_min + x.step, y = y.mean + y.step,
                        hjust = 0, vjust = 0, colour=palette.annotate[[var]])
    }  # for
  }
  p
}


plot_scatter <- function(res, title, update.curr, x_name, y_name, 
                         color_var = NULL, shape_var = NULL, 
                         x_breaks = NULL, x_lims = NULL, x_labels = NULL, x_step=NULL,  
                         y_breaks= NULL, y_lims = NULL, y_labels = NULL, y_step=NULL,
                         palette.colors = palette.lines[[1]],
                         x_signif = 2, y_signif = 2,
                         x_digits = 0, y_digits = 0, 
                         size = 5, 
                         x_pct = FALSE, y_pct = FALSE,
                         names = NULL,
                         averages = FALSE, smooth = NULL, formula = NULL, 
                         panel_top = FALSE, 
                         tier = NULL, subtitle = NULL, 
                         max_rows = row.lim.scatter
) {
  
  # remove NA rows
  res <- na.omit(res)
  
  if (!is.null(max_rows) && ( nrow(res) > max_rows)) {
     res <- res[sample(.N, max_rows)]
  }
  
  ## X
  tmp <- get_axis_labels_breaks(x=res[, get(x_name)], breaks=x_breaks, step=x_step, pct = x_pct)
  x_breaks <- tmp$breaks
  x_lims <- get_lims(x_lims, x_breaks)
  if (is.null(x_labels)) {
    x_labels <- tmp$labels
  }
  
  ## Y
  tmp <- get_axis_labels_breaks(x=res[, get(y_name)], breaks=y_breaks, step=y_step, pct = y_pct)
  y_breaks <- tmp$breaks
  y_lims <- get_lims(y_lims, y_breaks)
  y_step <- tmp$step
  
  if (is.null(y_labels)) {
    y_labels <- tmp$labels
  }
  
  
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  } 
  if (is.null(color_var)) {
    p <- ggplot(res, aes(x=get(x_name), y=get(y_name)) ) + 
      geom_point(na.rm = TRUE, show.legend = FALSE, colour=palette.colors, size=size) 
  } else {
    p <- ggplot(res, aes(x=get(x_name), y=get(y_name), colour=get(color_var), 
                         shape=get(shape_var))) + 
      geom_point(na.rm = TRUE, show.legend = TRUE, size=size) + 
      scale_color_manual(values=palette.colors, name=color_var) + 
      scale_shape_discrete(name=shape_var) 
  }
  if (! is.null(names)) {
    p  <- p + geom_text_repel(aes(label=get(names)))
    
  } 
  p <- p  + guides(fill="none") + 
    ## Works
    scale_x_continuous(x_name, labels = x_labels, breaks = x_breaks, limits = x_lims) +
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks, limits = y_lims)
  
  if (!is.null(smooth)) {
    p <- p + geom_smooth(method = smooth, formula = formula, na.rm = TRUE, 
                         show.legend = TRUE, colour = color.highlight,
                         se = averages)
  } 
  
  p + plot_theme(panel_top = panel_top) +  ggtitle(title, subtitle = subtitle)
}


plot_level_discrete <- function(res, title, update.curr, x_name, y_name, 
                                fill_var = NULL, shape_var=NULL, 
                                x_ordered = TRUE, x_labels = NULL,  
                                y_breaks= NULL, y_lims = NULL, y_labels = NULL, y_step=NULL,
                                # y_signif = 2,y_digits = 0, 
                                palette.colors = palette.lines[[1]], size=5, 
                                y_pct = FALSE, top=TRUE,
                                panel_top = FALSE,  
                                tier = NULL, subtitle = NULL 
) {
  
  # remove NA rows
  res <- na.omit(res)

  ## Y
  tmp <- get_axis_labels_breaks(x=res[, get(y_name)], breaks=y_breaks, step=y_step, pct = y_pct)
  y_breaks <- tmp$breaks
  if (is.null(y_labels)) {
    y_labels <- tmp$labels
  }
  
  if (is.null(x_labels)) {
    x_labels.tmp <- as.vector(res[,get(x_name)])
    x_labels.angle = get_X_axis_label_angle(x_labels.tmp)
    x_labels = waiver()
  } else {
    x_labels.angle = get_X_axis_label_angle(x_labels)
  }

  if (x_ordered) {
      aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) if (top) {-x } else {x})), 
                     y = get(y_name), 
                     colour=if (is.null(fill_var)) NULL else get(fill_var), 
                     shape=if (is.null(shape_var)) NULL else get(shape_var))
  } else {
    if (is.null(fill_var)) {
      aes_1st <- aes(x = get(x_name), y = get(y_name))
    } else {
      aes_1st <- aes(x = get(x_name), y = get(y_name), 
                     colour=if (is.null(fill_var)) NULL else get(fill_var), 
                     shape=if (is.null(shape_var)) NULL else get(shape_var)); 
    }
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  } 
  p <- ggplot(res) + aes_1st 
    
  if (! is.null(fill_var)) {
    if (! is.null(shape_var) ) { 
      p <- p + geom_point(na.rm = TRUE, show.legend = TRUE, size=size)
    } else {
      p <- p + geom_point(na.rm = TRUE, show.legend = TRUE, size=size, shape='square')
    }
    p <- p + scale_color_manual(values=palette.colors, name=fill_var)
  } else {
    if (! is.null(shape_var) ) { 
      p <- p + geom_point(na.rm = TRUE, show.legend = TRUE, size=size,
                 colour=palette.colors)
      } else {
        p <- p + geom_point(na.rm = TRUE, show.legend = FALSE, size=size,
                   colour=palette.colors, shape='square') 
    }
  }
  
  if (! is.null(shape_var) ) {
    p <- p + scale_shape(name=shape_var)
  }
  
  p <- p  + guides(fill="none") + 
    ## Works
    scale_x_discrete(x_name) +
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks, limits = y_lims)
  
  p + plot_theme_H_grid(x_labels.angle, panel_top = panel_top) +  
    ggtitle(title, subtitle = subtitle)
}



plot_average_XY <- function(res, title, update.curr, x_name, y_name, 
                            x_breaks = NULL, x_lims = NULL, x_labels = NULL,  
                            y_breaks= NULL, y_lims = NULL,y_labels = NULL, 
                            bins = 20, binwidth = NULL, z_palette = NULL,
                            x_signif = 2, y_signif = 2,
                            x_digits = 0, y_digits = 0, 
                            x_pct = FALSE, y_pct = FALSE, 
                            h_line = NULL, v_line = NULL, average=TRUE, 
                            panel_top = FALSE, 
                            tier = NULL, subtitle = NULL, 
                            max_rows = row.lim.density) {
  
  # remove NA rows
  res <- na.omit(res)

  if (!is.null(max_rows) && ( nrow(res) > max_rows)) {
    res <- res[sample(.N, max_rows)]
  }
  
  if (is.null(x_labels)) {
    if (x_pct) { 
      x_labels = percent_format(x_breaks, digits = x_digits, signif = x_signif) 
    } else {  
      x_labels = auto_format(x_breaks)
    }
  }
  
  if (is.null(y_labels)) {
    if (y_pct) { 
      y_labels = percent_format(y_breaks, digits = y_digits, signif = y_signif) 
    } else {  
      y_labels = auto_format(y_breaks) 
    }
  }
  if (! is.null(z_palette)) {
    scale_fill <- scale_fill_manual(values = z_palette)
  } else {
    scale_fill <- NULL
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  } 
  
  p <- ggplot(res, aes(x=get(x_name), y=get(y_name)) ) + 
    geom_density_2d_filled(contour_var = 'ndensity', na.rm = TRUE, show.legend = FALSE, bins = bins, binwidth = binwidth) + 
    scale_fill + guides(fill="none") + 
    ## Works
    scale_x_continuous(x_name, labels = x_labels, breaks = x_breaks, limits = x_lims) +
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks, limits = y_lims)
  
  if (! is.null(h_line)) p <- p + geom_hline(yintercept = h_line, na.rm = TRUE, aes(colour = 'black')) 
  if (! is.null(v_line)) p <- p + geom_hline(yintercept = v_line, na.rm = TRUE, aes(colour = 'black')) 
  
  if (average) { 
    p <- p + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), span = 0.5, na.rm = TRUE, 
                  show.legend = TRUE, se = FALSE, aes(colour = 'Average'))
  }
  p + scale_colour_manual(name='',breaks =c('Average'), values = c('Average'= 'blue')) +
    plot_theme(panel_top = panel_top) +
    ggtitle(title, subtitle = subtitle)
}


plot_prob_XY <- function(res, title, update.curr, x_name, line_var, 
                         x_step = NULL, x_steps = NULL, x_breaks = NULL, x_labels = NULL, 
                         y_step = NULL, y_steps = NULL, y_breaks = NULL, y_labels = NULL, 
                         line_labels = NULL,
                         x_pct = FALSE, 
                         cumf = TRUE, tier = NULL, subtitle = NULL) {
  # remove NA rows
  res <- na.omit(res)
  
  ## X
  x.tmp <- get_axis_labels_breaks(x=res[,max(get(x_name))], 
                                  step=x_step, steps=x_steps, breaks=x_breaks,
                                  pct = x_pct, signif = 2, f.10 = FALSE)
  x_breaks <- x.tmp$breaks
  if (is.null(x_labels)) { 
    x_labels <- x.tmp$labels 
  }
  ## Y
  y.tmp <- get_axis_labels_breaks(0, 1, step=y_step, steps=y_steps, breaks=y_breaks, 
                                  pct = TRUE, signif = 2, f.10 = FALSE)
  y_breaks <- y.tmp$breaks
  if (is.null(y_labels)) { 
    y_labels <- y.tmp$labels 
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  } 
  
  p <- ggplot(res, aes(x=get(x_name), colour=get(line_var)))
  if (cumf) {
    p <- p + stat_ecdf(na.rm = TRUE, geom = "step")
  } else {
    p <- p + geom_density(aes(y = after_stat(ndensity)), na.rm = TRUE) 
  }
  p <- p + scale_y_continuous("% of Players", labels = y_labels, breaks = y_breaks) 
  
  x_labels.angle = get_X_axis_label_angle(x_labels)
  
  p + scale_x_continuous(x_name, breaks = x_breaks, labels = x_labels) + 
    scale_color_discrete(line_var, labels = line_labels) + 
    plot_theme(x_labels.angle) +
    ggtitle(title, subtitle = subtitle) 
}


plot_marimekko <- function(res, title, update.curr, x_name, fill_var, x_labels = NULL, 
                           palette.fill = palette.vehicle_class, 
                           fill_labels = NULL, 
                           count = 'N', 
                           tier = NULL, subtitle = NULL) {
  
  # remove NA rows
  res <- na.omit(res)
  
  N.total = res[, sum(get(count))];
  res[, X.group := sum(get(count))/N.total, by=get(x_name)];
  res[, Z.group := get(count)/(X.group*N.total)];
  
  tmp <- res[ , .( xmax = first(X.group)), by=.(x_name = get(x_name)) ][,.(x_name, xmax = cumsum(xmax))]
  res <- merge(res, tmp, by.x=x_name, by.y='x_name')
  # old 
  # res[get(fill_var)== fill_elem, xmax:=cumsum(X.group)];
  
  res[is.na(xmax),xmax:= 0];
  res[,xmax:= max(xmax), by=get(x_name)];
  res[,xmin:= xmax-X.group];
  
  res[,ymax:=cumsum(Z.group), by=get(x_name)];
  res[,ymin:= ymax-Z.group];
  
  xaxis_pnts <- unique(res[order(xmin), xmin]);
  xaxis_pnts <- append(xaxis_pnts, 1);
  xaxis_pnts.mid_pts <- c();
  
  for (i in 1:(length(xaxis_pnts)-1)) {
    xaxis_pnts.mid_pts[i] <- mean(c(xaxis_pnts[i], xaxis_pnts[i+1]));
  }
  
  if (is.null(x_labels)) {
    x_labels <- as.character(res[, .(x_var =unique(get(x_name)))][order(x_var)][['x_var']]);
  } 
  x_labels.angle = get_X_axis_label_angle(x_labels)
  
  if (is.null(fill_labels)) {
    scale_fill <- scale_fill_manual(fill_var, values = palette.fill)
  } else {
    scale_fill <- scale_fill_manual(fill_var, guide=guide_legend(), values = palette.fill, labels = fill_labels)
    
  }
  y_breaks = seq(0,1,0.1)
  
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  ggplot(res) + 
    geom_rect(aes(ymin = ymin, ymax= ymax, xmin = xmin, xmax = xmax, fill = get(fill_var)), colour = color.line.grey, linewidth = 0.2) +
    scale_x_continuous(name= x_name, labels = x_labels, breaks = xaxis_pnts.mid_pts) + 
    scale_y_continuous(labels = percent_format(y_breaks, digits = 0), breaks = y_breaks) +
    scale_fill + 
    plot_theme(x_labels.angle) +
    ggtitle(title, subtitle = subtitle)
}


plot_col_share_discrete <- function(res, title, update.curr, x_name, fill_var,
                                    x_labels = NULL, 
                                    y_buckets = NULL, y_bucket_labels = NULL, y_bucket_pct = TRUE, 
                                    palette.fill = palette.WN8, 
                                    y_title = '% of Players', y_count = 'N', 
                                    tier = NULL, subtitle = NULL) {
  
  # remove NA rows
  res <- na.omit(res)
  
  N.total = res[, sum(get(y_count))];
  res[, Y.group := N/sum(get(y_count)), by=get(x_name)]
  
  res[,ymax:=cumsum(Y.group), by=get(x_name)]
  res[,ymin:= ymax-Y.group]
  res[, (x_name):= as.factor(get(x_name))]
  x_values <- as.character(res[, .(x_var =unique(get(x_name)))][order(x_var)][['x_var']])
  if (is.null(x_labels)) {
    x_labels <- x_values
  } else {
    x_labels <- x_labels[x_values]
  } 
  x_labels.angle = get_X_axis_label_angle(x_labels)
  
  y_breaks = seq(0,1,0.1)
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  p <- ggplot(data = remove_missing(res, na.rm = TRUE, vars = fill_var)) + 
    aes(x = get(x_name), y = Y.group, fill = get(fill_var)) + 
    geom_col(position = position_stack(reverse = TRUE), colour = color.line.grey) + 
    scale_x_discrete(x_name, breaks = factor(x_values), labels = x_labels) + 
    scale_y_continuous(y_title, labels = percent_format(y_breaks, digits = 0), breaks = y_breaks) +  
    plot_theme(x_labels.angle) +
    ggtitle(title, subtitle = subtitle)
  
  if (y_bucket_pct) {
    fill_vars <- res[, as.character(unique(get(fill_var)))]
    palette.fill <- palette_update(palette.fill, keys = fill_vars)
    p + scale_fill_manual(fill_var, values=palette.fill, guide=guide_legend(), 
                          labels = percent_range_format(y_buckets)) 
  } else {
    if (is.null(y_bucket_labels)) {
      y_bucket_labels <- y_buckets
    } 
    palette.fill <- palette_update(palette.fill, keys = y_bucket_labels)
    p + scale_fill_manual(fill_var, values=palette.fill, guide=guide_legend(reverse = TRUE), 
                          labels = y_bucket_labels) 
  }
}


plot_histogram_continuous <- function(res, title, update.curr, 
                                      x_name, y_name, fill_var = NULL, fill_elems = NULL, 
                                      avg_name = y_name,  
                                      binwidth = NULL, bins = 50, bin_breaks = NULL, extend_bins = FALSE, 
                                      x_lims = NULL, x_step = NULL, x_steps = NULL, 
                                      x_breaks = NULL, x_labels = NULL, 
                                      x_pct = FALSE, x_signif = 2, 
                                      y_step = NULL, y_steps = 10, 
                                      y_breaks = NULL, 
                                      y_pct = FALSE, y_signif = 2,
                                      mean = TRUE, median = TRUE, stacked = FALSE, 
                                      palette.fill = palette.graphs,   
                                      palette.annotate = palette.lines,
                                      tier = NULL, subtitle = NULL) {
  # remove NA rows
  res <- na.omit(res)
  
  # palette.total
  if (any(is.null(fill_var), is.null(fill_elems))) {
    palette.total <- palette.graphs[[2]]
    palette.total.mean <- color.highlight
  } else {
    # test if all the fill_elems are missing
    if (res[ get(fill_var) %in% fill_elems, .N ] == 0) {
      message(paste('ERROR: fill_elems=[', paste(fill_elems, collapse = ','), '] not found in fill_var=', fill_var ))
      return()  # stop execution of the function
    }
    palette.total <- color.total.graph
    palette.total.mean <- color.total.line
  }
  
  # X
  if ( all(is.null(x_step), is.null(x_steps), is.null(x_breaks)) ) {
    x_steps = bins
    x_step = binwidth
    x_breaks = bin_breaks
  }
  
  tmp = get_axis_labels_breaks(x=res[, get(x_name)], 
                               step=x_step, steps=x_steps, breaks=x_breaks, 
                               pct = x_pct, signif = x_signif, f.10 = TRUE)
  x_breaks = tmp$breaks
  if (is.null(x_labels)) { 
    x_labels <- tmp$labels 
  } 
  
  if (! is.null(x_lims)) {
    x_min = x_lims[[1]]
    x_max = x_lims[[2]]
    res <- subset(res, (get(x_name) >= x_min) & (get(x_name) <= x_max) )   
    ndx.keep <- ( (x_breaks >= x_min) & (x_breaks <= x_max) )             
    x_labels <- x_labels[ ndx.keep ]
    x_breaks <- x_breaks[ ndx.keep ]
  }
  
  x_min <- min(x_breaks)
  x_max <- max(x_breaks)
  x.step <- (x_max - x_min)/50
  
  tmp <- get_breaks(x_min=x_min, x_max=x_max, step=binwidth, steps=bins, breaks=bin_breaks, signif = 2)
  bin_breaks <- tmp$breaks
  
  if (extend_bins) {
    l = length(bin_breaks)
    bin_breaks[[1]] <- min(res[, get(x_name)])
    bin_breaks[[l]] <- max(res[, get(x_name)])
  }
  
  if (! is.null(x_lims)) {
    lim.min <- x_lims[[1]]
    lim.max <- x_lims[[2]]
    bin_breaks <- bin_breaks[(bin_breaks >= lim.min) & (bin_breaks<= lim.max)]
  } 
  x_min = min(bin_breaks)
  x_max = max(bin_breaks)
  res <- subset(res, (get(x_name) > x_min) & (get(x_name) < x_max))
  
  ## calc total average
  res.total = NULL
  res.total.mean = NULL
  res.total.median = NULL
  if ( ! is.null(avg_name)) {
    if (mean) {
      res.total.mean <- res[,mean(get(x_name))]
    }
    if (median) {
      res.total.median <- res[,median(get(x_name))]
    }
    palette.annotate <- c(palette.total.mean, palette.annotate)
    
    res.total  <- res[, .(N = as.double(.N)), 
                      by=.(x_name = cut(get(x_name), breaks=bin_breaks))][order(x_name)]
    res.total <- na.omit(res.total, cols = 'x_name')
    if (y_pct) {
      total = sum(res.total[, N])
      res.total[, N := N/sum(N)]  
    } 
    res.total[, 'fill_var' := as.factor(avg_name)]
    if (is.null(fill_elems)) fill_var = 'Total'
    setnames(res.total, c('fill_var', 'x_name', 'N'), c(fill_var, x_name, y_name))
    palette.fill <- c(palette.total, palette.fill)
  }
  
  res.fill_vars = NULL
  res.fill_vars.means = NULL
  res.fill_vars.medians = NULL
  if ( ! is.null(fill_elems)) {
    if (mean) {
      res.fill_vars.means <- res[ get(fill_var) %in% fill_elems, 
                                  .(x_mean = mean(get(x_name))), 
                                  by=.(fill_var = get(fill_var))][order(fill_var)][, x_mean]
    }
    if (median) {
      res.fill_vars.medians <- res[ get(fill_var) %in% fill_elems, 
                                    .(x_median = median(get(x_name))), 
                                    by=.(fill_var = get(fill_var))][order(fill_var)][, x_median]
    }
    res.fill_vars <- res[ (!is.na(x_name)) & 
                            (get(fill_var) %in% fill_elems), 
                          .(N = as.double(.N)), 
                          by=.(fill_var = get(fill_var), 
                               x_name = cut(get(x_name), breaks=bin_breaks))][order(fill_var, x_name)]
    if (y_pct) {
      if (stacked) {
        res.fill_vars[!is.na(x_name), N := N/total, by=fill_var]
      } else {
        res.fill_vars[!is.na(x_name), N := N/sum(N), by=fill_var]
      } 
    }                                       
    setnames(res.fill_vars, c('fill_var', 'x_name', 'N'), c(fill_var, x_name, y_name))
  }
  ## build plot DT
  if (mean) {
    res.means   <- data.table('x_mean' = c(res.total.mean, res.fill_vars.means))
  }
  if (median) {
    res.medians <- data.table('x_median' = c(res.total.median, res.fill_vars.medians))
  }
  
  res.plot <- rbindlist(list(res.total, res.fill_vars), use.names = TRUE)
  #res.plot[is.na(get(x_name))] ]
  #res.plot <- delete(res.plot, del.ndx);
  
  res.plot[, 'X' := as.integer(factor(get(x_name), levels = levels(get(x_name)) )) ]
  res.plot[, c('y_min', 'y_max') := list(0, get(y_name)) ] 
  res.plot[, c('x_min', 'x_max') := list(bin_breaks[X], bin_breaks[X+1])]
  
  
  fill_vars <- res.plot[, unique(get(fill_var))]
  fill_vars_sub <- fill_vars[ ! fill_vars %in% avg_name]
  names(palette.fill)       <- fill_vars
  names(palette.annotate)   <- fill_vars
  # palette.fill <- setNames(palette.fill[seq(length(fill_vars))], fill_vars)
  # palette.annotate <- setNames(palette.annotate[seq(length(fill_vars))], fill_vars)
  
  if (stacked) { 
    res.plot[ get(fill_var) == avg_name, c('y_min', 'y_max') := list(0, get(y_name)) ] 
    res.plot[ get(fill_var) %in% fill_vars_sub, c('y_min', 'y.height') := list(0, get(y_name)) ] 
    
    res.plot[ get(fill_var) %in% fill_vars_sub, 'y_max' := cumsum(y.height), by=get(x_name) ] 
    res.plot[ get(fill_var) %in% fill_vars_sub, 'y_min' := y_max - y.height, by=get(x_name) ] 
  }
  
  # Y
  y_min <- 0
  y_max <- max(res.plot[, get(y_name)])
  y.step <- (y_max - y_min)/20
  
  tmp = get_axis_labels_breaks(y_min, y_max, 
                               step=y_step, steps=y_steps, breaks=y_breaks, 
                               pct = y_pct, signif = y_signif, f.10 = TRUE)
  y_breaks = tmp$breaks
  y_labels = tmp$labels
  
  if (fill_var == 'fill_var') { 
    fill_legend = FALSE 
  } else { 
    fill_legend = TRUE 
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  ## plot
  p <- ggplot(res.plot) + 
    geom_rect(aes(ymin = `y_min`, ymax= `y_max`, xmin = `x_min`, xmax = `x_max`, 
					fill = get(fill_var)), alpha = 0.5, linewidth = 0, 
					show.legend = fill_legend, na.rm = TRUE) +
    scale_fill_manual(fill_var, values = palette.fill)  + 
    scale_x_continuous(x_name, breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(paste0(if(y_pct) {'% of '}, y_name), breaks = y_breaks, labels = y_labels) +
    plot_theme() +
    ggtitle(title, subtitle = subtitle)
  
  if (mean) {
    palette.annotate <- palette.annotate[seq(1, length(fill_vars))]
    p <- p + geom_vline(data = res.means, aes(xintercept=`x_mean`, colour=`fill_var`), 
                        colour=palette.annotate, linetype="dashed", linewidth = 1, show.legend = FALSE) 
    
    i = 1 
    for (var in fill_vars) {
      x.mean <- res.means[i, x_mean]
      hjust = 0
      if (x.mean >point_between(x_min, x_max, 2/3)) {
        step.dir = -1
        hjust = 1
      } else {
        step.dir = 1
      }
      if (var == 'fill_var') {
        var_str = ''
      } else {
        var_str = var
      }
      if (x_pct) { 
        label_str = paste(var_str, 'mean:', x_name, percent_format(x.mean, signif = x_signif))
      } else {
        label_str = paste(var_str, 'mean:', x_name, auto_format(x.mean))
      }

      p <- p + annotate("text", label = label_str, 
                    x = x.mean + step.dir * x.step, y = y_max + y.step*i, 
                    hjust = hjust, vjust = 0, colour=palette.annotate[[i]])
      i <- i + 1
    }  # for
  }
  if (median) {
    palette.annotate <- palette.annotate[seq(1, length(fill_vars))]
    
    p <- p + geom_vline(data = res.medians, aes(xintercept=`x_median`, color=`fill_var`), 
                        colour=palette.annotate, linetype="dotted", linewidth = 1, show.legend = FALSE)
    i = 1 
    for (var in fill_vars) {
      x.median <- res.medians[i, x_median]
      hjust = 0
      if (x.median > point_between(x_min, x_max, 2/3)) {
        step.dir = -1
        hjust = 1
      } else {
        step.dir = 1
      }
      if (var == 'fill_var') {
        var_str = ''
      } else {
        var_str = var
      }
      if (x_pct) { 
        label_str = paste(var_str, 'median:', x_name, percent_format(x.median, signif = x_signif))
      } else {
        label_str = paste(var_str, 'median:', x_name, auto_format(x.median))
      }
      p <- p + annotate("text", label = label_str, 
                        x = x.median + step.dir * x.step, y = y_max/2 + y.step*i, 
                        hjust = hjust, vjust = 0, colour=palette.annotate[[i]])
      i <- i + 1
    }  # for
  }
  p
}



plot_histogram <- function(res, title, update.curr, 
                           x_name, y_name, fill_var = NULL, avg_name = NULL,
                           binwidth = NULL, bins = 50, bin_breaks = NULL, 
                           x_lims = NULL, x_quantiles = NULL, 
                           x_step = NULL, x_steps = 10, x_breaks = NULL, x_labels = NULL, 
                           y_step = NULL, y_steps = 10, y_breaks = NULL, 
                           signif = NULL, x_signif = 2, y_signif = 2, relative = FALSE, 
                           x_pct= FALSE, y_pct= TRUE, mean = TRUE, median = TRUE, 
                           palette.fill = color.1st_series, 
                           palette.annotate = color.highlight,
                           tier = NULL, subtitle = NULL) {
  
  # remove NA rows
  res <- na.omit(res)
  
  if (! is.null(signif)) {
    x_signif = signif
    y_signif = signif
  }
  
  ## X 
  
  if (! is.null(x_quantiles)) {
    if (! is.null(x_quantiles[[1]])) {
      x_min <- quantile(res[, get(x_name)], x_quantiles[[1]], na.rm = TRUE)
    } else {
      x_min <- res[, min(get(x_name))]
    }
    if (! is.null(x_quantiles[[2]])) {
      x_max <- quantile(res[, get(x_name)], x_quantiles[[2]], na.rm = TRUE)
    } else {
      x_max <- res[, max(get(x_name))]
    }
  } else if (is.null(x_lims)) {
    if (is.null(bin_breaks)) {
      x_min <- res[, min(get(x_name))]
      x_max <- res[, max(get(x_name))]
    } else {
      x_min <- min(bin_breaks)
      x_max <- max(bin_breaks)
    }
  } else {
    x_min <- x_lims[1]
    x_max <- x_lims[2]
  }
  res <- subset(res, (get(x_name) >= x_min) & (get(x_name) <= x_max))
  
  bin.tmp <- get_breaks(x_min=x_min, x_max=x_max,  step=binwidth, steps=bins, breaks = bin_breaks, signif = x_signif)
  bin_breaks <- bin.tmp$breaks
  binwidth <- bin.tmp$step
  
  x.tmp <- get_axis_labels_breaks(x_min, x_max, 
                                  step=x_step, steps=x_steps, breaks=x_breaks, 
                                  pct = x_pct, signif = x_signif, f.10 = FALSE)
  x_breaks <- x.tmp$breaks
  if (is.null(x_labels)) { 
    x_labels <- x.tmp$labels 
  }

  ## y_max 
  y_max = 0
  y.scale = 1 # due to weird hist$density behavior
  if (is.null(fill_var)) {
    h <- hist(res[, get(x_name)], breaks = bin_breaks, plot = FALSE, warn.unused = FALSE)
    y_max <- max(h$counts)
    if (y_pct) {
      y.total <- sum(h$counts)
      y_max <- y_max / y.total
      if (relative) {
        y.scale <- sum(h$density)
      }
    }
  } else {
    fill_vars <- res[, unique(get(fill_var))]
    for (var in fill_vars) {
      h <- hist(res[get(fill_var) == var, get(x_name)], breaks = bin_breaks, plot = FALSE)
      if (y_pct) {
        y.total <- sum(h$counts)
        y_max <- max(h$counts/y.total, y_max)
        if (relative) {
          y.scale <- max(y.scale, sum(h$density))
        }
      } else {
        y_max <- max(h$counts, y_max)
      }# if  
    } # for
    
    if (is.null(names(palette.fill))) {
      palette.fill <- palette_update(palette.fill, keys=fill_vars)
    } 
    if (is.null(names(palette.annotate))) {
      palette.annotate <- palette_update(palette.annotate, keys=fill_vars)
    }
    
  }  # if fill_var
  
  # print(paste("y_max", y_max))  ## DEBUG
  
  y.tmp <- get_axis_labels_breaks(0, y_max, step=y_step, steps=y_steps, 
                                  breaks=y_breaks, pct = y_pct, signif = y_signif)
  y_labels <- y.tmp$labels
  y_breaks <- y.tmp$breaks
  
  if (is.null(fill_var)) {
    p <- ggplot(res, aes(x=get(x_name)))
    if (y_pct) {
      if (relative) {
        p <- p + geom_histogram(aes(y=after_stat(density)), position="identity", alpha=0.5, 
                                fill=palette.fill[[1]], colour=NA, breaks=bin_breaks, boundary = 0)
      } else {
        p <- p + geom_histogram(aes(y=after_stat(count)/sum(after_stat(count))), position="identity", alpha=0.5, 
                                fill=palette.fill[[1]], colour=NA, breaks=bin_breaks, boundary = 0)
      }
    } else {
      p <- p + geom_histogram(position="identity",alpha=0.5, 
                              fill=palette.fill[[1]],colour=NA, breaks=bin_breaks, boundary = 0)
    }
  } else { # fill_var
    p <- ggplot(res, aes(x=get(x_name), fill=get(fill_var), colour=get(fill_var), group=get(fill_var)))
    if (y_pct) {
      if (relative) {
        p <- p + geom_histogram(aes(y=after_stat(density)), position="identity", 
                                alpha=0.5, colour=NA, breaks=bin_breaks, boundary = 0)
      } else {
        p <- p + geom_histogram(aes(y=after_stat(count)/sum(after_stat(count))), position="identity", 
                                alpha=0.5, colour=NA, breaks=bin_breaks, boundary = 0)
      }
    } else {
      p <- p + geom_histogram(position="identity", 
                              alpha=0.5, colour=NA, breaks=bin_breaks, boundary = 0) 
    }
  }
  if (y_pct && relative) {  # fix weird hist bug
     y_breaks <- y_breaks * y.scale
     y_max <- y_max * y.scale
  }
  p <- p + scale_y_continuous(name = y_name, breaks = y_breaks, labels = y_labels, expansion(mult = 0, add = 0))
  
  
  if (! is.null(fill_var)) {
    p <- p +  scale_fill_manual(fill_var, values = palette.fill) + scale_color_manual(fill_var, values = palette.annotate) 
  }
  
  x.step <- (x_max - x_min)/40
  
  ## Mean
  if (mean) {
    if (is.null(fill_var)) {
      x_mean <- res[, .(Mean=mean(get(x_name)))]
      x_mean_var <- x_mean[, 'Mean'][[1]]
      hjust = 0
      x.step_var <- x.step
      if (x_mean_var > point_between(x_min, x_max, 2/3)) {
        x.step_var <- -x.step
        hjust = 1 
      }
      label_str = paste("Mean", x_name, if (x_pct) { percent_format(x_mean_var, signif = 3) } else { auto_format(x_mean_var) })
      p <- p + geom_vline(data = x_mean, aes(xintercept=`Mean`), 
                          colour=palette.annotate, linetype="dashed", linewidth = 1, 
                          show.legend = FALSE) +
        annotate("text", x = x_mean_var + x.step_var, y = y_max*1.1, hjust = hjust,
                 label = label_str, colour=palette.annotate)
    } else {
      x_mean <- res[, .(Mean=mean(get(x_name))), by=get(fill_var)]
      setnames(x_mean, 'get', fill_var)
      
      p <- p + geom_vline(data = x_mean, aes(colour=get(fill_var), xintercept=`Mean`), linetype="dashed", linewidth = 1, show.legend = FALSE)
      i = 1
      for (var in rev(fill_vars)) {
        x_mean_var <- x_mean[get(fill_var) == var, 'Mean'][[1]]
        hjust = 0
        x.step_var <- x.step
        if (x_mean_var > point_between(x_min, x_max, 2/3)) {
          x.step_var <- -x.step
          hjust = 1 
        }
        color_var <- palette.annotate[[i]]
        label_str = paste("Mean", x_name, if (x_pct) { percent_format(x_mean_var, signif = 3) } else { auto_format(x_mean_var) })
        p <- p + annotate("text", x = x_mean_var + x.step_var, y = y_max*(0.8+0.07*i), hjust = hjust, label = label_str, colour=color_var)
        i = i + 1
      } # for
    } # if fill_var
  } # if mean
  
  ## Median
  if (median) {
    if (is.null(fill_var)) {
      x_median <- res[, .(Median=median(get(x_name)))]
      x_median_var <- x_median[, 'Median'][[1]]
      hjust = 0
      x.step_var <- x.step
      if (x_median_var > point_between(x_min, x_max, 2/3)) {
        x.step_var <- -x.step
        hjust = 1 
      }
      label_str = paste("Median", x_name, if (x_pct) { percent_format(x_median_var, signif = 3) } else { auto_format(x_median_var) })
      p <- p + geom_vline(data = x_median, aes(xintercept=`Median`), colour=palette.annotate, linetype="dotted", linewidth = 1, show.legend = FALSE) +
        annotate("text", x = x_median_var + x.step_var, y = y_max*0.6, hjust = hjust,
                 label = label_str, colour=palette.annotate)
    } else {
      x_median <- res[, .(Median=median(get(x_name))), by=get(fill_var)]
      setnames(x_median, 'get', fill_var)
      
      p <- p + geom_vline(data = x_median, aes(colour=get(fill_var), xintercept=`Median`), linetype="dotted", linewidth = 1, show.legend = FALSE)
      i = 1
      for (var in rev(fill_vars)) {
        x_median_var <- x_median[get(fill_var) == var, 'Median'][[1]]
        hjust = 0
        x.step_var <- x.step
        if (x_median_var > point_between(x_min, x_max, 2/3)) {
          x.step_var <- -x.step
          hjust = 1 
        }
        color_var <- palette.annotate[[i]]
        label_str = paste("Median", x_name, if (x_pct) { percent_format(x_median_var, signif = 3) } else { auto_format(x_median_var) })
        p <- p + annotate("text", x = x_median_var + x.step_var, y = y_max*(0.5+0.05*i), hjust = hjust, label = label_str, colour=color_var)
        i = i + 1
      } # for
    } # if fill_var
  } # if median
  
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  p <- p + scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labels, expansion(mult = 0, add = 0)) + 
    plot_theme() +
    labs(x=x_name, y = y_name)+
    ggtitle(title, subtitle = subtitle)
  if (! is.null(x_lims)) {
    p <- p + coord_cartesian(xlim = x_lims)
  }
  ## Plot
  p
}


plot_col_continuous_1Y <- function(res, title, update.curr, x_name, y_name, fill_var = NULL,  
                                   y_step = NULL, y_steps = 10, y_breaks = NULL, 
                                   y_signif = 2, y_digits = NULL, y_pct = TRUE, 
                                   tier = NULL, subtitle = NULL, top = TRUE) {
  
  # remove NA rows
  res <- na.omit(res)
  
  # Y axis
  y_min <- min(append(as.list(res[, sum(get(y_name)), by= get(x_name) ])$V1, 0))
  y_max <- max(append(as.list(res[, sum(get(y_name)), by= get(x_name) ])$V1, 0))
  
  tmp <- get_axis_labels_breaks(y_min, y_max, 
                                step = y_step, steps = y_steps, breaks = y_breaks, 
                                pct = y_pct, signif = y_signif, f.10 = TRUE)
  y_labels <- tmp$labels
  y_breaks <- tmp$breaks
  
  if (top) {
    if (is.null(fill_var)) {
      aes_1st <- aes(x = get(x_name), y = get(y_name))
    } else {
      aes_1st <- aes(x = get(x_name), y = get(y_name), fill= get(fill_var)); 
    }
  } else {
    if (is.null(fill_var)) { 
      aes_1st <- aes(x = get(x_name), y = get(y_name))
    } else {
      aes_1st <- aes(x = get(x_name), y = get(y_name), fill= get(fill_var))
    }
  } # if top
  
  if (is.null(fill_var)) {
    geom_col_ <- geom_col(fill=color.1st_series)
  } else {
    geom_col_ <- geom_col(aes(group=fill_var))
  }
  
  if (! is.null(fill_var)) {
    if (fill_var == 'Tank type') {
      palette.fill <- palette.premium
    } else if (fill_var == 'Vehicle Class') {
      palette.fill <- palette.vehicle_class
    } else if (fill_var == 'Nation') {
      palette.fill <- palette.nation
    } # fi
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  p <- ggplot(res) + geom_col_ + aes_1st +
    scale_x_continuous(x_name, labels = seq(1,10), breaks = seq(1,10)) + 
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks) +
    plot_theme() +
    ggtitle(title, subtitle = subtitle);
  if (! is.null(fill_var)) { 
    p <- p + scale_fill_manual(fill_var, values = palette.fill) 
  }
  p
}


## Column plot. X = Tank
plot_col_discrete_1Y <- function(res, title, update.curr, 
                                 x_name = 'Tank', y_name, fill_var = NULL, 
                                 palette.fill = NULL, x_ordered = TRUE, 
                                 y_step = NULL, y_steps = 10, y_breaks = NULL, 
                                 x_labels = NULL, 
                                 y_signif = 3, y_pct = TRUE, 
                                 tier = NULL, subtitle = NULL, top = TRUE) {
  # remove NA rows
  res <- na.omit(res)
  
  # Y axis
  y_min <- min(append(as.list(res[, sum(get(y_name)), by= list(get(x_name)) ])$V1, 0))
  y_max <- max(append(as.list(res[, sum(get(y_name)), by= list(get(x_name)) ])$V1, 0))
  
  tmp <- get_axis_labels_breaks(y_min, y_max, step = y_step, steps = y_steps, 
                                breaks = y_breaks, signif = y_signif, pct = y_pct)
  y_labels <- tmp$labels
  y_breaks <- tmp$breaks
  
  if (top) {
    if (x_ordered) {
      if (is.null(fill_var)) {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) -x)), y = get(y_name))
      } else {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) -x)), y = get(y_name), fill= get(fill_var))
      }
    } else {
      if (is.null(fill_var)) {
        aes_1st <- aes(x = get(x_name), y = get(y_name))
      } else {
        aes_1st <- aes(x = get(x_name), y = get(y_name), fill= get(fill_var)); 
      }
    }
  } else {
    if (x_ordered) {
      if (is.null(fill_var)) {
        aes_1st <-  aes(x = reorder(get(x_name), get(y_name), (function (x) x)), y = get(y_name))
      } else {
        aes_1st <-  aes(x = reorder(get(x_name), get(y_name), (function (x) x)), y = get(y_name), fill= get(fill_var))
      }
    } else {
      if (is.null(fill_var)) { 
        aes_1st <- aes(x = get(x_name), y = get(y_name))
      } else {
        aes_1st <- aes(x = get(x_name), y = get(y_name), fill= get(fill_var))
      }
    }
  } # if top
  
  if (is.null(fill_var)) {
    geom_col_ <- geom_col(fill=color.1st_series)
  } else {
    geom_col_ <- geom_col(aes(group=fill_var))
  }
  
  if (! is.null(fill_var)) {
    if (is.null(palette.fill)) {
      if (fill_var == 'Tank type') {
        palette.fill <- palette.premium
      } else if (fill_var == 'Vehicle Class') {
        palette.fill <- palette.vehicle_class
      } else if (fill_var == 'Nation') {
        palette.fill <- palette.nation
      } # fi
    } else {
      palette.fill <- palette.fill
    }
  }
  
  if (is.null(x_labels)) {
    x_labels.tmp <- as.vector(res[, get(x_name)])
    x_labels.angle = get_X_axis_label_angle(x_labels.tmp)
    x_labels = waiver()
  } else {
    x_labels.angle = get_X_axis_label_angle(x_labels)
  }
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  p <- ggplot(res) + geom_col_ + aes_1st +
    scale_x_discrete(x_name, labels = x_labels)  + 
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks) +
    plot_theme_H_grid(x_labels.angle) +
    ggtitle(title, subtitle = subtitle);
  if (! is.null(fill_var)) { 
    p <- p + scale_fill_manual(fill_var, values = palette.fill) 
  }
  p
}


plot_col_discrete_2Y <- function(res, title, update.curr, x_name = 'Tank', y_name, y2_name, 
                                 fill_var = NULL, x_ordered = TRUE, 
                                 y_step = NULL, y_steps = 10,  y_breaks = NULL,
                                 y2_step = NULL, y2_steps = 10, y2_breaks = NULL, 
                                 y_signif = 2, y2_signif = 2,
                                 y_pct = TRUE, y2_pct = TRUE,  
                                 tier = NULL, subtitle = NULL, top = TRUE) {
  # remove NA rows
  res <- na.omit(res)
  
  scaleFactor <- res[, max(abs(get(y_name)))/max(get(y2_name))];
  # Y axis
  y_min <- min(append(as.list(res[, sum(get(y_name)), by= get(x_name) ])$V1, 0))
  y_max <- max(append(as.list(res[, sum(get(y_name)), by= get(x_name) ])$V1, 0))
  
  tmp <- get_axis_labels_breaks(y_min, y_max, step = y_step, steps = y_steps, 
                                breaks = y_breaks, pct = y_pct, signif = y_signif)
  y_labels <- tmp$labels
  y_breaks <- tmp$breaks
  
  # 2nd Y axis
  y2.min <- min(append(as.list(res[, sum(get(y2_name)), by= get(x_name) ])$V1, 0))
  y2.max <- max(append(as.list(res[, sum(get(y2_name)), by= get(x_name) ])$V1, 0))
  
  tmp <- get_axis_labels_breaks(y2.min, y2.max, step = y2_step, steps  = y2_steps, 
                                breaks = y2_breaks, pct = y2_pct, signif = y2_signif)
  y2_labels <- tmp$labels
  y2_breaks <- tmp$breaks
  
  if (x_ordered) {
    if (top) {
      if (is.null(fill_var)) {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) -x)), y = get(y_name));
      } else {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) -x)), y = get(y_name), fill= get(fill_var)); 
      }
      aes_2nd <- aes(x = reorder(get(x_name), get(y_name), (function (x) -x)), y=get(y2_name)*scaleFactor, 
                     colour = color.2nd_series, shape=y2_name)
    } else {
      if (is.null(fill_var)) {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) x)), y = get(y_name))
      } else {
        aes_1st <- aes(x = reorder(get(x_name), get(y_name), (function (x) x)), y = get(y_name), fill= get(fill_var));
      }
      aes_2nd <- aes(x = reorder(get(x_name), get(y_name), (function (x) x)), y=get(y2_name)*scaleFactor, 
                     colour = color.2nd_series, shape=yname_2)
    }
  } else {
    if (is.null(fill_var)) {
      aes_1st <- aes(x = get(x_name), y = get(y_name)); 
    } else {
      aes_1st <- aes(x = get(x_name), y = get(y_name), fill= get(fill_var)); 
    }
    aes_2nd <- aes(x = get(x_name), y = get(y2_name)*scaleFactor, colour = color.2nd_series, shape=y2_name)
  }
  
  if (is.null(fill_var)) {
    geom_col_ <- geom_col(fill=color.1st_series)
  } else {
    geom_col_ <- geom_col(aes(group=fill_var))
    if (fill_var == 'Tank type') {
      palette.fill <- palette.premium
    } else if (fill_var == 'Vehicle Class') {
      palette.fill <- palette.vehicle_class
    } else if (fill_var == 'Nation') {
      palette.fill <- palette.nation
    } # fi
  }
  x_labels.angle = get_X_axis_label_angle(as.vector(res[,get(x_name)]))
  
  ## Sub-title
  if (is.null(subtitle)) {
    subtitle <- get_subtitle(update.curr, tier)
  }
  
  p <- ggplot(res) + geom_col_ + aes_1st + 
    geom_point(aes_2nd, shape='-', size=20, inherit.aes = FALSE) + 
    scale_color_identity(name = NULL, guide='legend', labels= y2_name) + 
    scale_x_discrete(x_name) + 
    scale_y_continuous(y_name, labels = y_labels, breaks = y_breaks, 
                       sec.axis = sec_axis(~./scaleFactor, name = y2_name, 
                                           labels = y2_labels, breaks = y2_breaks)) +
    plot_theme_H_grid(x_labels.angle) +
    ggtitle(title, subtitle = subtitle);
  if (! is.null(fill_var)) { 
    p <- p + scale_fill_manual(fill_var, values = palette.fill) 
  }
  p
}


## PLOT TEMPLATES ###########################################


plot_tank_relativeWR <- function(stats = NULL, tank_id = NULL, update = NULL, 
                                  subtitle = NULL) {
 
	stopifnot(! is.null(stats))
	stopifnot(! is.null(tank_id))
	stopifnot(! is.null(update))

	perf_measure <- 'Relative WR'
	by_measure   <- 'WR at Tier'

	tier 		<- get_tank_tier(tank_id)
	tier.roman <- get_tier_roman(tier)
	tank_type <- get_tank_type(tank_id)
	tank_name <- get_tank_name(tank_id)

	res <- stats[ DT_filter_enough_battles.rWR(stats), 
														.('WR at Tier'  = WR.tier.maxed, 
															'Relative WR' = rWR,
                              'Vehicle Class'= first(type), 															
                              'Premium'      = first(is_premium)
                              ), 
														by=.(Player = account_id, Tank = name)]
  
    
	title.tier.tank_type <- paste0('Tier ', tier.roman, ' ', tank_type, 's')

	title <- get_plot_title_perf_by(tank_name, perf_measure, 'WR at Tier')
  bin_breaks <- get_bin_breaks(stats, tank_id)
  
	plot_average_Y(res, title, update, by_measure, perf_measure, 
								fill_var = 'Tank', fill_elems = tank_name, 
								bin_breaks = bin_breaks,
								x_pct = TRUE, y_step = 0.01, y_pct = TRUE,
								palette.fill = palette.vehicle_class[[tank_type]],
								palette.annotate = palette.vehicle_class.lines[[tank_type]],
								avg_name = title.tier.tank_type, 
                mean = TRUE, tier = tier, 
                subtitle = subtitle)

} 


plot_tank_avg_dmg <- function(stats = NULL, tank_id = NULL, update = NULL, 
                              subtitle = NULL) {
 
	stopifnot(! is.null(stats))
	stopifnot(! is.null(tank_id))
	stopifnot(! is.null(update))

	tier 		<- get_tank_tier(tank_id)
	tier.roman <- get_tier_roman(tier)
	tank_type <- get_tank_type(tank_id)
	tank_name <- get_tank_name(tank_id)

	perf_measure <- 'Average Damage'
	by_measure  <- 'WR at Tier'

	res <- stats[ DT_filter_enough_battles.rWR(stats), 
                          .('WR at Tier' = WR.tier.maxed,
                            'Average Damage' = avg_dmg), 
                          by=.(Player = account_id, Tank = name)]

  title.tier.avg <- paste0('Tier ', tier.roman, ' Average')

  title <- get_plot_title_perf_by(tank_name, perf_measure, 'WR')
  bin_breaks <- get_bin_breaks(stats, tank_id)
  plot_quantile_Y(res, title, update, by_measure, perf_measure, 
                fill_var ='Tank', fill_elems = tank_name, 
                bin_breaks = bin_breaks, x_pct = TRUE, y_pct = FALSE,
                palette.fill = palette.vehicle_class[[tank_type]],
                palette.annotate = palette.vehicle_class.lines[[tank_type]],
                avg_name = title.tier.avg, 
                tier = tier, 
                subtitle = subtitle)

} 


plot_tank_player_base <- function(stats = NULL, tank_id = NULL, update = NULL, 
                                  title.avg = NULL, 
                                  subtitle = NULL) {
  stopifnot(!is.null(stats))
  stopifnot(!is.null(tank_id))
  # stopifnot(!is.null(title.avg))
  stopifnot(!is.null(update))

  tank_name <- get_tank_name(tank_id)
  tierN     <- get_tank_tier(tank_id) 
  tank_type <- get_tank_type(tank_id)
  if (is.null(title.avg)) {
    title.avg <- paste('Tier', get_tier_roman(tierN), 'average')
  }

  res <- stats[ DT_filter_enough_battles.rWR(stats), 
                .('WR at Tier' = WR.tier.maxed, 
                  'Tank' = name)]

  title         <- 'Player Base Skill-level (WR at the Tier)'
  perf_measure  <- 'Players'
  by_measure    <- 'WR at Tier'

  bin_breaks <- get_bin_breaks(stats, tank_id)
  
  plot_histogram_continuous(res, title, update, 
                          by_measure, perf_measure, 
                          fill_var ='Tank', fill_elems = tank_name, 
                          x_step = 0.05,y_step = 0.02,
                          x_pct = TRUE, y_pct = TRUE, x_signif = 3, 
                          bin_breaks = bin_breaks, 
                          palette.fill = palette.vehicle_class[[tank_type]],
                          palette.annotate = palette.vehicle_class.lines[[tank_type]],
                          avg_name = title.avg, mean = FALSE, 
                          tier = tierN, 
                          subtitle = subtitle)
}


plot_tier_tank_type_relativeWR <- function(stats = NULL, tier = NULL, 
                                          tank_type = NULL, update = NULL, 
                                          fill_var = 'Tank type', 
																					top=TRUE, topN = topN.default, 
                                          subtitle = NULL) {

	stopifnot(! is.null(stats))
	stopifnot(! is.null(tier))
	#stopifnot(! is.null(tank_type))
	stopifnot(! is.null(update))

  res <- stats[ DT_filter_enough_battles.rWR(stats),  
                               .('Relative WR'  = mean(rWR),
															   'Vehicle Class'= first(type), 
															   'Players'      = uniqueN(account_id), 
                                 'Premium'      = first(is_premium)),
                               by=name ][(Players >= min_players.tank)][ order(-`Relative WR`)];
	x_name = 'Tank'
	y_name = 'Relative WR'
	res <- prep_plot_tank(res, y_name)
  
  if (is.null(tank_type)) {
    title <- get_plot_title_top_tanks(y_name, tier = tier, top = top, topN = topN)    
  } else {
	  title <- get_plot_title_top_tank_type(y_name, tank_type = tank_type, tier = tier, 
                                          top = top, topN = topN)
    res <- subset(res, `Vehicle Class` == tank_type)
  }
  res <- get_topN(res, topN, top)

	plot_col_discrete_1Y(res, title, update, x_name, y_name, 
                      fill_var = fill_var, 
											y_pct = TRUE, top = top, 
                      tier = tier, 
                      subtitle = subtitle)

}


plot_tier_tank_type_avg_dmg <- function(stats = NULL, tier = NULL, tank_type = NULL, 
																				update = NULL, fill_var = 'Tank type', 
																				top=TRUE, topN = topN.default, 
                                        subtitle = NULL) {

	stopifnot(! is.null(stats))
	stopifnot(! is.null(tier))
	stopifnot(! is.null(tank_type))
	stopifnot(! is.null(update))

	res <-  stats[ ,.('Average Damage'= mean(avg_dmg), 
																	'Average WR' = mean(WR), 
																	'Player WR at Tier' = mean(WR.tier.maxed, na.rm = TRUE), 
																	'Players' =uniqueN(account_id), 
																	'Battles/Player' = mean(all.battles),
                                  'Vehicle Class'= first(type), 
																	'Premium'= first(is_premium)), 
																by=name ][(Players >= min_players.tank)][ order(-`Average Damage`)]; 
  
  x_name = 'Tank'
	y_name = 'Average Damage'
	res <- prep_plot_tank(res, y_name)

  if (is.null(tank_type)) {
    title <- get_plot_title_top_tanks(y_name, tier = tier, top = top, topN = topN)    
  } else {
	  title <- get_plot_title_top_tank_type(y_name, tank_type = tank_type, tier = tier, 
                                          top = top, topN = topN)
    res <- subset(res, `Vehicle Class` == tank_type)
  }	
  res <- get_topN(res, topN, top)
	plot_col_discrete_1Y(res, title, update, 
                      x_name, y_name, fill_var = fill_var, 
											y_pct = FALSE, y_steps = 20, 
                      top = top, tier = tier, 
                      subtitle = subtitle)
}


plot_relativeWR <- function(stats = NULL, update = NULL, 
														fill_var = 'Tank type', 
														top = TRUE, topN = topN.default, 
                            subtitle = NULL) {
	stopifnot(! is.null(stats))
	stopifnot(! is.null(update))														
														
	res <- stats[ DT_filter_enough_battles.rWR(stats),
													.('Relative WR'  = mean(rWR),														
														'Players'           = uniqueN(account_id), 
														'Vehicle Class' 		= first(type),  
														'Premium'           = first(is_premium)),
													by=name ][Players >= min_players.tank][ order(-`Relative WR`)];
	x_name = 'Tank'
	y_name = 'Relative WR'
	y2_name = 'Players'
  res <- get_topN(res, topN, top)
	res <- prep_plot_tank(res, y_name)
	title <- get_plot_title_top_tanks(y_name, top = top, topN = topN)
	plot_col_discrete_2Y(res, title, update, 
                        x_name, y_name, y2_name, 
                        fill_var = fill_var, 
                        y2_pct = FALSE, 
                        subtitle = subtitle)
}

plot_averageWR <- function(stats = NULL, update = NULL, 
														fill_var = 'Tank type', 
														top = TRUE, topN = topN.default, 
                            subtitle = NULL) {
	stopifnot(! is.null(stats))
	stopifnot(! is.null(update))														
														
	res <- stats[ DT_filter_enough_battles(stats),
													.('Average WR'        = mean(WR),														
														'Players'           = uniqueN(account_id), 
														'Vehicle Class' 		= first(type),  
														'Premium'           = first(is_premium)),
													by=name ][Players >= min_players.tank][ order(-`Average WR`)];
	x_name = 'Tank'
	y_name = 'Average WR'
	y2_name = 'Players'
  res <- get_topN(res, topN, top)
	res <- prep_plot_tank(res, y_name)
	title <- get_plot_title_top_tanks(y_name, top = top, topN = topN)
	plot_col_discrete_2Y(res, title, update, 
                      x_name, y_name, y2_name, 
                      fill_var = fill_var, 
                      y2_pct = FALSE, 
                      subtitle = subtitle)
}


plot_avg_dmg_tier4tier <- function(stats = NULL, update = NULL, 
                                    fill_var = 'Tank type', 
                                    top = TRUE, topN = topN.default, 
                                    subtitle = NULL) {
  res <- stats[ ,.('Average Damage' = mean(avg_dmg), 
                      'Tier' = first(tier),
                      'Average WR' = DT_ratio(all.wins, all.battles),
                      'Players' = uniqueN(account_id),
                      'Battles/Player' = mean(all.battles),
                      'Premium' = first(is_premium),
                      'Vehicle Class' = first(type)),
                    by = name ][Players >= min_players.tank][order(-`Average Damage`)]

  avg_dmg.tier <- stats[ DT_filter_enough_battles.tier(stats), 
                              .('Average Damage/Tier' = DT_ratio(all.damage_dealt, all.battles)), 
                              by=.(Tier=tier)]
  setkey(res,Tier)
  setkey(avg_dmg.tier,Tier)
  res <- merge(res, avg_dmg.tier,all.x=TRUE)

  res <- res[, .(name, Tier = Tier, 
                'Damage/Tier Average %' = `Average Damage` / `Average Damage/Tier` -1, 
                `Average Damage`,
                `Average WR`, 
                Players, 
                `Vehicle Class`, 
                `Premium`) ][order(-`Damage/Tier Average %`)];
  
  x_name = 'Tank'
  y_name = 'Damage/Tier Average %'
  y2_name = 'Players'

  if (top) {
    res <- head(res, n=topN)    
  } else {
    res <- tail(res, n=topN)    
  }
  res <- prep_plot_tank(res, y_name) 
  title <- get_plot_title_top_tanks('Average Damage (tier-for-tier)', top = top, topN = topN)
  plot_col_discrete_2Y(res, title, update, 
                        x_name, y_name, y2_name, 
                        fill_var = fill_var, 
                        y2_pct = FALSE, 
                        subtitle = subtitle)
}
