## utils.R


point_between <- function(a, b, share = 0.5) {
  return( a + share * (b-a))
}



# Eval
EVAL <- function(...) eval(parse(text=paste0(...)),envir=parent.frame(2))


# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    # stopCluster(parallelCluster) # package: `parallel`
  })
})


# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

## List with FAST appends
linkedList <- function() {
  head <- list(0)
  length <- 0
  
  methods <- list()
  
  methods$add <- function(val) {
    length <<- length + 1
    head <<- list(head, val)
  }
  
  methods$last <- function() {
    return(head[[2]])
  }
  
  methods$as.list <- function() {
    b <- vector('list', length)
    h <- head
    for(i in length:1) {
      b[[i]] <- head[[2]]
      head <- head[[1]]
    }
    return(b)
  }
  methods
}


## Named list with FAST appends  
namedExpandingList <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  names <- character(capacity)
  length <- 0
  
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    names <<- c(names, character(capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(name, val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
    names[length] <<- name
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    names(b) <- names[0:length]
    return(b)
  }
  
  methods
}


now <- function() {
  # return epoch time (UTC)
  return(as.integer(Sys.time()))
}


toc.fn <- function(tic, toc, msg) {
  elapsed <- toc - tic
  elapsed.str <- NULL
  if (elapsed >= 24*3600) {
    elapsed.str <- paste(elapsed %/% (24*3600), "d")
  }
  if (elapsed >= 3600) {
    elapsed.str <- paste(elapsed.str, elapsed %/% 3600 %% 24, "h")
  }
  if (elapsed >= 60) {
    elapsed.str <- paste(elapsed.str, elapsed %/% 60 %% 60, "min")
  }
  elapsed.str <- paste(elapsed.str, sprintf("%.2f sec elapsed", elapsed %% 60)) 
  
  return(paste0(msg, ': ', elapsed.str ))
}

toc_ <- function(...) {
  toc(func.toc = toc.fn, ...)
}


rm_vars <- function(vars2clear = NULL, clear_vars = TRUE) {
  if (clear_vars) {
    for(var_ in vars2clear) {
      if (exists(var_))
        rm(list = var_, envir = globalenv())
    }
  }   
  gc(verbose = FALSE, full = TRUE)
  return(TRUE)
}


estimate_cores <- function(rows = NULL, n_cores = NULL) {
  if (! exists('n_cores.default')) {
    n_cores.default <- 4
  }
  if (is.null(n_cores)) { 
    if (is.null(rows)) {
      if (exists('stats.career')) {
        rows <- nrow(stats.career)
      } else if (exists('stats.update')) {
        rows <- nrow(stats.update)
      } else if (exists('stats.tier')) {
        rows <- nrow(stats.tier)
      } else {
        return(min(n_cores.default, availableCores() ))
      }
    } 
    return(min(availableCores(), n_cores.max, max(n_cores.min, row.lim.cores  %/% rows)))    
  } else {
    return(min(n_cores, availableCores() ))
  }
}


set_threads <- function(n_threads) {
  o <- omp_set_num_threads(n_threads)
  d <- setDTthreads(n_threads)
  b <- blas_set_num_threads(n_threads)

  return(max(o, b, d, na.rm = TRUE))
}

## data.table utils  #############################

delete <- function(DT, del.ndx, opt_mem=TRUE) {
  if (length(del.ndx) == 0) {
    return(DT)
  } else  if (opt_mem) {
    ## delete rows column-wise by assigment. Lower RAM consumption, but 5x slower. 
    cols = names(DT);
    keep.ndx <- setdiff(seq_len(nrow(DT)), del.ndx);
    DT.subset <- data.table(DT[[1]][keep.ndx]); # subset DT's 1st col
    setnames(DT.subset, cols[1]);
    for (col in cols[2:length(cols)]) {
      DT.subset[, (col) := DT[[col]][keep.ndx]];
      DT[, (col) := NULL]; #delete
    }
    return(DT.subset);
  } else {
    ## 5x faster delete by assignment
    return(DT[-del.ndx])
  }
}


## Get number of data.table splits
get_DT_splits <- function(DT, rows.split=row.lim.split) {
  stopifnot(exists('row.lim.split'))
  rows = 0
  if (is(DT, 'list')) {
    ## DT list
    for (i in seq_len(length(DT))) {
      rows <- rows + DT[, .N]
    }
  } else if (is.data.table(DT)) {
    rows <- DT[, .N]
  } else {
    return(NULL)
  }
  return(ceiling(rows / rows.split))  
}


## Split data-table into smaller pieces to limit RAM usage in certain operations
DT_split <- function(DT, N, col = "account_id", tmp.col = "TMP_SPLIT") {
  ## Returns ALWAYS a list of DTs, even length of 1
  N = as.integer(N)
  if (is.data.table(DT)) {
    if (N > 1) {
      DT.org <- DT
      DT[, (tmp.col) := get(col) %% N]
      DT <- split(DT, by=tmp.col, keep.by = FALSE, sorted = TRUE)
      # DT.org[, (tmp.col)  := NULL ]
    } else {
      DT <- list('1'=DT)    # to confirm the rbindlist logic
    }
    return(DT)
  } else if (is(DT, 'list')) {  
    ## Handle DT that are already split (DT saved as a splitted DT due to RAM limits)
    DT.split <- linkedList()
    for (i in seq(length(DT))) {
      DT.split$add(DT_split(DT[[1]], N, col, tmp.col))
      DT[[1]] <- NULL
    }
    return(DTL_flatten(DT.split$as.list()))
  } 
  return(NULL)
}


DTL_rebind <- function(DTL) {
  if (is(DTL, 'list')) {
    ## DTL == list()
    if (length(DTL) > 1) {
      return(rbindlist(DTL, use.names = TRUE, fill = TRUE))
    } else if (length(DTL) == 1) {
      return(DTL[[1]])
    }
  } else if (is.data.table(DTL)) {
    ## nothing to rebind
    return(DTL)
  } 
  return(NULL)    
}


DTL_flatten <- function(DTL) {
  # DTL is a list of splitted DT lists returned by split.data.table()
  stopifnot(is(DTL, 'list'))
  DTL.len <- length(DTL)
  splits.leaf <- NULL
  
  for (s.root in seq_len(DTL.len)) {
    splits.leaf <- union(splits.leaf, names(DTL[[s.root]]))
  }
  stopifnot(! is.null(splits.leaf))

  DT.res <- namedExpandingList()  
  for (s.leaf in str_sort(splits.leaf)) {
    DT.tmp <- linkedList()
    for (s.root in seq_len(DTL.len)) {
      if (s.leaf %in% names(DTL[[s.root]])) {
        DT.tmp$add(DTL[[s.root]][[s.leaf]])
        DTL[[s.root]][[s.leaf]] <- NULL
      }
    } # for s.root
    DT.tmp <- DT.tmp$as.list()
    DT.res$add(s.leaf, rbindlist(DT.tmp, use.names = TRUE, fill=TRUE))
  } # for s.leaf
  
  return(DT.res$as.list())
}


## Run function on a data either in a list or not
 run_parts <- function(dat_list, func, ...) {
  if (is.data.table(dat_list)) {
    dat_list <- func(dat_list, ...)
  } else if (is(dat_list, 'list')) {
    for (i in seq(length(dat_list))) {
		# message('run_parts(): ', i)	
      	dat_list[[i]] <- func(dat_list[[i]], ...)
    }
  }
  return(dat_list)
}

## Process files in parts, save results in another file
 process_parts <- function(src_file, dst_file, force, func, ...) {
  if (!exists('n_threads')) {
    n_threads <- 1
  }
  if ( file.exists(src_file)) {
    if (file.exists(dst_file) ) {
      if (!force) {
        message(paste(basename(dst_file), ': File exists and force=FALSE: Skipping'))
        return()
      }
    }
    cat(paste('Reading file:', basename(src_file), "\n"))
    DT <- qread(src_file, nthreads=n_threads)
    cat(paste('Processing file:', basename(src_file), "\n"))
    DT <- func(DT, ...)
    cat(paste('Saving file:', basename(dst_file), "\n"))
    qsave(DT, dst_file, nthreads=n_threads)
  } else if ( file.exists(filename_part(src_file, 0)) ) {
    ndx = 0 
    src_file.tmp <- filename_part(src_file, ndx)
    dst_file.tmp <- filename_part(dst_file, ndx)
    while ( file.exists(src_file.tmp)) {
      if (file.exists(dst_file.tmp) ) {
        if (!force) {
          message(paste(basename(dst_file.tmp), ': File exists and force=FALSE: Skipping'))
          ndx <- ndx + 1
          src_file.tmp <- filename_part(src_file, ndx)
          dst_file.tmp <- filename_part(dst_file, ndx)
          next
        }
      }
      cat(paste('Reading filepart:', basename(src_file.tmp), "\n"))
      DT <- qread(src_file.tmp, nthreads=n_threads)
      cat(paste('Processing filepart:', basename(src_file.tmp), "\n"))
      DT <- func(DT, ...)
      
      cat(paste('Saving filepart:', basename(dst_file.tmp), "\n"))
      qsave(DT, dst_file.tmp, nthreads=n_threads)
      
      DT <- NULL
      ndx <- ndx + 1
      src_file.tmp <- filename_part(src_file, ndx)
      dst_file.tmp <- filename_part(dst_file, ndx)
    } # while
  } # if
}

## Transform a file in parts
transform_parts <- function(src_file, func, ...) {
  cat("Transforming files. OVERWRITING DATA\n")
  Sys.sleep(2)
  process_parts(src_file, src_file, force = TRUE, func, ...)
}

## Process files in parts
# process_parts <- function(src_file, dst_file, force, func, ...) {
#   if (!exists('n_threads')) {
#     n_threads <- 1
#   }
#   if (file.exists(dst_file) ) {
#     if (!force) {
#       message(paste(basename(dst_file), ': File exists and force=FALSE: Skipping'))
#       return()
#     }
#   }
#   if ( file.exists(src_file)) {
#     cat(paste('Reading file:', basename(src_file), "\n"))
#     DT <- qread(src_file, nthreads=n_threads)
#     cat(paste('Processing file:', basename(src_file), "\n"))
#     DT <- func(DT, ...)
#     cat(paste('Saving file:', basename(dst_file), "\n"))
#     qsave(DT, dst_file, nthreads=n_threads)
#     
#   } else if ( file.exists(filename_part(src_file, 0)) ) {
#     ndx = 0 
#     src_file.tmp <- filename_part(src_file, ndx)
#     
#     DTL <- list()
#     while ( file.exists(src_file.tmp)) {
#       cat(paste('Reading filepart:', basename(src_file.tmp), "\n"))
#       DT <- qread(src_file.tmp, nthreads=n_threads)
#       cat(paste('Processing filepart:', basename(src_file.tmp), "\n"))
#       ndx <- ndx+1
#       DTL[[ndx]] <- func(DT, ...)
#       DT <- NULL
#       src_file.tmp <- filename_part(src_file, ndx)
#       
#     } # while
#     cat(paste('Saving stats for update', update, "\n"))
#     save_tank_stats(DTL, dst_file)
#   } # if
# }


drop_cols <- function(DT, cols.drop) {
  cols <- colnames(DT)
  for (col in base::intersect(cols, cols.drop)) {
    DT[, (col) := NULL]
  }
  return(DT)
} 


