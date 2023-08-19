library(microbenchmark)
lPtrAppend <- function(lstptr, lab, obj) {lstptr[[deparse(lab)]] <- obj}
### Store list inside new environment
envAppendList <- function(lstptr, obj) {lstptr$list[[length(lstptr$list)+1]] <- obj} 
env2list <- function(env, len) {
  l <- vector('list', len)
  for (i in 1:len) {
    l[[i]] <- env[[as.character(i)]]
  }
  l
}
envl2list <- function(env, len) {
  l <- vector('list', len)
  for (i in 1:len) {
    l[[i]] <- env[[paste(as.character(i), 'L', sep='')]]
  }
  l
}
runBenchmark <- function(n) {
  microbenchmark(times = 5,  
                 # env_with_list_ = {
                 #   listptr <- new.env(parent=globalenv())
                 #   listptr$list <- NULL
                 #   for(i in 1:n) {envAppendList(listptr, i)}
                 #   listptr$list
                 # },
                 # c_ = {
                 #   a <- list(0)
                 #   for(i in 1:n) {a = c(a, list(i))}
                 # },
                 # list_ = {
                 #   a <- list(0)
                 #   for(i in 1:n) {a <- list(a, list(i))}
                 # },
                 # by_index = {
                 #   a <- list(0)
                 #   for(i in 1:n) {a[length(a) + 1] <- i}
                 #   a
                 # },
                 # append_ = { 
                 #   a <- list(0)    
                 #   for(i in 1:n) {a <- append(a, i)} 
                 #   a
                 # },
                 # env_as_container_ = {
                 #   listptr <- new.env(hash=TRUE, parent=globalenv())
                 #   for(i in 1:n) {lPtrAppend(listptr, i, i)} 
                 #   envl2list(listptr, n)
                 # },
                 # better_env_as_container = {
                 #   env <- new.env(hash=TRUE, parent=globalenv())
                 #   for(i in 1:n) env[[as.character(i)]] <- i
                 #   env2list(env, n)
                 # },
                 linkedList = {
                   a <- linkedList()
                   for(i in 1:n) { a$add(i) }
                   a$as.list()
                 },
                 # inlineLinkedList = {
                 #   a <- list()
                 #   for(i in 1:n) { a <- list(a, i) }
                 #   b <- vector('list', n)
                 #   head <- a
                 #   for(i in n:1) {
                 #     b[[i]] <- head[[2]]
                 #     head <- head[[1]]
                 #   }                
                 # },
                 expandingList = {
                   a <- expandingList()
                   for(i in 1:n) { a$add(i) }
                   a$as.list()
                 # },
                 # inlineExpandingList = {
                 #   l <- vector('list', 10)
                 #   cap <- 10
                 #   len <- 0
                 #   for(i in 1:n) {
                 #     if(len == cap) {
                 #       l <- c(l, vector('list', cap))
                 #       cap <- cap*2
                 #     }
                 #     len <- len + 1
                 #     l[[len]] <- i
                 #   }
                 #   l[1:len]
                 }
  )
}

# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
expandingList <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0
  
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }
  
  methods
}

linkedList <- function() {
  head <- list(0)
  length <- 0
  
  methods <- list()
  
  methods$add <- function(val) {
    length <<- length + 1
    head <<- list(head, val)
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

# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
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