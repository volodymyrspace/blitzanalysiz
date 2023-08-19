set.seed(2017)

n = 250e3L
m = 50L

B <- structure(sample.int(1000, n*m, replace = TRUE), .Dim = c(n, m))
tp <- rnorm(m, 1, 0.3)

bm <- microbenchmark("double transpose"= { res <- t(t(B) * tp)}, 
                     "row product" = {res <- mtrx_row_product(B, tp)})