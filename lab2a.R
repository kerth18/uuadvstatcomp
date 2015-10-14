f <- function(x) {(x-3)^2 + 2*(x-3)^2 + 3*(x-15)^2 + sin(100*x)}

plot(f, 0, 15)
interval1 <- c(0,15)
interval2 <- c(9,12)
interval3 <- c(10,11)

optimize(f, interval1)
optimize(f, interval2)
optimize(f, interval3)
