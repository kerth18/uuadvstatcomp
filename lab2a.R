### Optimization ###

f <- function(x) {(x-3)^2 + 2*(x-3)^2 + 3*(x-15)^2 + sin(100*x)}

plot(f, 0, 15)
interval1 <- c(0,15)
interval2 <- c(9,12)
interval3 <- c(10,11)

optimize(f, interval1)
## $minimum 9.283052

optimize(f, interval2)
## $minimum 9.534078 

optimize(f, interval3)
## $minimum 10.16165

### Integrating a function ###

f2 <- function(x) {x*sin(x)}
plot(f2, -7e5, 7e5)

integrate(f2, -7e5, 7e5, subdivisions = 1e7)
system.time(integrate(f2, -7e5, 7e5, subdivisions = 1e7))
## user  system elapsed 
## 32.37    0.03   34.24 