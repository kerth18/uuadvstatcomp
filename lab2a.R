################### Optimization ###################

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


################### Integrating a function ###################

f2 <- function(x) {x*sin(x)}
plot(f2, -7e5, 7e5)

integrate(f2, -7e5, 7e5, subdivisions = 1e7)
system.time(integrate(f2, -7e5, 7e5, subdivisions = 1e7))
## user  system elapsed 
# 32.37    0.03   34.24 

library(parallel)
cores <- detectCores() ## 2 cores
cluster <- makePSOCKcluster(cores)

# first try, divide the interval into 2 intervals
int1 <- c(-7e5,0)
int2 <- c(0,7e5)
list_intervals <- list(int1,int2)
parLapply(cluster, list_intervals, 
          (function(int) {integrate(function(x) {x*sin(x)}, int[1], int[2], subdivisions = 1e7)}))
system.time(parLapply(cluster, list_intervals, 
                      (function(int) {integrate(function(x) {x*sin(x)}, int[1], int[2], subdivisions = 1e7)})))
#on 8 cores
#user  system elapsed 
#0.00    0.00    2.62 

# create a list of n intervals of equal length
start_interval = -7e5
end_interval = 7e5
nb_intervals = 16
interval_length = (end_interval - start_interval)/nb_intervals
list_intervals <- list()
for (i in 1:nb_intervals){
  left = start_interval + interval_length*(i-1)
  right = start_interval + interval_length*i
  list_intervals[[i]] <- c(left,right)
}

parLapply(cluster, list_intervals, 
          (function(int) {integrate(function(x) {x*sin(x)}, int[1], int[2], subdivisions = 1e7)}))
system.time(parLapply(cluster, list_intervals, 
                      (function(int) {integrate(function(x) {x*sin(x)}, int[1], int[2], subdivisions = 1e7)})))
#8 intervals, 8 clusters
#user  system elapsed 
#0.00    0.00    0.04 

#16 intervals, 8 clusters
#user  system elapsed 
# 0.00    0.00    0.3 

#16 intervals, 16 clusters
#user  system elapsed 
#0.00    0.00    0.31 


################### Functional Operators ###################


### Memoisation ###
library(memoise)

fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})

fib3 <- memoise(fib)

#> system.time(fib(28))
#user  system elapsed 
#1.44    0.00    1.44 
#> system.time(fib2(28))
#user  system elapsed 
#   0       0       0 
#> system.time(fib3(28))
#user  system elapsed 
#   0       0       0

### ggplot ###
library(ggplot2)

qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv)
qplot(displ, hwy, data=mpg, geom=c("point","smooth")) 
qplot(displ, hwy, data =mpg, geom=c("point","smooth"), method="lm") 
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),xmethod="lm", color=drv)
gr <- qplot(displ, hwy, data =mpg, geom=c("point","smooth"),xmethod="lm", color=drv)
gr + theme(panel.background = element_rect(fill = "pink"))

qplot(carat, price, data=diamonds) 
grc <- qplot(carat, price, data=diamonds, color=color) 
qplot(carat, price, data=diamonds, geom=c("point","smooth"),xmethod="lm", color=color)
grd <- qplot(carat, price, data=diamonds, geom=c("point","smooth"),xmethod="lm", color=color)
grd + scale_colour_brewer(name = "New legends")
qplot(color, price / carat, data = diamonds, geom = "boxplot")

