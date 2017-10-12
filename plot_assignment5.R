visualization <- function(mean=0, sd=1, from=mean-4*sd, to=mean+4*sd){
  
  # Generation distribution
  lower_bound <- mean - 4*sd
  upper_bound <- mean + 4*sd
  step = (upper_bound-lower_bound)/1000
  x <- seq(lower_bound, upper_bound, step)
  
  curve(dnorm(x, mean, sd), xlim = c(lower_bound, upper_bound), ylab = "y")
  
  # Area under the curve
  x_ <- c(from, seq(from, to, step), to)
  y_ <- c(0, dnorm(seq(from, to, step), mean, sd), 0)
  
  polygon(x_, y_, col = "red")
  
}

# Default function
visualization()


# 3.2
visualization(from = -1.13) ### a
visualization(to = 0.18) ### b
visualization(from = 8) ### c
visualization(from = -0.5, to = 0.5) ### d

# 3.10
visualization(mean = 55, sd = 6, to = 48) ### a
visualization(mean = 55, sd = 6, from = 60, to = 65) ### b
visualization(mean = 55, sd = 6,
              from = qnorm(p=0.1, mean = 55, sd = 6, lower.tail = FALSE)) ### c
visualization(mean = 55, sd = 6, to = 55) ### d

# 3.12
visualization(mean = 72.6, sd = 4.78, to = 80) ### a
visualization(mean = 72.6, sd = 4.78, from = 60, to = 80) ### b
visualization(mean = 72.6, sd = 4.78,
              from = qnorm(p=0.05, mean = 72.6, sd = 4.78, lower.tail = FALSE)) ### c
visualization(mean = 72.6, sd = 4.78, from = 70) ### d
