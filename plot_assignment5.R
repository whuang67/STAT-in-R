visualization <- function(mean=0, sd=1, from=mean-4*sd, to=mean+4*sd){
  
  # Generation distribution
  lower_bound <- mean - 4*sd
  upper_bound <- mean + 4*sd
  step = 8*sd/1000
  x <- seq(lower_bound, upper_bound, step)
  
  curve(dnorm(x, mean, sd), xlim = c(lower_bound, upper_bound), ylab = "y")
  
  # Area under the curve
  if(from < mean-4*sd){
    from = mean-4*sd
  } else if(from > mean+4*sd){
    from = mean+4*sd
  }
  
  if(to > mean+4*sd){
    to = mean+4*sd
  } else if(to < mean-4*sd){
    to = mean-4*sd
  }
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


library(ggplot2)
visualization2 <- function(mean=0, sd=1, from=mean-4*sd, to=mean+4*sd){
  lower_bound <- mean - 4*sd
  upper_bound <- mean + 4*sd
  step = (upper_bound-lower_bound)/1000
  x <- seq(lower_bound, upper_bound, step)
  
  if(from < mean-4*sd){
    from = mean-4*sd
  } else if(from > mean+4*sd){
    from = mean+4*sd
  }
  
  if(to > mean+4*sd){
    to = mean+4*sd
  } else if(to < mean-4*sd){
    to = mean-4*sd
  }
  
  ggplot(mapping = aes(x = x)) +
    # Generation distribution
    stat_function(fun = dnorm,
                  args = list(mean = mean, sd = sd)) +

    # Area under the curve
    stat_function(fun = dnorm,
                  args = list(mean = mean, sd = sd),
                  geom = "area",
                  xlim = c(from, to),
                  alpha = .5)
}

# Default function
visualization2()


# 3.2
visualization2(from = -1.13) ### a
visualization2(to = 0.18) ### b
visualization2(from = 8) ### c
visualization2(from = -0.5, to = 0.5) ### d

# 3.10
visualization2(mean = 55, sd = 6, to = 48) ### a
visualization2(mean = 55, sd = 6, from = 60, to = 65) ### b
visualization2(mean = 55, sd = 6,
               from = qnorm(p=0.1, mean = 55, sd = 6, lower.tail = FALSE)) ### c
visualization2(mean = 55, sd = 6, to = 54) ### d

# 3.12
visualization2(mean = 72.6, sd = 4.78, to = 80) ### a
visualization2(mean = 72.6, sd = 4.78, from = 60, to = 80) ### b
visualization2(mean = 72.6, sd = 4.78,
               from = qnorm(p=0.05, mean = 72.6, sd = 4.78, lower.tail = FALSE)) ### c
visualization2(mean = 72.6, sd = 4.78, from = 70) ### d
