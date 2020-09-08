## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(vacuum)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

## ----echo = FALSE, fig.width = 7, fig.height = 4------------------------------
# Normal Q-Q plot for Tukey's sample data
data.frame(y = table_1) %>% 
ggplot(aes(sample = y)) +
  stat_qq() +
  stat_qq_line() +
  #geom_abline(aes(intercept = 0, slope = 1)) +
  labs(title = 'Normal Q-Q plot of sample data')

## ----echo = FALSE, fig.width = 7, fig.height = 4------------------------------
x <- table_1
n <- length(x)
y_split <- median(x)

dat <- data.frame(i = 1:n, 
                  y_sorted = sort(x),
                  a = a_qnorm(1:n, n), 
                  z = (sort(x) - y_split) / a_qnorm(1:n, n)
)

# find the point with the greatest slope
key_pt <- dat[which.max(dat$z), ]

# plot the points and the slope of the point key point
ggplot(dat, aes(x = a, y = y_sorted)) +
  geom_point(color = 'darkgrey') + 
  geom_point(dat = key_pt, aes(x = a, y = y_sorted), color = 'black') + 
  geom_point(aes(x = 0, y = y_split), color = 'red') +
  # example line
  geom_segment(aes(x = 0, y = y_split, xend = key_pt$a, yend = key_pt$y_sorted)) +
  labs(title = 'Sorted values of y plotted against theoretical distribution a(y)', y = 'y')


## ----echo = FALSE, , fig.width = 7, fig.height = 4----------------------------
A <- 0
B <- 1.5
tmp <- funop(table_1, A, B)
# need z & special for every point
tmp$z <- (tmp$y - median(tmp$y)) / a_qnorm(tmp$i, n)
tmp$special[is.na(tmp$special)] <- FALSE
# need outer to get special and outer to add up as a factor
tmp$outer <- !tmp$middle
z_split <- attr(tmp, 'z_split')
y_split <- attr(tmp, 'y_split')
extreme_B <- B * z_split
extreme_A <- A * z_split

ggplot(tmp, aes(x = y, y = z)) +
  geom_point(aes(color = as.factor(outer + special))) +
  geom_hline(yintercept = z_split) +
  geom_hline(yintercept = extreme_B,
             color = 'blue',
             alpha = 0.5) +
  geom_vline(xintercept = y_split,
             color = 'green3',
             alpha = 0.75) +
  geom_rect(
    xmin = -Inf,
    xmax = Inf,
    ymin = extreme_B,
    ymax = Inf,
    alpha = 0.0075,
    fill = 'blue'
  ) +
  geom_rect(
    xmin = y_split - extreme_A,
    xmax = y_split + extreme_A,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.01,
    fill = 'green'
  ) +
  labs(title = 'FUNOP: slope (z) plotted against original values') +
  labs(x = 'original values') +
  scale_color_manual(
    name = 'values',
    labels = c(
      'excluded',
      'undistinguished',
      'special attention'
    ),
    values = c('green3', 'black', 'red')
  )



## -----------------------------------------------------------------------------
# Table 1 contains example data from Tukey's paper
table_1

result <- funop(table_1)
result

# value of the outliers
result[which(result$special == TRUE), 'y']


## ----echo = FALSE, fig.width = 7, fig.height = 4------------------------------
# cleanse the table
cleansed_t2 <- funor_funom(table_2)

data.frame(
  i = 1:length(table_2),
  original = as.vector(table_2),
  cleansed = as.vector(cleansed_t2)
) %>%
  ggplot(aes(x = i)) +
  geom_point(aes(y = original), color = 'black') +
  geom_segment(aes(
    x = i,
    y = original,
    xend = i,
    yend = cleansed
  ),
  color = 'black',
  linetype = 'dashed') +
  geom_point(aes(y = cleansed), color = 'grey') +
  labs(title = 'Changes to Table 2', y = 'value', x = '')


## -----------------------------------------------------------------------------
# create a random dataset, representing a two-way table 
set.seed(42)
dat <- matrix(rnorm(16), nrow = 4)
# add some noise
dat[2, 1] <- rnorm(1, mean = 10)

# cleanse the table of outliers
cleansed_dat <- funor_funom(dat)

# look at the two tables, before and after
dat
cleansed_dat 

# look at the difference between the two tables
# only one non-zero difference
dat - cleansed_dat


## -----------------------------------------------------------------------------
# convert the output of vacuum_cleaner into a long data frame
dat <- vacuum_cleaner(table_8) %>% 
  as.data.frame() %>% 
  dplyr::mutate('row' = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('V'),
               names_to = 'col',
               values_to = 'value')

# since the table_8 was a two-way table, the rows are factors
dat$row <- as.factor(dat$row)

# conduct analysis of variance
aov_results <- aov(value ~ row + col, dat) 
summary(aov_results)

