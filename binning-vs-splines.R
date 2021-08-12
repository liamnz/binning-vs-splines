library(ggplot2)
library(splines)

wiggly <- function(x){
  (x + 2) * (x - 3) * (x - 1) + 25 * sin(x) + 150
}

set.seed(69)
n <- 500
x <- round(rnorm(n, 0, 1.5), 4)
y <- wiggly(x) + rnorm(n, 0, 15)

n_bins <- 10
n_knots <- 6
quantiles <- quantile(x, seq(0, 1, 1 / n_bins))
x_binned <- cut(x, quantiles, include.lowest = TRUE, ordered_result = TRUE)

df <- data.frame(y, x, x_binned)

summary(model_linear <- lm(y ~ x, data = df))
summary(model_binned <- lm(y ~ x_binned, data = df))
summary(model_spline <- lm(y ~ ns(x, n_knots), data = df))

df$fit_linear <- fitted(model_linear)
df$fit_binned <- fitted(model_binned)
df$fit_spline <- fitted(model_spline)

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_function(fun = wiggly, colour = 'azure4', size = 1.5) +
  geom_line(aes(y = fit_linear), colour = 'steelblue', size = 1.5) +
  geom_line(aes(y = fit_binned), colour = 'darkorange', size = 1.5) +
  geom_line(aes(y = fit_spline), colour = 'purple', size = 1.5)




