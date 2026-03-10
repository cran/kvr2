## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(kvr2)

# Example: Data with a trend that doesn't pass through (0,0)
df_neg <- data.frame(
  x = c(110, 120, 130, 140, 150, 160, 170, 180, 190, 200),
  y = c(180, 170, 180, 170, 160, 160, 150, 145, 140, 145)
)

model_forced <- lm(y ~ x - 1, data = df_neg)
r2(model_forced)


## -----------------------------------------------------------------------------
# Power model via log-transformation
df1 <- data.frame(x = 1:6, y = c(15, 37, 52, 59, 83, 92))
model_power <- lm(log(y) ~ log(x), data = df1)
r2(model_power)

## -----------------------------------------------------------------------------
# A linear model where the dependent variable is named 'log'
df_log_name <- data.frame(x = 1:6, log = c(15, 37, 52, 59, 83, 92))
model_linear_log <- lm(log ~ x, data = df_log_name)

# kvr2 correctly identifies this as "linear", not "power"
model_info(r2(model_linear_log))$type

## -----------------------------------------------------------------------------
model_no_int <- lm(y ~ x - 1, df1)
res <- r2(model_no_int)
model_info(res)

## -----------------------------------------------------------------------------
df1 <- data.frame(x = 1:6, y = c(15, 37, 52, 59, 83, 92))

# Model without an intercept
model_no_int <- lm(y ~ x - 1, data = df1)

# Calculate all 9 types
results <- r2(model_no_int)
results

## ----fig.height=6, fig.width=10-----------------------------------------------
# Example with the forced no-intercept model
plot_kvr2(model_forced)

## -----------------------------------------------------------------------------
model_int <- lm(y ~ x, data = df1)

# Compare the two model specifications
comparison <- comp_model(model_int)

## -----------------------------------------------------------------------------
# Compare adjusted R-squared values
comp_model(model_int, adjusted = TRUE)

## ----fig.height=6, fig.width=10-----------------------------------------------
# Generate the 2x2 comparison dashboard
plot(comparison)

## -----------------------------------------------------------------------------
# Create a dataset where y = 50 + 2x + noise
set.seed(123)
df_break <- data.frame(x = 1:10, y = 50 + 2 * (1:10) + rnorm(10, 0, 2))

# Compare models
res_break <- comp_model(lm(y ~ x, data = df_break))

# The numerical output will show R2_2 and R2_3 > 1.0
res_break

## ----fig.height=6, fig.width=10-----------------------------------------------
plot(res_break)

