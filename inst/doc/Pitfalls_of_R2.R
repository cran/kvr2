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
df1 <- data.frame(x = 1:6, y = c(15, 37, 52, 59, 83, 92))

# Model without an intercept
model_no_int <- lm(y ~ x - 1, data = df1)

# Calculate all 9 types
results <- r2(model_no_int)
results

## -----------------------------------------------------------------------------
# Power model via log-transformation
model_power <- lm(log(y) ~ log(x), data = df1)
r2(model_power)

