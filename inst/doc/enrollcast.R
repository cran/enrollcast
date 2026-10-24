## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(enrollcast)

## -----------------------------------------------------------------------------
history <- data.frame(
  year = rep(2021:2023, each = 3),
  grade = factor(rep(c("K", "1", "2"), 3), levels = c("K", "1", "2")),
  enrollment = c(100, 90, 80, 110, 95, 88, 120, 99, 91)
)
history

## -----------------------------------------------------------------------------
ratios <- progression_ratios(history, method = "mean")
ratios

## -----------------------------------------------------------------------------
progression_matrix(ratios)

## -----------------------------------------------------------------------------
base <- history[history$year == 2023, c("grade", "enrollment")]
projection <- project_enrollment(
  base = base,
  ratios = ratios,
  horizon = 3,
  entry = c(125, 130, 128),
  start_year = 2023
)
projection

## -----------------------------------------------------------------------------
observed <- data.frame(
  year = history$year,
  grade = as.character(history$grade),
  enrollment = history$enrollment
)
combined <- rbind(observed, projection)
head(combined)

## -----------------------------------------------------------------------------
depressed <- c(K = 80, `1` = 66, `2` = 60)
schedule <- swing_schedule(ratios,
  horizon = 6, swing_years = 2,
  recovery = c(225, 245, 260), entry = rep(90, 6)
)
recovered <- project_enrollment(depressed, schedule = schedule, start_year = 2023)
recovered
tapply(recovered$enrollment, recovered$year, sum)

## -----------------------------------------------------------------------------
m <- progression_matrix(ratios)
closed <- m
closed["2", ] <- 0
opening <- swing_schedule(
  ratios = list(closed, closed, m, m),
  horizon = 4, swing_years = 1,
  recovery = c(165, 235), entry = rep(90, 4)
)
project_enrollment(c(K = 80, `1` = 66, `2` = 0), schedule = opening)

