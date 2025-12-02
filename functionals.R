# Library -----------------------------
library(purrr)
library(broom)

# The `map()` -------------------------
## Use `as_mapper()` ------------------

as_mapper(1)
as_mapper(c(1, 2,3))
as_mapper(c("a", "b", "c"))
as_mapper(list("1", "b", "2"))

## Differences in formula of map -------------
 map(1:3, ~runif(2),.progress = TRUE)
 map(1:3, runif(2))

## Simulated t-test for poisson distribution
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10)))

p_values <- map_dbl(trials, 1, "p.value")
hist(p_values)
t_test <- t.test(rpois(10, 10), rpois(7, 10))
tidy(t_test)
