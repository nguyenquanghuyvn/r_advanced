# Library -----------------------------
library(purrr)
library(broom)
library(ggplot2)
library(scales)
library(rlang)
library(tibble)
renv::install("bench")
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

# Function factories -----------------

power1 <- function(exp) {
  force(exp)
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

square
cube

rlang::env_print(square)
rlang::env_print(cube)

fn_env(square)$exp
fn_env(cube)$exp

square(10)
square(2)

x <- 2
square <- power1(x)
x <- 3
square(2) #> Return 8, instead of 2 because of lazy evaluation

## Stateful functions
new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_two()

fn_env(counter_one)
fn_env(counter_two)

env_print(counter_one)
env_print(counter_two)

### Garbage collection
#> We need to use `rm()` to unbind the unused objects in manufactured function

f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

g1 <- f1(1e6)
env_print(g1)
lobstr::obj_size(g1)

f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
env_print(g2)
lobstr::obj_size(g2)

pick <- function(index) {
  force(index)
  function(x) {
    x[[index]]
  }
}

lapply(mtcars, pick(5))

moment <- function(i) {
  function(x) {
    mom = 1 / length(x) * sum(x^i)
    mom
  }
}

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
m1(x)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}

new_counter2()

### Graphical factories ------------------------------------------
y <- c(12345, 123456, 1234567)

df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)
  
core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)

#### Historgram bins---------------------------------------------
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))

ggplot(df, aes(x)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ sd, scales = "free_x") +
  labs(x = NULL)

binwidth_bins <- function(n) {
  force(n)
  function(x) {
   (max(x) - min(x)) / n
  }
}

## Graphical factories -------------------------------------------
y <- c(12345, 25368, 78956, 230.400)
label_number( decimal.mark = ",", accuracy = 0.01)(y)

df <- tibble(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(labels = label_number())
core + scale_y_continuous(labels = label_comma())
core + scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))

## Histogram bins
sd <- c(1, 5, 15)
n <- 100

df <- tibble(
  x = rnorm(3 * n, sd = sd),
  sd = rep(sd, n)
)

ggplot(df, aes(x = x)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ sd, scales = "free_x") +
  labs(x = NULL)

### Function get the number of bins, output a function returning a binwidth
binwidth_bins <- function(n) {
  force(n)

  function(x) {
    (max(x) - min(x)) / n
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = binwidth_bins(15)) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

base_bins <- function(type) {
  fun <- switch(type,
    Sturges = nclass.Sturges,
    scott = nclass.scott,
    FD = nclass.FD,
    stop("Unknown type", call. = FALSE)
  )
  
  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = base_bins("Sturges")) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

## Statistical factories ------------------------------------------
### Boxcox transformation ----------------------------------------

boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <- function(lambda) {
  stat_function(aes(color = lambda), fun = boxcox2(lambda), linewidth = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

### Bootstrap generators -----------------------------------------
boot_permute <- function(df, var) {
  n <- nrow(df)
  force(var)

  function() {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
boot_mtcars1()

boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)

  function() {
    fitted + sample(resid)
  }
} 

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())

### Maximum likelihood estimation --------------------------------
ll_poisson2 <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))

  function(lambda) {
    log(lambda) * sum_x - n * lambda - c
  }
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
ll1 <- ll_poisson2(x1)
ll1(10)

fn_env(ll1)
env_print(ll1)

optimize(ll1, c(0, 100), maximum = TRUE)

### Exercises ----------------------------------------------------------
boxcox3 <- function(x) {
  function(lambda) {
    if (lambda == 0) {
      log(x)
    } else {
      (x ^ lambda - 1) / lambda
    }
  }  
}

bc3 <- boxcox3(x1)
bc3()

stat_boxcox <- function(x) {
  stat_function( fun = boxcox3(x), linewidth = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

ll_poisson1 <- function(x) {
  n <- length(x)

  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}

x1 <- rnorm(1000)
ll1 <- ll_poisson1(x1)
ll2 <- ll_poisson2(x1)

bench::mark(
  ll1(30),
  ll2(30)
)
 # Functio factories and functionals ---------------------------------------
power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)
funs

with(funs, root(100))
with(funs, cube(2))
with(funs, cuberoot(8))

funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)

attach(funs)
#> The following objects are masked from package:base:
#> 
#>     mean, sum
mean <- function(x) stop("Hi!")
mean(c(1,2, 3))
detach(funs)

env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 
env_unbind(globalenv(), names(funs))


# Function operators ------------------------------------------------------
deplay_by <- function(f, amount) {
  force(f)
  force(amount)

  function(...) {
    Sys.sleep(amount)
    f(...)
  }
}

walk2()