# Set up --------------------------------------------------------
library(rlang)
library(lobstr)
#---------------------------------------------------------------#

# Introduction -------------------------------------------------
#> How do we capture the intent of the code without executing it?

y <- x * 10

z <- rlang::expr(y <- x * 10)
z
#> Expression is an object capturing the structure of the code

x <- 4
eval(z)
y

# Abstract syntax trees --------------------------------------
lobstr::ast(f(x, "y", 2))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)

# Expressions -----------------------------------------------
rlang::is_syntactic_literal("x")
identical(expr(TRUE),TRUE)

expr(x)
rlang::sym("x")
str(expr(x))
is.symbol(expr(x))
is.symbol("x")

## Calls ---------------------------------------------------
lobstr::ast(read.table("important.csv", row.names = FALSE))

x <- expr(read.table("important.csv", row.names = FALSE))
typeof(x)
is.call(x)

x[[1]]
x[[2]]
as.list(x[-1])
length(x) - 1
rlang::call_standardise(x)
rlang::call_match(x, read.table)


# Parsing and grammar----------------------------------------
## Finding F and T ------------------------------------------
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if(is.symbol(x)) {
    "symbol"
  } else if(is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

expr_type("x")
expr_type(expr(c))
expr_type(expr(f(1, 2)))
expr_type(expr(mean(x, na.rm = TRUE)))

switch_expr <- function(x, ...) {
  switch(expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

logical_abbr_rec <- function(x) {
  switch_expr(x,
    constant = FALSE,
    symbol = as_string(x) %in% c("F", "T"),

    call = ,
    pairlist = purrr::some(x, logical_abbr_rec)
  )
}

logical_abbr <- function(x) {
  logical_abbr_rec(enexpr(x))
}

logical_abbr(T)
logical_abbr(mean(x, na.rm = T))

f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
