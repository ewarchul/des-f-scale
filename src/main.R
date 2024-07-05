source("des.R")
source("exp.R")

sphere <- function(x) { sum(x^2) }

random_problem <- function(x) { rnorm(1, 0) }

problems <- list(
  list("name" = "rnorm_0_1", fn = random_problem)
)

solvers <- list(
  list("name" = "vanila-des", "solver" = des)
)

exp_spec_10d <- list("dim" = 50, "rep" = 10)

run_exp(problems, solvers, exp_spec_10d, "/home/ewarchul/Phd/des-f-scale/src")


