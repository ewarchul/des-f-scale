library(tibble)
library(logger)
library(readr)
library(here)
library(stringr)
library(furrr)


library(purrr)
library(dplyr)
library(logger)

aggr_results <- function(results_df) {
  reps <- length(results_df)
  logger::log_info("Experiments results will be averaged over [{reps}] repetitions.")
  purrr::reduce(results_df, function(acc, x) {
    acc$best <- (acc$best + x$best)
    return(acc)
  }) |>
    dplyr::mutate(
      best = best / reps
    )
}

run_solver_ <- function(problem, solver, dim) {
  x0 <- rep(100, dim)
  result <- solver(x0, problem)
  tibble::tibble(
    t = seq_len(result$n.iters),
    best = c(result$diagnostic$bestVal),
    solver = result$label,
    dim = dim
  )
}

do_exp_ <- function(problem, solver, dim, rep_num) {
  results <- list()
  logger::log_info("Numerical experiments for solver [{solver$name}] will be repeated [{rep_num}] times for problem [{problem$name}] with dimensionality [{dim}].") # nolint
  for (rep in 1:rep_num) {
    logger::log_info("Running [{rep}] repetition of solver [{solver$name}] for problem [{problem$name}] with dimension [{dim}].")
    results[[rep]] <- run_solver_(problem$fn, solver$solver, dim)
  }
  results
}

generate_output_path_ <- function(solver, problem, exp_spec, result_dir) {
  here::here(result_dir, stringr::str_glue("{solver$name}-{problem$name}-{exp_spec$dim}D.csv")) # nolint
}

run_exp <- function(problems, solvers, exp_spec, result_dir) {
  for(s in solvers) {
    for (problem in problems) {
      results <- do_exp_(problem, s, exp_spec$dim, exp_spec$rep)
      output_path <- generate_output_path_(s, problem, exp_spec, result_dir)
      aggregated <- aggr_results(results)
      readr::write_csv(aggregated, output_path)
    }
  }
}
