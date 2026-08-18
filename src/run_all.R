#============================================================================
# Orchestrator: run all data download tasks and report results
#
# Two modes:
#   Rscript src/run_all.R          — CI mode: only public-API tasks
#   Rscript src/run_all.R --vpn    — Local mode: also runs BM UA (needs VPN)
#
# Each task runs in a tryCatch — one failure doesn't block others.
#============================================================================

args <- commandArgs(trailingOnly = TRUE)
include_vpn <- "--vpn" %in% args

message("=== Data Update Pipeline ===")
message("Mode:    ", if (include_vpn) "LOCAL (incl. VPN tasks)" else "CI (public APIs only)")
message("Started: ", Sys.time())
message("")

results <- list()

# ── CI tasks (public APIs — no VPN required) ─────────────────────────────────

message("\u2500\u2500 [1] DAM UA \u2500\u2500")
results$dam_ua <- tryCatch({
  source("src/tasks/task_dam_ua.R")
  task_dam_ua()
}, error = function(e) {
  list(task = "dam_ua", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [2] Solar UA \u2500\u2500")
results$solar_ua <- tryCatch({
  source("src/tasks/task_solar_ua.R")
  task_solar_ua()
}, error = function(e) {
  list(task = "solar_ua", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [3] Wind UA \u2500\u2500")
results$wind_ua <- tryCatch({
  source("src/tasks/task_wind_ua.R")
  task_wind_ua()
}, error = function(e) {
  list(task = "wind_ua", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [4] EU gen + load + BM \u2500\u2500")
results$eu <- tryCatch({
  source("src/tasks/task_eu.R")
  task_eu()
}, error = function(e) {
  list(task = "eu_data", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [5] EU DAM prices \u2500\u2500")
results$dam_eu <- tryCatch({
  source("src/tasks/task_dam_eu.R")
  task_dam_eu()
}, error = function(e) {
  list(task = "dam_eu", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [6] EU transmission flows \u2500\u2500")
results$transm_eu <- tryCatch({
  source("src/tasks/task_transm_eu.R")
  task_transm_eu()
}, error = function(e) {
  list(task = "transm_eu", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [7] Market prices (TTF gas + EUA carbon) \u2500\u2500")
results$market_prices <- tryCatch({
  source("src/tasks/task_market_prices.R")
  task_market_prices()
}, error = function(e) {
  list(task = "market_prices", status = "error", message = e$message)
})
message("")

message("\u2500\u2500 [8] UEEX gas prices (UA) \u2500\u2500")
results$ueex_gas <- tryCatch({
  source("src/tasks/task_ueex_gas.R")
  task_ueex_gas()
}, error = function(e) {
  list(task = "ueex_gas", status = "error", message = e$message)
})
message("")

message("── [9] PSE Poland (PV curtailment + KSE load) ──")
results$pse_pl <- tryCatch({
  source("src/tasks/task_pse_pl.R")
  task_pse_pl()
}, error = function(e) {
  list(task = "pse_pl", status = "error", message = e$message)
})
message("")

# ── VPN tasks (local only) ───────────────────────────────────────────────────

if (include_vpn) {
  message("\u2500\u2500 [10] BM UA (VPN) \u2500\u2500")
  results$bm_ua <- tryCatch({
    source("src/tasks/task_bm_ua.R")
    task_bm_ua()
  }, error = function(e) {
    list(task = "bm_ua", status = "error", message = e$message)
  })
  message("")

  message("\u2500\u2500 [11] Imbalance UA (VPN) \u2500\u2500")
  results$imbalance_ua <- tryCatch({
    source("src/tasks/task_imbalance_ua.R")
    task_imbalance_ua()
  }, error = function(e) {
    list(task = "imbalance_ua", status = "error", message = e$message)
  })
  message("")
}

# ── Transform (always last) ─────────────────────────────────────────────────

n_task <- if (include_vpn) "[12]" else "[10]"
message("\u2500\u2500 ", n_task, " Transform \u2500\u2500")
results$transform <- tryCatch({
  source("src/tasks/task_transform.R")
  task_transform()
}, error = function(e) {
  list(task = "transform", status = "error", message = e$message)
})
message("")

# ── Summary ──────────────────────────────────────────────────────────────────
message("=== Pipeline Summary ===")
for (r in results) {
  icon <- switch(r$status,
    success = "\u2713",
    skipped = "\u2013",
    warning = "\u26a0",
    error   = "\u2717",
    "?"
  )
  message("  ", icon, " ", r$task, ": ", r$status, " \u2014 ", r$message)
}

statuses <- sapply(results, \(r) r$status)
n_err <- sum(statuses == "error")

message("")
message("Finished: ", Sys.time())

if (n_err > 0) {
  message(n_err, " task(s) failed \u2014 see above for details")
}
