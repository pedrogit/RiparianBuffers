#' Convert terra objects inside a list so they can be saved and sent to workers
#'
#' terra vectors and rasters hold external pointers and cannot be serialised
#' directly. `terra::wrap` turns them into plain data. Lists are processed
#' recursively; everything else is returned unchanged.
packSpat <- function(x) {
  if (inherits(x, c("SpatVector", "SpatRaster"))) {
    return(terra::wrap(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, packSpat))
  }
  x
}

#' Undo `packSpat` (runs inside the workers)
unpackSpat <- function(x) {
  if (inherits(x, c("PackedSpatVector", "PackedSpatRaster"))) {
    return(terra::unwrap(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, unpackSpat))
  }
  x
}

#' Run a list of independent jobs, in this process or in separate R processes
#'
#' Each job is run as `run(ctx, job)`. With `workers = 1` the jobs run here, one
#' after the other. With more workers, that many background R processes are
#' started (package callr). Each worker reads `ctx` once from a file, so the
#' large inputs are not sent again with every job, and the next job goes to
#' whichever worker is free first. Results are handed to `onResult(i, value)` in
#' this process, in the order they finish (not in job order); `onResult` is the
#' only place where shared output, such as the raster file, is written.
#'
#' `helpers` names the module functions that `run` calls; they are copied to
#' the workers, which do not load the module. If a job fails, or a worker
#' process dies, every worker is stopped and the error names the job.
#' `tmpDir` holds the worker's temporary files and is cleaned up afterwards.
runJobs <- function(
  jobs,
  run,
  ctx,
  helpers = character(),
  workers = 1L,
  tmpDir,
  onResult,
  what = "job"
) {
  n <- length(jobs)
  if (!n) {
    return(invisible(NULL))
  }
  workers <- min(as.integer(workers), n)
  if (workers <= 1L) {
    for (i in seq_len(n)) {
      onResult(i, run(ctx, jobs[[i]]))
    }
    return(invisible(NULL))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Running with workers > 1 requires package 'callr'.")
  }
  # Workers do not load the module, so copy the functions they need. Closures
  # would drag their whole enclosing environment along, so reset it.
  fns <- mget(
    unique(c(helpers, "unpackSpat")),
    envir = environment(runJobs),
    inherits = TRUE
  )
  fns <- lapply(fns, function(f) {
    environment(f) <- globalenv()
    f
  })
  runFn <- run
  environment(runFn) <- globalenv()
  startFn <- function(ctxFile, fns, runFn) {
    list2env(fns, globalenv())
    assign(".rb_run", runFn, globalenv())
    assign(".rb_ctx", unpackSpat(readRDS(ctxFile)), globalenv())
    invisible(TRUE)
  }
  jobFn <- function(job) {
    get(".rb_run", globalenv())(get(".rb_ctx", globalenv()), job)
  }
  environment(startFn) <- globalenv()
  environment(jobFn) <- globalenv()

  dir.create(tmpDir, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("riparian_workers_", tmpdir = tmpDir)
  dir.create(work)
  sessions <- list()
  on.exit(
    {
      for (s in sessions) {
        try(s$kill(), silent = TRUE)
      }
      unlink(work, recursive = TRUE)
    },
    add = TRUE
  )
  ctxFile <- file.path(work, "ctx.rds")
  saveRDS(packSpat(ctx), ctxFile)
  options <- callr::r_session_options(
    env = c(callr::rcmd_safe_env(), TEMP = work, TMP = work, TMPDIR = work)
  )
  failure <- function(r, label) {
    e <- r$error
    reason <- if (!is.null(e$parent)) {
      conditionMessage(e$parent)
    } else if (!is.null(e)) {
      conditionMessage(e)
    } else {
      r$message
    }
    stop(label, ": ", reason, call. = FALSE)
  }
  for (w in seq_len(workers)) {
    # callr waits only 3 s by default for a worker to start, which is too short
    # on a busy machine or with many workers.
    sessions[[w]] <- callr::r_session$new(
      options,
      wait = TRUE,
      wait_timeout = 120000
    )
    sessions[[w]]$call(startFn, list(ctxFile, fns, runFn))
  }
  # Every worker loads the inputs at the same time; wait for all of them.
  for (w in seq_len(workers)) {
    sessions[[w]]$poll_process(-1)
    r <- sessions[[w]]$read()
    if (!is.null(r$error) || !identical(r$code, 200L)) {
      failure(r, paste0("Worker ", w, " could not load its inputs"))
    }
  }
  running <- rep(NA_integer_, workers)
  nextJob <- 1L
  finished <- 0L
  while (finished < n) {
    for (w in seq_len(workers)) {
      if (is.na(running[w]) && nextJob <= n) {
        sessions[[w]]$call(jobFn, list(jobs[[nextJob]]))
        running[w] <- nextJob
        nextJob <- nextJob + 1L
      }
    }
    progressed <- FALSE
    for (w in which(!is.na(running))) {
      if (!identical(sessions[[w]]$poll_process(0), "ready")) {
        next
      }
      r <- sessions[[w]]$read()
      i <- running[w]
      running[w] <- NA_integer_
      label <- paste0("Worker ", w, " failed on ", what, " ", i)
      if (!is.null(r$error) || !identical(r$code, 200L)) {
        failure(r, label)
      }
      # Give this worker its next job before spending time on the result.
      if (nextJob <= n) {
        sessions[[w]]$call(jobFn, list(jobs[[nextJob]]))
        running[w] <- nextJob
        nextJob <- nextJob + 1L
      }
      onResult(i, r$result)
      finished <- finished + 1L
      progressed <- TRUE
    }
    if (!progressed) {
      Sys.sleep(0.1)
    }
  }
  invisible(NULL)
}
