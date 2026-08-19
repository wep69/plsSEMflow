# Simulation and validation ------------------------------------------------

#' Simulate agronomic PLS-SEM teaching data from a model
#'
#' Generates continuous manifest variables from a recursive construct model.
#' It is intended for method validation, teaching, power/sensitivity studies,
#' and frozen regression tests, not for replacing real agronomic observations.
#'
#' @param model A `plssem_model` with acyclic direct/mediation paths.
#' @param n Number of observations.
#' @param path_values Optional named numeric vector using labels such as
#'   `"SOIL -> NUTR"`. Unspecified paths default to `0.35`.
#' @param loading Loading magnitude for reflective indicators.
#' @param weight Weight magnitude for composite/formative indicators.
#' @param indicator_noise Standard deviation of indicator noise.
#' @param structural_noise Standard deviation of structural disturbances.
#' @param seed Optional random seed.
#' @return A data frame with manifest variables and latent-score attributes.
#' @export
pls_simulate <- function(model, n = 300L, path_values = NULL, loading = 0.80,
                         weight = 0.55, indicator_noise = 0.60,
                         structural_noise = 1, seed = NULL) {
  .pls_validate_model(model)
  n <- as.integer(n)
  if (n < 20L) .pls_abort("n must be at least 20 for this simulation helper.")
  if (!is.null(seed)) set.seed(seed)
  cn <- .pls_construct_names(model)
  p <- .pls_paths_df(model)
  A <- matrix(0, length(cn), length(cn), dimnames = list(cn, cn))
  if (nrow(p)) {
    for (i in seq_len(nrow(p))) A[p$to[i], p$from[i]] <- 1
  }
  # Kahn topological ordering; recursive models only.
  indeg <- rowSums(A != 0)
  queue <- cn[indeg == 0]
  ord <- character()
  while (length(queue)) {
    u <- queue[1]; queue <- queue[-1]
    ord <- c(ord, u)
    children <- rownames(A)[A[, u] != 0]
    for (v in children) {
      indeg[v] <- indeg[v] - 1
      if (indeg[v] == 0) queue <- c(queue, v)
    }
  }
  if (length(ord) != length(cn)) .pls_abort("pls_simulate() currently requires an acyclic structural model.")

  eta <- matrix(NA_real_, n, length(cn), dimnames = list(NULL, cn))
  for (k in ord) {
    parents <- colnames(A)[A[k, ] != 0]
    if (!length(parents)) {
      eta[, k] <- stats::rnorm(n)
    } else {
      pred <- rep(0, n)
      for (pa in parents) {
        key <- paste(pa, "->", k)
        beta <- if (!is.null(path_values) && key %in% names(path_values)) path_values[[key]] else 0.35
        pred <- pred + beta * eta[, pa]
      }
      eta[, k] <- pred + stats::rnorm(n, sd = structural_noise)
      eta[, k] <- .pls_std_vec(eta[, k])
    }
  }

  out <- list()
  for (cs in model$measurement$constructs) {
    if (identical(cs$type, "higher_order")) next
    inds <- cs$indicators
    for (j in seq_along(inds)) {
      coef <- if (identical(cs$type, "reflective")) loading else weight
      # Give indicators mild heterogeneous strength to avoid artificial equality.
      coef_j <- max(0.20, min(0.95, coef + (j - (length(inds)+1)/2) * 0.025))
      out[[inds[j]]] <- coef_j * eta[, cs$name] + stats::rnorm(n, sd = indicator_noise)
    }
  }
  dat <- as.data.frame(out, check.names = FALSE)
  attr(dat, "latent_scores") <- as.data.frame(eta)
  attr(dat, "simulation") <- list(n=n, path_values=path_values, loading=loading,
                                  weight=weight, indicator_noise=indicator_noise,
                                  structural_noise=structural_noise, seed=seed)
  dat
}

#' Validate path recovery in a frozen simulation scenario
#'
#' @param model Model specification.
#' @param n Number of simulated observations.
#' @param path_values Named true path coefficients.
#' @param R Bootstrap replications for interval coverage checks. Use a small
#'   value in unit tests and a larger value in method-validation studies.
#' @param seed Seed.
#' @param level Confidence level.
#' @return A validation object with path bias and bootstrap coverage.
#' @export
pls_validation_case <- function(model, n = 500L, path_values, R = 499L,
                                seed = 260819L, level = 0.95) {
  dat <- pls_simulate(model, n=n, path_values=path_values, seed=seed)
  fit <- pls_fit(model, dat, engine="native")
  b <- pls_bootstrap(fit, R=R, seed=seed+1L, level=level)
  p <- fit$native$paths
  p$key <- paste(p$from, "->", p$to)
  truth <- unname(path_values[p$key])
  bt <- b$table[b$table$component == "path", ]
  bt$key <- sub(" -> ", " -> ", bt$label, fixed=TRUE)
  m <- match(p$key, bt$key)
  tab <- data.frame(
    path = p$key,
    truth = truth,
    estimate = p$estimate,
    bias = p$estimate - truth,
    conf_low = bt$conf_low[m],
    conf_high = bt$conf_high[m],
    covered = truth >= bt$conf_low[m] & truth <= bt$conf_high[m],
    stringsAsFactors = FALSE
  )
  structure(list(data=dat, fit=fit, bootstrap=b, table=tab,
                 design=list(n=n,R=R,seed=seed,level=level)),
            class="plssem_validation_case")
}

#' Compare numerically comparable engines
#'
#' Fits the same model using requested available engines and reports absolute
#' differences in structural path estimates where unified extraction is
#' possible. The native engine is the reference by default.
#'
#' @param model Model specification.
#' @param data Data frame.
#' @param engines Engines to compare.
#' @param tolerance Absolute numerical tolerance for a validation flag.
#' @return A cross-engine validation object.
#' @export
pls_validate_cross_engine <- function(model, data, engines=c("native","cSEM"), tolerance=0.05) {
  cmp <- pls_compare_engines(model, data, engines=engines)
  ref <- cmp$fits[["native"]]
  if (is.null(ref)) .pls_abort("Cross-engine validation currently requires 'native' among successful engines.")
  refp <- ref$native$paths
  refp$key <- paste(refp$from, "->", refp$to)
  rows <- list()
  for (e in setdiff(names(cmp$fits), "native")) {
    f <- cmp$fits[[e]]
    ext <- .pls_extract_backend_paths(f)
    if (is.null(ext)) next
    ext$key <- paste(ext$from, "->", ext$to)
    keys <- intersect(refp$key, ext$key)
    if (!length(keys)) next
    rr <- refp[match(keys, refp$key), ]
    ee <- ext[match(keys, ext$key), ]
    rows[[e]] <- data.frame(
      engine=e, path=keys, native=rr$estimate, other=ee$estimate,
      abs_diff=abs(rr$estimate-ee$estimate),
      within_tolerance=abs(rr$estimate-ee$estimate)<=tolerance,
      stringsAsFactors=FALSE
    )
  }
  structure(list(status=cmp$status, comparison=if(length(rows)) do.call(rbind,rows) else data.frame(),
                 tolerance=tolerance), class="plssem_cross_validation")
}

.pls_extract_backend_paths <- function(fit) {
  if (fit$engine == "native") return(fit$native$paths[,c("from","to","estimate")])
  if (fit$engine == "plspm") {
    pc <- try(fit$backend$path_coefs, silent=TRUE)
    if (!inherits(pc,"try-error") && is.matrix(pc)) {
      z <- which(pc != 0, arr.ind=TRUE)
      return(data.frame(from=colnames(pc)[z[,2]], to=rownames(pc)[z[,1]], estimate=pc[z]))
    }
  }
  # cSEM result structures can vary by release. Prefer its canonical estimates table when present.
  if (fit$engine == "cSEM") {
    obj <- fit$backend
    cand <- try(obj$Estimates$Path_estimates, silent=TRUE)
    if (!inherits(cand,"try-error") && !is.null(cand)) {
      x <- as.data.frame(cand)
      nms <- tolower(names(x))
      estcol <- which(nms %in% c("estimate","estimates","value"))[1]
      namecol <- which(nms %in% c("name","path","relationship"))[1]
      if (length(estcol) && length(namecol)) {
        lab <- as.character(x[[namecol]])
        parts <- strsplit(gsub("~", "->", lab, fixed=TRUE), "->", fixed=TRUE)
        ok <- lengths(parts) == 2
        return(data.frame(from=trimws(vapply(parts[ok],`[`,character(1),1)),
                          to=trimws(vapply(parts[ok],`[`,character(1),2)),
                          estimate=as.numeric(x[[estcol]][ok])))
      }
    }
  }
  NULL
}

#' Print a simulation validation case
#' @param x A `plssem_validation_case`.
#' @param ... Unused.
#' @export
print.plssem_validation_case <- function(x, ...) {
  cat("<plsSEMflow simulation validation case>\n")
  cat("n =", x$design$n, "| bootstrap R =", x$design$R, "\n")
  print(x$table, row.names=FALSE)
  invisible(x)
}

#' Print a cross-engine validation result
#' @param x A `plssem_cross_validation`.
#' @param ... Unused.
#' @export
print.plssem_cross_validation <- function(x, ...) {
  cat("<plsSEMflow cross-engine validation>\n")
  print(x$status, row.names=FALSE)
  if (nrow(x$comparison)) print(x$comparison, row.names=FALSE)
  invisible(x)
}
