# Native PLS-PM engine ----------------------------------------------------

.pls_native_prepare <- function(model, data, na_action = c("complete", "fail")) {
  na_action <- match.arg(na_action)
  .pls_validate_model(model, data)
  manifest <- .pls_manifest_names(model)
  .pls_check_numeric(data, manifest)
  d <- data[, manifest, drop = FALSE]
  if (anyNA(d)) {
    if (na_action == "fail") .pls_abort("Missing values detected. Use na_action='complete' or an optional backend that supports the intended missing-data strategy.")
    keep <- stats::complete.cases(d)
    data <- data[keep, , drop = FALSE]
    d <- d[keep, , drop = FALSE]
  }
  sc <- .pls_standardize(d)
  list(data = data, X = sc$z, center = sc$center, scale = sc$scale)
}

.pls_native_fit <- function(model, data, max_iter = 300L, tol = 1e-07,
                            scheme = c("path", "centroid"), na_action = "complete") {
  scheme <- match.arg(scheme)
  prep <- .pls_native_prepare(model, data, na_action)
  X <- prep$X
  constructs <- model$measurement$constructs
  # Higher-order constructs are delegated to specialized backends.
  if (any(vapply(constructs, function(x) identical(x$type, "higher_order"), logical(1)))) {
    .pls_abort("The native engine does not estimate higher-order constructs. Use engine='cSEM' or another compatible backend.")
  }
  J <- length(constructs)
  cn <- vapply(constructs, `[[`, character(1), "name")
  blocks <- lapply(constructs, function(cst) match(cst$indicators, colnames(X)))
  names(blocks) <- cn
  W <- vector("list", J); names(W) <- cn
  for (j in seq_len(J)) {
    k <- length(blocks[[j]])
    w <- rep(1 / sqrt(k), k)
    W[[j]] <- w / sqrt(sum(w^2))
  }
  paths <- .pls_paths_df(model)
  A <- matrix(0, J, J, dimnames = list(cn, cn))
  if (nrow(paths)) for (i in seq_len(nrow(paths))) A[paths$from[i], paths$to[i]] <- 1
  connected <- (A + t(A)) > 0
  iter <- 0L; delta <- Inf
  Y <- matrix(NA_real_, nrow(X), J, dimnames = list(NULL, cn))
  while (iter < max_iter && delta > tol) {
    iter <- iter + 1L
    for (j in seq_len(J)) Y[, j] <- .pls_std_vec(drop(X[, blocks[[j]], drop = FALSE] %*% W[[j]]))
    Z <- matrix(0, nrow(Y), J, dimnames = dimnames(Y))
    for (j in seq_len(J)) {
      neigh <- which(connected[j, ])
      if (!length(neigh)) { Z[, j] <- Y[, j]; next }
      if (scheme == "centroid") {
        signs <- sign(stats::cor(Y[, j], Y[, neigh, drop = FALSE], use = "pairwise.complete.obs"))
        signs[!is.finite(signs) | signs == 0] <- 1
        Z[, j] <- drop(Y[, neigh, drop = FALSE] %*% signs)
      } else {
        # Path weighting: predecessors use regression coefficients; successors use correlations.
        pred <- which(A[, j] != 0)
        succ <- which(A[j, ] != 0)
        z <- rep(0, nrow(Y))
        if (length(pred)) {
          design <- cbind(1, Y[, pred, drop = FALSE])
          b <- .pls_safe_solve(crossprod(design), crossprod(design, Y[, j]))[-1]
          z <- z + drop(Y[, pred, drop = FALSE] %*% b)
        }
        if (length(succ)) {
          cr <- as.numeric(stats::cor(Y[, j], Y[, succ, drop = FALSE], use = "pairwise.complete.obs"))
          cr[!is.finite(cr)] <- 0
          z <- z + drop(Y[, succ, drop = FALSE] %*% cr)
        }
        if (all(abs(z) < .Machine$double.eps)) z <- Y[, j]
        Z[, j] <- z
      }
      Z[, j] <- .pls_std_vec(Z[, j])
    }
    Wnew <- W
    for (j in seq_len(J)) {
      Xj <- X[, blocks[[j]], drop = FALSE]
      mode <- constructs[[j]]$mode
      if (identical(mode, "A")) {
        w <- as.numeric(stats::cor(Xj, Z[, j], use = "pairwise.complete.obs"))
      } else {
        XtX <- crossprod(Xj)
        w <- as.numeric(.pls_safe_solve(XtX, crossprod(Xj, Z[, j])))
      }
      w[!is.finite(w)] <- 0
      if (sqrt(sum(w^2)) <= .Machine$double.eps) w <- rep(1, ncol(Xj))
      Wnew[[j]] <- w / sqrt(sum(w^2))
    }
    delta <- max(unlist(Map(function(a, b) max(abs(a - b)), W, Wnew)))
    W <- Wnew
  }
  for (j in seq_len(J)) Y[, j] <- .pls_std_vec(drop(X[, blocks[[j]], drop = FALSE] %*% W[[j]]))

  # Align sign so the dominant loading is positive.
  for (j in seq_len(J)) {
    ld <- as.numeric(stats::cor(X[, blocks[[j]], drop = FALSE], Y[, j], use = "pairwise.complete.obs"))
    if (length(ld) && ld[which.max(abs(ld))] < 0) {
      Y[, j] <- -Y[, j]; W[[j]] <- -W[[j]]
    }
  }

  path_rows <- list(); r2 <- setNames(rep(NA_real_, J), cn)
  for (to in unique(paths$to)) {
    from <- unique(paths$from[paths$to == to])
    design <- cbind(1, Y[, from, drop = FALSE])
    fit <- stats::lm.fit(design, Y[, to])
    co <- fit$coefficients[-1]
    pred <- drop(design %*% fit$coefficients)
    r2[to] <- 1 - sum((Y[, to] - pred)^2) / sum((Y[, to] - mean(Y[, to]))^2)
    path_rows[[to]] <- data.frame(from = from, to = to, estimate = as.numeric(co), stringsAsFactors = FALSE)
  }
  path_df <- if (length(path_rows)) do.call(rbind, path_rows) else data.frame(from=character(),to=character(),estimate=numeric())
  rownames(path_df) <- NULL

  load_rows <- list(); weight_rows <- list()
  for (j in seq_len(J)) {
    inds <- constructs[[j]]$indicators
    Xj <- X[, blocks[[j]], drop = FALSE]
    ld <- as.numeric(stats::cor(Xj, Y[, j], use = "pairwise.complete.obs"))
    load_rows[[j]] <- data.frame(construct = cn[j], indicator = inds, loading = ld, stringsAsFactors = FALSE)
    weight_rows[[j]] <- data.frame(construct = cn[j], indicator = inds, weight = W[[j]], stringsAsFactors = FALSE)
  }
  list(
    scores = as.data.frame(Y),
    paths = path_df,
    loadings = do.call(rbind, load_rows),
    weights = do.call(rbind, weight_rows),
    r2 = r2,
    iterations = iter,
    converged = is.finite(delta) && delta <= tol,
    final_change = delta,
    preprocessing = list(center = prep$center, scale = prep$scale, n_used = nrow(X)),
    data_used = prep$data
  )
}
