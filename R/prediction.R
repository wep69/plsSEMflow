# Prediction ----------------------------------------------------------------

.pls_apply_scores <- function(fit, newdata) {
  pre <- fit$native$preprocessing
  manifests <- names(pre$center)
  X <- as.matrix(newdata[, manifests, drop = FALSE])
  X <- sweep(sweep(X, 2, pre$center, "-"), 2, pre$scale, "/")
  cs <- fit$model$measurement$constructs
  S <- matrix(NA_real_, nrow(X), length(cs), dimnames = list(NULL, vapply(cs, `[[`, character(1), "name")))
  for (j in seq_along(cs)) {
    wtab <- fit$native$weights[fit$native$weights$construct == cs[[j]]$name, ]
    w <- setNames(wtab$weight, wtab$indicator)
    S[,j] <- drop(X[, names(w), drop=FALSE] %*% w)
  }
  as.data.frame(S)
}

#' Cross-validated construct prediction
#'
#' @param fit Native fitted model.
#' @param folds Number of folds.
#' @param repeats Number of repeated random fold assignments.
#' @param seed Optional seed.
#' @return Prediction metrics at the construct-score level.
#' @export
pls_predict <- function(fit, folds = 10L, repeats = 10L, seed = NULL) {
  .pls_check_fit(fit)
  if (fit$engine == "cSEM") return(stats::predict(fit$backend))
  if (fit$engine != "native") .pls_abort("Unified prediction currently supports native and cSEM engines.")
  n <- nrow(fit$data); folds <- min(as.integer(folds), n); repeats <- as.integer(repeats)
  .pls_st <- .pls_rng_save(); on.exit(.pls_rng_restore(.pls_st), add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  targets <- .pls_endogenous(fit$model); rows <- list(); rr <- 0L
  for (r in seq_len(repeats)) {
    fold_id <- sample(rep(seq_len(folds), length.out = n))
    for (k in seq_len(folds)) {
      tr <- which(fold_id != k); te <- which(fold_id == k)
      ftr <- .pls_native_fit(fit$model, fit$data[tr,,drop=FALSE])
      fobj <- structure(list(model=fit$model,data=fit$data[tr,,drop=FALSE],engine="native",native=ftr),class="plssem_fit")
      Str <- ftr$scores; Ste <- .pls_apply_scores(fobj, fit$data[te,,drop=FALSE])
      # Reference outcome scores use a model fitted on all training weights applied to test indicators.
      for (to in targets) {
        from <- ftr$paths$from[ftr$paths$to == to]
        b <- ftr$paths$estimate[ftr$paths$to == to]
        pred <- drop(as.matrix(Ste[,from,drop=FALSE]) %*% b)
        obs <- Ste[[to]]
        # Linear benchmark on manifest-derived construct scores.
        lmfit <- stats::lm(stats::reformulate(from, response=to), data=Str)
        lmp <- stats::predict(lmfit, newdata=Ste)
        rr <- rr+1L
        rows[[rr]] <- data.frame(rep=r,fold=k,construct=to,
          RMSE=sqrt(mean((obs-pred)^2)), MAE=mean(abs(obs-pred)),
          LM_RMSE=sqrt(mean((obs-lmp)^2)), LM_MAE=mean(abs(obs-lmp)), stringsAsFactors=FALSE)
      }
    }
  }
  raw <- do.call(rbind, rows)
  summary <- stats::aggregate(cbind(RMSE,MAE,LM_RMSE,LM_MAE) ~ construct, raw, mean)
  summary$better_RMSE <- summary$RMSE < summary$LM_RMSE
  summary$better_MAE <- summary$MAE < summary$LM_MAE
  structure(list(summary=summary, folds=raw, settings=list(folds=folds,repeats=repeats,seed=seed)), class="plssem_prediction")
}

#' Print prediction results
#' @param x Prediction object.
#' @param ... Unused.
#' @export
print.plssem_prediction <- function(x, ...) { print(x$summary, row.names=FALSE); invisible(x) }
