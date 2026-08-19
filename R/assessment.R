# Assessment ---------------------------------------------------------------

.pls_alpha <- function(X) {
  X <- as.matrix(X); k <- ncol(X)
  if (k < 2) return(NA_real_)
  v <- apply(X, 2, stats::var, na.rm = TRUE)
  tv <- stats::var(rowSums(X), na.rm = TRUE)
  k/(k-1) * (1 - sum(v)/tv)
}

.pls_rhoc <- function(loadings) {
  num <- sum(loadings)^2
  den <- num + sum(1 - loadings^2)
  num / den
}

.pls_htmt <- function(data, blocks) {
  cn <- names(blocks); M <- matrix(1, length(cn), length(cn), dimnames = list(cn, cn))
  C <- abs(stats::cor(data[, unique(unlist(blocks)), drop = FALSE], use = "pairwise.complete.obs"))
  for (i in seq_along(cn)) for (j in seq_along(cn)) if (i < j) {
    xi <- blocks[[i]]; xj <- blocks[[j]]
    hetero <- C[xi, xj, drop = FALSE]
    mono_i <- C[xi, xi, drop = FALSE]; mono_j <- C[xj, xj, drop = FALSE]
    mi <- mono_i[upper.tri(mono_i)]; mj <- mono_j[upper.tri(mono_j)]
    den <- sqrt(mean(mi, na.rm = TRUE) * mean(mj, na.rm = TRUE))
    M[i,j] <- M[j,i] <- mean(hetero, na.rm = TRUE) / den
  }
  M
}

#' Assess the measurement model
#' @param fit Fitted model.
#' @return List with loadings, reliability, AVE, HTMT, and formative VIF where applicable.
#' @export
pls_measurement_assess <- function(fit) {
  .pls_check_fit(fit)
  if (fit$engine == "cSEM") return(cSEM::assess(fit$backend))
  if (fit$engine != "native") .pls_abort("Unified measurement assessment is currently guaranteed for native and cSEM engines.")
  d <- fit$native$data_used
  constructs <- fit$model$measurement$constructs
  rel <- list(); vifs <- list(); blocks <- list()
  for (cst in constructs) {
    inds <- cst$indicators; blocks[[cst$name]] <- inds
    ld <- fit$native$loadings$loading[fit$native$loadings$construct == cst$name]
    if (cst$type == "reflective") {
      rel[[cst$name]] <- data.frame(construct = cst$name, alpha = .pls_alpha(d[, inds, drop = FALSE]),
                                    rhoC = .pls_rhoc(ld), AVE = mean(ld^2, na.rm = TRUE), stringsAsFactors = FALSE)
    } else {
      if (length(inds) > 1) {
        C <- stats::cor(d[, inds, drop = FALSE], use = "pairwise.complete.obs")
        inv <- try(.pls_safe_solve(C), silent = TRUE)
        vif <- if (inherits(inv, "try-error")) rep(NA_real_, length(inds)) else diag(inv)
        vifs[[cst$name]] <- data.frame(construct = cst$name, indicator = inds, VIF = vif, stringsAsFactors = FALSE)
      }
    }
  }
  reflective_blocks <- blocks[vapply(constructs, function(x) x$type == "reflective", logical(1))]
  htmt <- if (length(reflective_blocks) >= 2) .pls_htmt(d, reflective_blocks) else NULL
  list(loadings = fit$native$loadings, weights = fit$native$weights,
       reliability = if(length(rel)) do.call(rbind, rel) else data.frame(),
       htmt = htmt, formative_vif = if(length(vifs)) do.call(rbind, vifs) else data.frame())
}

#' Assess the structural model
#' @param fit Fitted model.
#' @return List with paths, R2, f2 effect sizes, and construct-score VIF.
#' @export
pls_structural_assess <- function(fit) {
  .pls_check_fit(fit)
  if (fit$engine == "cSEM") return(cSEM::assess(fit$backend))
  if (fit$engine != "native") .pls_abort("Unified structural assessment is currently guaranteed for native and cSEM engines.")
  scores <- as.data.frame(fit$native$scores); paths <- fit$native$paths
  f2rows <- list(); vifrows <- list()
  for (to in unique(paths$to)) {
    from <- unique(paths$from[paths$to == to])
    full <- stats::lm(stats::reformulate(from, response = to), data = scores)
    r2f <- summary(full)$r.squared
    if (length(from) > 1) {
      C <- stats::cor(scores[, from, drop = FALSE]); inv <- try(.pls_safe_solve(C), silent = TRUE)
      v <- if (inherits(inv,"try-error")) rep(NA_real_, length(from)) else diag(inv)
    } else v <- 1
    vifrows[[to]] <- data.frame(from = from, to = to, VIF = v, stringsAsFactors = FALSE)
    for (x in from) {
      redfrom <- setdiff(from, x)
      r2r <- if (!length(redfrom)) 0 else summary(stats::lm(stats::reformulate(redfrom, response = to), data = scores))$r.squared
      f2rows[[paste(to,x)]] <- data.frame(from=x,to=to,f2=(r2f-r2r)/(1-r2f), stringsAsFactors = FALSE)
    }
  }
  list(paths = paths, r2 = data.frame(construct = names(fit$native$r2), R2 = as.numeric(fit$native$r2)),
       f2 = if(length(f2rows)) do.call(rbind,f2rows) else data.frame(),
       vif = if(length(vifrows)) do.call(rbind,vifrows) else data.frame())
}

#' Assess measurement and structural results
#' @param fit Fitted model.
#' @return Combined assessment list.
#' @export
pls_assess <- function(fit) {
  list(measurement = pls_measurement_assess(fit), structural = pls_structural_assess(fit))
}
