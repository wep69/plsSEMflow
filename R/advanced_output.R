# Advanced publication outputs --------------------------------------------

#' Publication-ready coefficient plot for advanced PLS-SEM results
#'
#' Accepts results produced by `pls_moderation_test()`, `pls_nonlinear_test()`,
#' or another data frame containing estimates and confidence limits.
#'
#' @param x Data frame with `estimate` and optional `conf_low`, `conf_high`.
#' @param term Column containing coefficient labels.
#' @param title Optional title.
#' @param file Optional PDF/SVG/TIFF/PNG output file.
#' @param width,height Figure size in inches.
#' @param dpi Raster resolution.
#' @return A ggplot object.
#' @export
pls_plot_coefficients <- function(x, term = NULL, title = NULL, file = NULL,
                                  width = 8, height = 5, dpi = 600) {
  .pls_require("ggplot2", "for coefficient plots.")
  if (!is.data.frame(x) || !"estimate" %in% names(x)) .pls_abort("x must be a coefficient data frame containing 'estimate'.")
  if (is.null(term)) term <- intersect(c("term","path","label","effect"), names(x))[1]
  if (!length(term) || is.na(term)) .pls_abort("Could not identify a coefficient-label column; supply 'term'.")
  dat <- x
  dat$.term <- as.character(dat[[term]])
  gg <- ggplot2::ggplot(dat, ggplot2::aes(x=stats::reorder(.term, estimate), y=estimate)) +
    ggplot2::geom_point(size=2) +
    ggplot2::geom_hline(yintercept=0, linetype=2) +
    ggplot2::coord_flip() +
    ggplot2::labs(x=NULL, y="Coefficient", title=title) +
    ggplot2::theme_minimal(base_size=11)
  if (all(c("conf_low","conf_high") %in% names(dat))) {
    gg <- gg + ggplot2::geom_errorbar(ggplot2::aes(ymin=conf_low,ymax=conf_high), width=.15)
  }
  if (!is.null(file)) ggplot2::ggsave(file, gg, width=width, height=height, dpi=dpi, units="in")
  gg
}

#' Plot native two-group MGA differences with bootstrap intervals
#'
#' @param x Result from `pls_mga()`.
#' @param file Optional output filename.
#' @param width,height Size in inches.
#' @param dpi Raster resolution.
#' @return A ggplot object.
#' @export
pls_plot_mga <- function(x, file=NULL, width=8, height=5, dpi=600) {
  if (!inherits(x,"plssem_mga")) .pls_abort("x must be returned by pls_mga().")
  .pls_require("ggplot2", "for MGA plots.")
  dat <- x$comparison
  gg <- ggplot2::ggplot(dat, ggplot2::aes(x=stats::reorder(path,difference),y=difference)) +
    ggplot2::geom_point(size=2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=conf_low,ymax=conf_high),width=.15) +
    ggplot2::geom_hline(yintercept=0,linetype=2) +
    ggplot2::coord_flip() +
    ggplot2::labs(x=NULL,y="Between-group path difference",
                  caption=paste0("Bootstrap comparison; R = ", x$bootstraps[[1]]$R, " per group")) +
    ggplot2::theme_minimal(base_size=11)
  if (!is.null(file)) ggplot2::ggsave(file,gg,width=width,height=height,dpi=dpi,units="in")
  gg
}

#' Publication table for advanced coefficient results
#'
#' @param x A coefficient data frame or `plssem_mga` result.
#' @param format Output format.
#' @param digits Display digits.
#' @return A data frame or formatted table.
#' @export
pls_table_advanced <- function(x, format=c("data.frame","markdown","gt","flextable"), digits=3L) {
  format <- match.arg(format)
  dat <- if (inherits(x,"plssem_mga")) x$comparison else x
  if (!is.data.frame(dat)) .pls_abort("x must be a coefficient data frame or a plssem_mga result.")
  if (format == "data.frame") return(dat)
  disp <- dat
  num <- vapply(disp,is.numeric,logical(1)); disp[num] <- lapply(disp[num],round,digits=digits)
  if (format == "markdown") { .pls_require("knitr"); return(knitr::kable(disp,format="pipe")) }
  if (format == "gt") { .pls_require("gt"); return(gt::gt(disp)) }
  .pls_require("flextable"); flextable::flextable(disp)
}

#' Print native MGA result
#' @param x A `plssem_mga` object.
#' @param ... Unused.
#' @export
print.plssem_mga <- function(x, ...) {
  cat("<plsSEMflow multigroup bootstrap comparison>\n")
  cat("Groups:", paste(x$groups, collapse=" vs "), "| R per group:", x$bootstraps[[1]]$R, "\n")
  print(x$comparison, row.names=FALSE)
  invisible(x)
}
