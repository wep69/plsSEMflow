if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop("Install 'roxygen2' before regenerating documentation and NAMESPACE.")
}
roxygen2::roxygenise(package.dir = ".", roclets = c("rd", "namespace", "collate"))
