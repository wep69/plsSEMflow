# Run from the package root after installing development dependencies.
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")
roxygen2::roxygenise()
