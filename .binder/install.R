print("install.R: Start...")

# Define required packages.
# TODO: Fix versions, ask Anthony.
required_packages_versions <- list(
  "sf" = NULL,
  "curl" = NULL,
  "rgbif" = NULL,
  "rvertnet" = NULL,
  "rinat" = NULL,
  "rfishbase" = NULL
)

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.rstudio.com/")
}

# Define function to install from CRAN.
install_if_missing <- function(pkg, version) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_version(pkg, version = version, repos = "https://cran.rstudio.com/")
  }
}

# Logging:
num = length(required_packages_versions)
list_dep = paste(required_packages_versions, collapse="+")
print(paste0("install.R: Install ", num, " dependencies: ", list_dep, "..."))

# Run the installs:
invisible(lapply(names(required_packages_versions), function(pkg) {
  install_if_missing(pkg, required_packages_versions[[pkg]])
}))


# Install specleanr itself from GitHub, and try loading it.
# TODO: Must fix version!
print("install.R: Install specleanr...")
remotes::install_github("AnthonyBasooma/specleanr")
print("install.R: Install specleanr... DONE.")
print("install.R: Try loading specleanr...")
library(specleanr)

# Session info:
print("install.R: sessionInfo...")
sessionInfo()

