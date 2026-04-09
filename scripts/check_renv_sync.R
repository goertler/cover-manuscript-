dependencies <- unique(renv::dependencies(path = ".", quiet = TRUE)$Package)
lockfile <- renv::lockfile_read("renv.lock")
lockfile_packages <- names(lockfile$Packages)
base_and_recommended <- rownames(installed.packages(priority = c("base", "recommended")))

missing_from_lockfile <- setdiff(
  dependencies,
  c(lockfile_packages, base_and_recommended)
)

if (length(missing_from_lockfile) > 0) {
  stop(
    paste(
      "renv.lock is missing packages referenced by the repository:",
      paste(sort(missing_from_lockfile), collapse = ", "),
      "\nRun renv::snapshot() after intentional dependency changes."
    ),
    call. = FALSE
  )
}

message("renv.lock covers all discovered project dependencies.")
