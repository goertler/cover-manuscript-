repo_root <- normalizePath(
  file.path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "io.R"))
source(file.path(repo_root, "R", "species.R"))
source(file.path(repo_root, "R", "lifestage.R"))
source(file.path(repo_root, "R", "location.R"))
source(file.path(repo_root, "R", "plotting.R"))
