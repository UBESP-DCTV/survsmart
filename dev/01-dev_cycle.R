
# Use new packages ------------------------------------------------
used_pkgs <- c("checkmate", "purrr", "dplyr", "survival", "glue")
installed <- rownames(installed.packages())

install.packages(setdiff(used_pkgs, installed), quiet = TRUE)
purrr::walk(used_pkgs, usethis::use_package)
usethis::use_tidy_description()




# Add new functions -----------------------------------------------
.add_function("ldt")
.add_function("utils")
.add_function("print")
.add_function("extractors")




# Quality checks --------------------------------------------------
spelling::spell_check_package()
  ## spelling::update_wordlist()
lintr::lint_package()

## CTRL + SHIFT + D: update project documentation
## CTRL + SHIFT + T: run all project's tests
## CTRL + SHIFT + E: run all CRAN tests



# Update version --------------------------------------------------

usethis::use_version()
