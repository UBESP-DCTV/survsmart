# survsmart (development version)

* added support from `{tibble}`.
* refactor `ldt` class and print method. (#6)
* `ldt` now require class code coherent with class names, i.e. arm 1 for stage 1 (X) is coded by 1 (and not 0). (fixes #5)
* `ldt` now correctly manage a single value of censor time if ties happened from time reference (i.e. event times) and reference (e.g. censor times). (fixes #3)

# survsmart 0.0.0.9000

## Utils
* Added `.add_function()` function in the `.Rprofile` as wrapper to correctly add a function to the package in interactive mode.
* Added `01-dev_cycle.R` as supporting script during package development.

## CI/CD
* Added automating GitHub-Actions for lint (`use_github_action("lint")`) and coverage (`use_github_action("test-coverage")`).
* Added automating GitHub-Actions for CMD-checks on win/linux and mac and on the latest release of R and on R-devel triggered by pushes on the main master branch and on linux and latest R release only from the develop branch (`use_github_action_check_standard()`, plus a customized copy for the develop branch).
* Added support for `{testthat}` (`use_testthat()`), and created `R/foo.R` and `tests/testthat/test-foo.R` file as base example and template; Added also `tests/testthat/setup.R` to include additional support from `{checkmate}`.

## Documentation
* Added automation for creation and update of the package website (`use_pkgdown_github_pages()`) and corresponding badge in the `README` (`badge_custom()`).
* Added MIT licence (`use_mit_license()`) and Code of Conduct (`use_code_of_conduct`).
* Added CRAN (`use_cran_badge()`) and lifecycle (`use_lifecycle_badge("experimental")`) badges to the `README` .
* Added `README.Rmd` as the homepage for the package (`use_readme_rmd()`).
* Created base package help documentation page (`use_package_doc()`).

## Base setup
* Added support for documentation spellcheck (`use_spell_check()`).
* Added support for `{roxygen2}` functionality for functions and data documentation (`use_roxygen_md()`).
* Added support for better GitHub contributions (`use_tidy_github()`). 
* Added a `NEWS.md` file to track changes to the package.
