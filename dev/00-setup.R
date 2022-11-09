# install.packages("usethis")
usethis::create_package("survsmart")
usethis::git_vaccinate()
usethis::use_package("usethis", type = "suggests")
usethis::use_package("devtools", type = "suggests")


# Documentation ---------------------------------------------------
usethis::use_news_md()
usethis::use_tidy_github()
usethis::use_roxygen_md()  # documentation
usethis::use_spell_check()
usethis::use_package_doc()


# README, licence, CoC, badges website ----------------------------
usethis::use_readme_rmd()
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

usethis::use_mit_license("Giulia Lorenzoni & Corrado Lanera")
usethis::use_code_of_conduct("Corrado.Lanera@unipd.it")

devtools::build_readme()

usethis::use_pkgdown_github_pages()
# remotes::install_github("GuangchuangYu/badger")
badger::badge_custom("WEBsite", "click-me", "orange", "http://UBESP-DCTV/survsmart/")


# TDD and CI/CD ---------------------------------------------------
usethis::use_testthat()  # TDD
usethis::use_package("checkmate", type = "suggests")

usethis::use_test("foo") # `test_that("foo works", expect_null(foo()))`
devtools::test()         # see it fails!!
usethis::use_r("foo")    # define `foo <- function() NULL`
devtools::test()         # see it passes!!

usethis::use_github_action_check_standard()
usethis::use_github_action("lint")
usethis::use_github_actions_badge("lint")
usethis::use_package("lintr", type = "suggests")

usethis::use_github_action("test-coverage", save_as = "covr.yaml")
usethis::use_github_actions_badge("covr")
usethis::use_package("covr", type = "suggests")


# NSE
usethis::use_tidy_eval()

# Final checks ----------------------------------------------------
usethis::use_tidy_description()
devtools::document(
  roclets = c('rd', 'collate', 'namespace', 'vignette')
)
devtools::check_man()
spelling::spell_check_package()
spelling::update_wordlist()
lintr::lint_package()
devtools::check()
