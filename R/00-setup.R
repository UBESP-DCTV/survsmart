install.packages("usethis")
usethis::create_package("survsmart")

usethis::use_tidy_github()

usethis::use_roxygen_md()  # documentation
usethis::use_test()  # TDD

usethis::use_readme_rmd()
usethis::use_github_action_check_standard()
usethis::use_lifecycle_badge("experimental")
usethis::use_mit_license("Giulia.Lorenzoni@unipd.it")
usethis::use_code_of_conduct("Corrado.Lanera@unipd.it")
devtools::build_readme()

usethis::use_package_doc()
usethis::use_pkgdown_github_pages()

usethis::use_spell_check()
