# document
devtools::document(here::here())
# check package
devtools::check(here::here())
# fix github actions
usethis::use_github_action()
usethis::use_github_action("lint")
# check win-builder
devtools::check_win_devel(here::here())
devtools::check_win_release(here::here())
devtools::check_win_oldrelease(here::here())
# update pkg site
pkgdown::build_site(here::here())
# revdep
revdepcheck::cran_revdeps("furniture")
revdepcheck::revdep_check(here::here())
# release to CRAN
devtools::release(here::here())
