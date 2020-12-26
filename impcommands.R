
##Originally from https://github.com/bendeivide/meupacoter

require(usethis)

## README
use_readme_rmd()

## NEWS
use_news_md()

## Creating Vignettes
use_vignette("Options_3D")

## package version
usethis::use_version()
numeric_version("1.9") == numeric_version("1.9.0-")

## handbook of good practice
goodpractice::gp()

## roxygen2::roxygenize()
devtools::document()

## To use badges
# usethis::use_lifecycle_badge()

# Creating site

## First time
usethis::use_pkgdown()
pkgdown::build_site()

## After
pkgdown::build_site()



# git init
# git add .
# git commit -m "first commit"
# git branch -M main
# git remote add origin https://github.com/Kinzel/mt5R.git
# git push -u origin main
