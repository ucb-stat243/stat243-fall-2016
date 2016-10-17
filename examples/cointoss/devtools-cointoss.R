
setwd('~/Dropbox/course_stat243/stat243-fall-2016/examples')

# =====================================================
# Using devtools
# =====================================================

library(devtools)

pkg <- 'cointoss'

devtools::document(pkg = pkg)
devtools::check_man(pkg = pkg)
devtools::test(pkg = pkg)
devtools::build_vignettes(pkg = pkg)
devtools::build(pkg = pkg)
devtools::install(pkg = pkg)



# =====================================================
# R CMD
# =====================================================

# tarball builder
R CMD build camdown

# tarball checker
R CMD check camdown_0.1.0.tar.gz

# tarball installer
R CMD INSTALL camdown_0.1.0.tar.gz

# create zip file for windows
R CMD INSTALL --build camdown
