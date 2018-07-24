
library(knitr)
install.packages("stringi")
options(install.packages.check.source = "no")
library(stringi)


rmarkdown::render("Documents/r_for_snacknation/churn_pres.Rmd")
