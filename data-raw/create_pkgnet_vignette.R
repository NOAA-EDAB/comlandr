
# install.packages("pkgnet")
remotes::install_github("noaa-edab/comlandr", ref = "dev")

result <- pkgnet::CreatePackageReport(pkg_name = "comlandr",
                                      pkg_path = ".")

pkgnet::CreatePackageVignette()

# this doesn't work
knitr::knit(input = here::here("vignettes/pkgnet-report.Rmd"),
            output = here::here("data-raw/pkgnet-report.html"))
