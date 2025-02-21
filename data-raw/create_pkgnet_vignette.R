

devtools::load_all()
remotes::install_github("noaa-edab/comlandr", ref = "dev")

result <- pkgnet::CreatePackageReport(pkg_name = "comlandr",
                                      pkg_path = ".")

pkgnet::CreatePackageVignette()
