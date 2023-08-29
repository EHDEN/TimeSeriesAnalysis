# Manually delete package from library. Avoids "Already in use" message when rebuilding
unloadNamespace("TimeSeriesAnalysis")
.rs.restartR()
folder <- system.file(package = "TimeSeriesAnalysis")
folder
unlink(folder, recursive = TRUE, force = TRUE)
file.exists(folder)

# Format and check code:
styler::style_pkg()
devtools::spell_check()

# Create manual and vignettes:
unlink("extras/TimeSeriesAnalysis.pdf")
shell("R CMD Rd2pdf ./ --output=extras/TimeSeriesAnalysis.pdf")

rmarkdown::render("vignettes/UsingTimeSeriesAnalysisPackage.Rmd",
                  output_file = "../inst/doc/UsingTimeSeriesAnalysisPackage.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/UsingTimeSeriesAnalysisPackage.tex")

pkgdown::build_site()
