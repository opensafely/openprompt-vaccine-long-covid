rmarkdown::render(input = here::here("analysis/010_quick_checks.Rmd"),
                  output_format = "html_document",
                  output_file = here::here("output/010_quick_checks.html")
)