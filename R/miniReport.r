htmlTable <- function(
  df,
  caption="Basic Infos"
){
  require(kableExtra)
  kable_styling(knitr::kable(df,
                             format = "html",
                             col.names = NULL,
                             align = "c",
                             caption = caption,
                             table.attr = "class=\"table table-bordered\""),
                bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}
