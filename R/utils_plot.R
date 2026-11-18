# ------------------------------------------------------------------------------
# utils_plot.R
#
# Purpose : Shared ggplot2 theme and colours for report figures.
#           Categorical slots (fixed order, never cycled) and the sequential
#           blue ramp come from a validated reference palette; the four
#           categorical slots pass the colour-vision-deficiency checks for
#           adjacent series. Two of them (aqua, yellow) are below 3:1 contrast
#           on the light surface, so every figure carries a legend or direct
#           labels and a secondary encoding (line type or shape), and its
#           numbers are also given in a table.
# ------------------------------------------------------------------------------

PAL <- list(
  series    = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"),   # blue, orange, aqua, yellow
  seq_blue  = c("#86b6ef", "#5598e7", "#2a78d6", "#184f95"),   # light to dark
  ink       = "#0b0b0b",
  ink_2     = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  surface   = "#fcfcfb"
)

theme_report <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      panel.background  = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = PAL$grid, linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.line.x       = ggplot2::element_line(colour = PAL$axis, linewidth = 0.3),
      axis.text         = ggplot2::element_text(colour = PAL$muted),
      axis.title        = ggplot2::element_text(colour = PAL$ink_2),
      plot.title        = ggplot2::element_text(colour = PAL$ink, face = "bold", size = base_size + 1),
      plot.subtitle     = ggplot2::element_text(colour = PAL$ink_2),
      plot.caption      = ggplot2::element_text(colour = PAL$muted, hjust = 0),
      strip.text        = ggplot2::element_text(colour = PAL$ink, face = "bold"),
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = ggplot2::element_text(colour = PAL$ink_2),
      legend.text       = ggplot2::element_text(colour = PAL$ink),
      plot.title.position = "plot"
    )
}

save_fig <- function(p, path, width = 6.5, height = 3.6) {
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 300, bg = PAL$surface)
  invisible(path)
}
