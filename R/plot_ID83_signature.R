
#' Plot an ID83 signature ro series of signatures
#' @param x A TidySig dataframe/tibble
#' @param label The right-side (i.e., facet) label.
#' Usually "Signature" or "Sample" or a sample ID.
#' @param title A title for the plot
#' @param xlabel An x-axis label
#' @param ylabel A y-axis label
#' @param usePercent Use percent scales (rather than counts)
#' @param ylimits Use custom ylimits (useful for normalizing the views of multiple signatures).
#' Takes a numeric vector length-two OR a string "smart" to indicate that consistent y-limits should
#' be automatically fit from the values in x.
#' @param countsAsProportions Convert the input data (in counts) to per-signature proportions
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @import cowplot
plot_ID83_signature <- function(x,
                               label = "Signature",
                               title = "",
                               xlabel = "Motif Length or Count",
                               ylabel = "Count",
                               usePercent=TRUE,
                               ylimits=NULL,
                               countsAsProportions=FALSE){
  
  if (countsAsProportions){
    x <- normalize_counts(x)
  }
  if (!is.null(ylimits) && get_maximum_amount(x) > max(ylimits)){
    warning("WARNING: ylimits less than maximum value [tidysig::plot_ID83_signature]. Plot will inaccurately depict signature(s).")
  }
  if (is.character(ylimits)){
    # & ylimits == "smart"
    stopifnot(ylimits == "smart")
    use_ints <- get_maximum_amount(x) > 1
    ylimits <- calculate_smart_ylimits(x, asInteger = use_ints)
  }
  x <- compound_id_motifs(x) 
  p <- ggplot(x) + 
    geom_bar(aes(x = MotifLengthChar, y = Amount, fill = CompoundMotif), stat = "identity") + 
    facet_grid(cols = vars(CompoundMotif),
               rows = vars(Signature),
               scales = "free",
               shrink = TRUE) +
    theme_minimal_hgrid(12) +
    coord_cartesian(expand = FALSE) + 
    scale_fill_manual(values = id_83_colors, aesthetics = "fill") + 
    labs(x = xlabel, y = ylabel) +
    theme(axis.text.x = element_text(size = 8, angle = 0, margin = margin(2,10,0,10, "pt"), hjust = 0.2)) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text.x = element_text(angle = 90, face = "bold", margin = margin(10,10,10,10, "pt"))) +
    theme(strip.text.y = element_text(face="bold")) +
    guides(fill = FALSE) +
    theme(panel.spacing.x = unit(2, "mm"),
          panel.spacing.y = unit(4, "mm"))
  
  if (usePercent & !is.null(ylimits)){
    p <- p +
      coord_cartesian(expand=FALSE,ylim = ylimits) +
    scale_y_continuous(labels = scales::percent) +
      labs(y="Proportion")
  }
  else if(usePercent){
    p <- p +
      scale_y_continuous(labels = scales::percent) +
      labs(y="Proportion")
  }
  else if(!is.null(ylimits)){
    p <- p +
      coord_cartesian(expand=FALSE,ylim = ylimits) +
      scale_y_continuous(breaks=pretty_breaks())
  }
  
  g <- ggplot_gtable(ggplot_build(p))
  stript <- which(grepl('strip-t', g$layout$name))
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- id_83_colors[k]
    k <- k+1
  }
  return (ggdraw(g))
}

