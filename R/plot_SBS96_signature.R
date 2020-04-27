
plot_SBS96_contexts <- function(x,
                                usePercent=FALSE,
                                countsAsProportions=FALSE,
                                xlabel="Base Context",
                                ylabel="Count",
                                title=NULL,
                                labelVar="Sample"){
  return(
    plot_SBS96_signature()
  )
}

#' Plot an SBS96 signature or series of signatures
#' @param x A TidySig dataframe/tibble
#' @param label The right-side (i.e., facet) label.
#' Usually "Signature" or "Sample" or a sample ID.
#' @param title A title for the plot
#' @param xlabel An x-axis label
#' @param ylabel A y-axis label
#' @param facetCondition a condition to generate facet columns.
#' @param usePercent Use percent scales (rather than counts)
#' @param ylimits Use custom ylimits (useful for normalizing the views of multiple signatures).
#' Takes a numeric vector length-two OR a string "smart" to indicate that consistent y-limits should
#' be automatically fit from the values in x.
#' @param countsAsProportions Convert the input data (in counts) to per-signature proportions
#' @return a ggplot2 object
#' @export
#' @import ggplot2 cowplot scales
plot_SBS96_signature <- function(x,
                                 label = "Signature",
                                 title = NULL,
                                 xlabel = "Base Context",
                                 ylabel = "Count",
                                 ylimits=NULL,
                                 usePercent=FALSE,
                                 countsAsProportions=FALSE,
                                 facetCondition=NULL){
  if (countsAsProportions){
    x <- normalize_counts(x)
  }
  if (!is.null(ylimits) && get_maximum_amount(x) > max(ylimits)){
    warning("WARNING: ylimits less than maximum value [tidysig::plot_SBS96_signature]. Plot will inaccurately depict signature(s).")
  }
  if (is.character(ylimits)){
    # & ylimits == "smart"
    stopifnot(ylimits == "smart")
    use_ints <- get_maximum_amount(x) > 1
    ylimits <- calculate_smart_ylimits(x, asInteger = use_ints)
  }
  p <- ggplot2::ggplot(x) +
    geom_bar(aes(x = Context, y = Amount, fill = Change), stat = "identity") + 
    facet_grid(cols = vars(Change), rows = vars(Signature), scales = "free", shrink = TRUE) +
    theme_minimal_hgrid(12) +
    theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.3, margin = margin(0.1, 1, 0, 1, unit="pt"))) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text.x = element_text(face = "bold", color = "#FEFFFF")) +
    theme(strip.text.y = element_text(face="bold")) +
    theme(panel.spacing.y = unit(6, "mm")) +
    coord_cartesian(expand = FALSE) + 
    scale_fill_manual(values = sbs_96_changes_colors, aesthetics = "fill") +
    labs(x = xlabel, y = ylabel) +
    guides(fill = FALSE)
  
  if (!is.null(title)){
    p <- p + ggtitle(title)
  }
  
  if (usePercent & !is.null(ylimits)){
    p <- p +
      coord_cartesian(expand=FALSE,ylim = ylimits)
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
      coord_cartesian(expand=FALSE,ylim = ylimits)
      scale_y_continuous(breaks=pretty_breaks())
  }
  
  
  g <- ggplot_gtable(ggplot_build(p))
  stript <- which(grepl('strip-t', g$layout$name))
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- sbs_96_changes_colors[k]
    k <- k+1
  }
  return (ggdraw(g))
}



