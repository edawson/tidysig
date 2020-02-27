
#' @export
plot_SBS96_signature <- function(x,
                                 label = "Signature",
                                 title = NULL,
                                 xlabel = "Base Context",
                                 ylabel = "Count",
                                 ylimits=NULL,
                                 usePercent=FALSE,
                                 countsAsProportions=FALSE){
  if (countsAsProportions){
    x <- normalize_counts(x)
  }
  p <- ggplot(x) +
    geom_bar(aes(x = Context, y = Amount, fill = Change), stat = "identity") + 
    facet_grid(cols = vars(Change), rows = vars(Signature), scales = "free", shrink = TRUE) +
    theme_minimal_hgrid(12) +
    theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.3, margin = margin(0.1, 1, 0, 1, unit="pt"))) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text.x = element_text(face = "bold", color = "#FEFFFF")) +
    theme(strip.text.y = element_text(face="bold")) +
    theme(panel.spacing.y = unit(4, "mm")) +
    coord_cartesian(expand = FALSE) + 
    scale_fill_manual(values = sbs_96_changes_colors, aesthetics = "fill") +
    labs(x = xlabel, y = ylabel) +
    guides(fill = FALSE)
  
  if (!is.null(title)){
    p <- p + ggtitle(title)
  }
  
  if (usePercent & !is.null(ylimits)){
    p <- p + scale_y_continuous(labels = scales::percent, limits = ylimits) + labs(y="Proportion")
  }
  else if(usePercent){
    p <- p + scale_y_continuous(labels = scales::percent) + labs(y="Proportion")
  }
  else if(!is.null(ylimits)){
    p <- p + scale_y_continuous(breaks= pretty_breaks(),limits=ylimits)
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

