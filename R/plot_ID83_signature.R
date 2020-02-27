id_83_colors <- c(rgb(253/256,190/256,111/256), rgb(255/256,128/256,2/256), rgb(176/256,221/256,139/256), rgb(54/256,161/256,46/256),
                  rgb(253/256,202/256,181/256), rgb(252/256,138/256,106/256), rgb(241/256,68/256,50/256), rgb(188/256,25/256,26/256),
                  rgb(208/256,225/256,242/256), rgb(148/256,196/256,223/256), rgb(74/256,152/256,201/256), rgb(23/256,100/256,171/256),
                  rgb(226/256,226/256,239/256), rgb(182/256,182/256,216/256), rgb(134/256,131/256,189/256), rgb(98/256,64/256,155/256))

#' @export
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
  x <- compound_id_motifs(x) 
  p <- ggplot(x) + 
    geom_bar(aes(x = MotifLengthChar, y = Amount, fill = CompoundMotif), stat = "identity") + 
    facet_grid(cols = vars(CompoundMotif),
               rows = vars(Signature),
               scale = "free",
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
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- id_83_colors[k]
    k <- k+1
  }
  return (ggdraw(g))
}

