sbs_96_contexts <- c("ACA","ACC","ACG","ACT","ACA","ACC",
                     "ACG","ACT", "ACA","ACC","ACG","ACT",
                     "ATA","ATC","ATG","ATT","ATA","ATC",
                     "ATG","ATT","ATA","ATC","ATG","ATT",
                     "CCA","CCC","CCG","CCT","CCA","CCC",
                     "CCG","CCT","CCA","CCC","CCG","CCT",
                     "CTA","CTC","CTG","CTT","CTA","CTC",
                     "CTG","CTT","CTA","CTC","CTG","CTT",
                     "GCA","GCC","GCG","GCT","GCA","GCC",
                     "GCG","GCT","GCA","GCC","GCG","GCT",
                     "GTA","GTC","GTG","GTT","GTA","GTC",
                     "GTG","GTT","GTA","GTC","GTG","GTT",
                     "TCA","TCC","TCG","TCT","TCA","TCC",
                     "TCG","TCT","TCA","TCC","TCG","TCT",
                     "TTA","TTC","TTG","TTT","TTA","TTC",
                     "TTG","TTT","TTA","TTC","TTG","TTT")
sbs_96_changes <- c("C>A","C>G","C>T",
                    "T>A","T>C","T>G")

sbs_96_changes_colors <- c(rgb(3/256,189/256,239/256),
                           rgb(1/256,1/256,1/256),
                           rgb(228/256,41/256,38/256),
                           rgb(203/256,202/256,202/256),
                           rgb(162/256,207/256,99/256),
                           rgb(236/256,199/256,197/256))


#' @export
plot_SBS96_activity <- function(x,
                                title = NULL,
                                xlabel = "Samples",
                                ylabel = "Mutation Counts",
                                ylimits=NULL,
                                usePercent = FALSE,
                                countsAsProportions=FALSE,
                                showSampleNames = FALSE,
                                orderByMutationCount = TRUE,
                                facetGroupVariable = NULL){

  ## Calculate the total number of mutations
  ## so that we can order plots on that.
  x <- x %>%
    ungroup() %>%
    group_by(Sample) %>%
    mutate(totalForOrder = sum(Amount))
    if (countsAsProportions){
        x <- normalize_sample_counts(x)
    }
  
    p <- ggplot(x)
    if (orderByMutationCount){
        p <- p + geom_bar(aes(x = reorder(Sample, -totalForOrder), y = Amount, fill = Signature), stat = "identity", width = 0.99)
    }
    else{
        p <- p + geom_bar(aes(x = Sample, y = Amount, fill = Signature), stat = "identity", width = 0.99)
    }
    p <- p + theme_minimal_hgrid(14) +
    labs(x = xlabel, y = ylabel) +
    coord_cartesian(expand=F)
  
  if (!is.null(ylimits)){
    p <- p + ylimits
  }
  if (countsAsProportions | usePercent){
    p <- p + 
      scale_y_continuous(labels = scales::percent, breaks=pretty_breaks()) +
      labs(y="Proportion")
  }
  
  if (!showSampleNames){
    p <- p + theme(axis.text.x = element_blank())
  }
  else{
    p <- p +
      theme(axis.text.x = element_text(angle = 90,
                                       size = 6,
                                       vjust = 0.3,
                                       margin = margin(0.1, 1, 0, 1, unit="pt")))
  }
  
  if (!is.null(facetGroupVariable)){
    p <- p + facet_grid(rows = facetGroupVariable) + 
      theme(strip.background = element_rect(fill = "lightgrey")) +
      theme(strip.text.x = element_text(face = "bold", color = "#FEFFFF")) +
      theme(strip.text.y = element_text(face="bold")) +
      theme(panel.spacing.y = unit(4, "mm")) 
  }
  if (!is.null(title)){
    p <- p + ggtitle(title)
  }
  
  return (p)
}

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

