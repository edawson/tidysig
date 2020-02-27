
#' @export
signature_activity_figure <- function(x,
                            legendPosition = "bottom",
                            countYlim = NULL,
                            propYlim = NULL){
  countPlot <- plot_signature_activities(x) + theme(legend.position = legendPosition)
  propPlot <- plot_signature_activities(x, countsAsProportions = TRUE)
  legend <- cowplot::get_legend(countPlot)
  countPlot <- countPlot + theme(legend.position = "none")
  propPlot <- propPlot + theme(legend.position = "none")
  figure <- NULL
  if (legendPosition == "bottom"){
    figure <- plot_grid(countPlot, propPlot, legend,
                      ncol=1, nrow=3,
                      rel_heights = c(1,1,0.25),
                      labels = c("A", "B"),
                      align = "hv",axis = "lt")
  }
  else if (legendPosition == "right"){
    figure <- plot_grid(
      plot_grid(countPlot, propPlot,
                nrow = 2, ncol=1,
                align = "hv", axis = "tblr",
                labels = c("A", "B")),
      legend, ncol=2, nrow = 1, rel_widths = c(1,0.25), align = "r"
    )

  }
  return(figure)
}

#' @export
plot_signature_activities <- function(x,
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
