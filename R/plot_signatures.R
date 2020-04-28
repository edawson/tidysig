
#' A helper function to determine the context type of a dataframe
#' @param x A TidySig dataframe
#' @return A string indicating the mutational signature context of x
determine_context <- function(x){
  if ("Motif" %in% names(x) & 
      "MotifLength" %in% names(x)){
    return ("ID83")
  } else if ("Context" %in% names(x) &
             "Change" %in% names(x)){
    return ("SBS96")
  }
  
  stop("Error: incorrect context. Please use a TidySig format (https://github.com/edawson/tidysig)")
}

#' A helper function to plot mutational contexts,
#' which are generally counts (in contrast to signatures,
#' which are generally proportions).
#' @param x A TidySig dataframe/tibble
#' @param label The right-side (i.e., facet) label.
#' Usually "Signature" or "Sample" or a sample ID.
#' @param title A title for the plot
#' @param xlabel An x-axis label
#' @param ylabel A y-axis label
#' @param usePercent Use percent scales (rather than counts)
#' @param ylimits Use custom ylimits (useful for normalizing the views of multiple signatures).
#' Takes a numeric vector length-two.
#' @param countsAsProportions Use proportions, rather than raw counts.
#' @param separatePlots Produce separate plots, one per Signature
#' @param contextType An argument for determining the context, either ID83 or SBS96
#' @export
plot_context <- function(x,
			 label = "Signature",
			 title = "",
			 xlabel = "Motif Length or Count",
			 ylabel = "Count",
			 usePercent=TRUE,
			 ylimits=NULL,
			 countsAsProportions=FALSE,
			 separatePlots = FALSE,
			 contextType=contextType){
	return (plot_signature(x, label=label,
	                       title=title, xlabel=xlabel,
	                       ylabel=ylabel,
	                       usePercent=usePercent,
	                       ylimits=ylimits,
	                       countsAsProportions=countsAsProportions,
	                       separatePlots=separatePlots,
	                       contextType=contextType))
}


#' A helper function for plotting ID83 / SBS96 plots.
#' @param x A TidySig dataframe/tibble
#' @param label The right-side (i.e., facet) label.
#' Usually "Signature" or "Sample" or a sample ID.
#' @param title A title for the plot
#' @param xlabel An x-axis label
#' @param ylabel A y-axis label
#' @param usePercent Use percent scales (rather than counts)
#' @param ylimits Use custom ylimits (useful for normalizing the views of multiple signatures).
#' Takes a numeric vector length-two.
#' @param contextType An argument for determining the context, either ID83 or SBS96
#' @return a ggplot2 object
generate_plot <- function(x,
                          label = "Signature",
                          title = "",
                          xlabel = NULL,
                          ylabel = "Count",
                          usePercent=TRUE,
                          ylimits=NULL,
                          contextType = "SBS96"){
  
  if (contextType == "auto"){
    x <- normalize_counts(x)
  }
  if (contextType == "SBS96"){
    p <- ggplot2::ggplot(x) +
      geom_bar(aes(x = Context, y = Amount, fill = Change), stat = "identity") + 
      facet_grid(cols = vars(Change), rows = vars(Signature), scales = "free", shrink = TRUE)
  } else if (contextType == "ID83"){
    x <- compound_id_motifs(x) 
    p <- ggplot(x) + 
      geom_bar(aes(x = MotifLengthChar, y = Amount, fill = CompoundMotif), stat = "identity") + 
      facet_grid(cols = vars(CompoundMotif),
                 rows = vars(Signature),
                 scales = "free",
                 shrink = TRUE)
  }
  
  p <- p + theme_minimal_hgrid(12) +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text.x = element_text(face = "bold", color = "#FEFFFF")) +
    theme(strip.text.y = element_text(face="bold")) +
    theme(panel.spacing.y = unit(6, "mm")) +
    coord_cartesian(expand = FALSE)+
    labs(x = xlabel, y = ylabel) +
    guides(fill = FALSE)
  
  if (contextType == "SBS96"){
    p <- p + scale_fill_manual(values = sbs_96_changes_colors, aesthetics = "fill") + 
      theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.3, margin = margin(0.1, 1, 0, 1, unit="pt")))
      
  } else if (contextType == "ID83"){
    p <- p + scale_fill_manual(values = id_83_colors, aesthetics = "fill") + 
      theme(axis.text.x = element_text(size = 8, angle = 0, margin = margin(2,10,0,10, "pt"), hjust = 0.2))+ 
      theme(strip.text.x = element_text(angle = 90, face = "bold", color = "#FEFFFF", margin = margin(10,10,10,10, "pt")))
  }
  
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
  
  if (contextType == "SBS96"){
    g <- ggplot_gtable(ggplot_build(p))
    stript <- which(grepl('strip-t', g$layout$name))
    k <- 1
    for (i in stript) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- sbs_96_changes_colors[k]
      k <- k+1
    }
  } else if (contextType == "ID83"){
    g <- ggplot_gtable(ggplot_build(p))
    stript <- which(grepl('strip-t', g$layout$name))
    k <- 1
    for (i in stript) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- id_83_colors[k]
      k <- k+1
    }
  }
  
  return( ggdraw(g) )
}



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
#' @param separatePlots If true, return a vector of plots, one for each signature, rather than one plot
#' with a facet for each signature.
#' @param contextType An argument for determining the context, generally "SBS96" or "ID83". The default, "auto",
#' will attempt to determine the context automatically based on the columns of the x.
#' @return a ggplot2 object, unless separatePlot = True, in which case a list of ggplot objects is returned.
#' @export
#' @import ggplot2
#' @import cowplot
plot_signature <- function(x,
			   label = "Signature",
			   title = "",
			   xlabel = NULL,
			   ylabel = "Count",
			   usePercent=TRUE,
			   ylimits=NULL,
			   countsAsProportions=FALSE,
			   separatePlots = FALSE,
			   contextType = "auto"){

	if (countsAsProportions){
		x <- normalize_counts(x)
		ylabel <- "Proportion"
	}
	if (!is.null(ylimits) && get_maximum_amount(x) > max(ylimits)){
		warning("WARNING: ylimits less than maximum value [tidysig::plot_signature]. Plot will inaccurately depict signature(s).")
	}
	if (is.character(ylimits)){
		# & ylimits == "smart"
		stopifnot(ylimits == "smart")
		use_ints <- get_maximum_amount(x) > 1
		ylimits <- calculate_smart_ylimits(x, asInteger = use_ints)
	}
  
  stopifnot(contextType %in% c("auto", "SBS96", "ID83"))
  if (contextType == "auto"){
    contextType = determine_context(x)
    if (is.null(xlabel) & contextType == "SBS96"){
      xlabel <- "Base Context"
    } else if (is.null(xlabel) & contextType == "ID83"){
      xlabel <- "Motif Length or Count"
    }
  }
  
  g <- NULL
  if (separatePlots){
    sigs <- x %>% distinct(Signature)
    stop("Not yet implement [separatePlot, plot_signature]")
  }
  else{
    g <- generate_plot(x,
                  label = label,
                  title = title,
                  xlabel = xlabel,
                  ylabel = ylabel,
                  usePercent=usePercent,
                  ylimits=ylimits,
                  contextType = contextType)
  }
		return (g)
}

