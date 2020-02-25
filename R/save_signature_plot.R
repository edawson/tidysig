#' Save a plot for a single mutational signature
#' 
#' This function saves a plot object x (usualy from plot_*_signature)
#' to a PDF named filename.
#' @param x A ggplot object
#' @param filename The name of the PDF to generate (must end in .pdf).
#' @param plot_height The height of the plot to generate. All plots have
#' a 1:3 aspect ratio.
#' @export
save_signature_plot <- function(x, filename, plot_height = 4){
  save_plot(filename, x, nrow = 1, base_height = plot_height, base_asp = 3)
}

