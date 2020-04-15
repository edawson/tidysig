#' @importFrom magrittr "%>%"
#' @import dplyr
compound_id_motifs <- function(x){
  x <- x %>%
    dplyr::mutate(CompoundMotif = paste(Type,Length,Motif, sep = ":")) %>%
    dplyr::mutate(CompoundMotif = factor(CompoundMotif, levels = c("Del:1:C",
                                                            "Del:1:T",
                                                            "Ins:1:C",
                                                            "Ins:1:T",
                                                            "Del:2:Repeat",
                                                            "Del:3:Repeat",
                                                            "Del:4:Repeat",
                                                            "Del:5:Repeat",
                                                            "Ins:2:Repeat",
                                                            "Ins:3:Repeat",
                                                            "Ins:4:Repeat",
                                                            "Ins:5:Repeat",
                                                            "Del:2:Microhomology",
                                                            "Del:3:Microhomology",
                                                            "Del:4:Microhomology",
                                                            "Del:5:Microhomology"),
                                  labels = c("1bp Del, C",
                                             "1bp Del, T",
                                             "1bp Ins, C",
                                             "1bp Ins, T",
                                             "2bp Del\nin Repeat",
                                             "3bp Del\nin Repeat",
                                             "4bp Del\nin Repeat",
                                             "5+bp Del\nin Repeat",
                                             "2bp Ins\nin Repeat",
                                             "3bp Ins\nin Repeat",
                                             "4bp Ins\nin Repeat",
                                             "5+bp Ins\nin Repeat",
                                             "2bp Del\nw/ Microhomology",
                                             "3bp Del\nw/ Microhomology",
                                             "4bp Del\nw/ Microhomology",
                                             "5+bp Del\nw/ Microhomology"))) %>%
    dplyr::mutate(MotifLengthChar = case_when(MotifLength == 1 & Motif %in% c("C", "T") ~ "1 ",
                                       MotifLength == 2 & Motif %in% c("C", "T") ~ "2 ",
                                       MotifLength == 3 & Motif %in% c("C", "T") ~ "3 ",
                                       MotifLength == 4 & Motif %in% c("C", "T") ~ "4 ",
                                       MotifLength >= 5 & Motif %in% c("C", "T") ~ "5+",
                                       MotifLength == 1 & Motif %in% c("Repeat") ~ "1 ",
                                       MotifLength == 2 & Motif %in% c("Repeat") ~ "2 ",
                                       MotifLength == 3 & Motif %in% c("Repeat") ~ "3 ",
                                       MotifLength == 4 & Motif %in% c("Repeat") ~ "4 ",
                                       MotifLength >= 5 & Motif %in% c("Repeat") ~ "5+",
                                       MotifLength == 1 & Motif %in% c("Microhomology") ~ "1 ",
                                       MotifLength == 2 & Motif %in% c("Microhomology") ~ "2 ",
                                       MotifLength == 3 & Motif %in% c("Microhomology") ~ "3 ",
                                       MotifLength == 4 & Motif %in% c("Microhomology") ~ "4 ",
                                       MotifLength >= 5 & Motif %in% c("Microhomology") ~ "5+",
                                       TRUE ~ "0 ")) %>%
    dplyr::mutate(MotifLengthChar = factor(MotifLengthChar, levels = c("0 ", "1 ", "2 ", "3 ", "4 ", "5+")))
  return (x)
}


compound_id_motifs_labeller <- function(x){
  return (x)
}


integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}
