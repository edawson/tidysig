tidysig
--------
Eric T. Dawson  
February 2020

## Introduction
tidysig is an R package for plotting mutational signatures / mutational contexts in
the tidyverse style. It produces ggplot2 plots of SBS96 and ID83 features which can then
be modified with standard ggplot2 layers.

## Compatibility
tidysig is currently compatible with SigProfilerExtractor (as of version 1.0.3),
with plans to support SignatureAnalyzer.

## Tidy signature represenation
Internally, tidysig converts SigProfiler outputs to a tidy data format with four variables for SBS96
signatures and six variables for ID83 signatures.

SBS96 Columns:
Signature:
Context:
Change:
Amount:

## Usage

Load a SigProfilerExtractor file as input and plot all signatures:  
```
library(tidysig)
library(readr)

sigprofiler_results <- read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Signatures_SBS96.txt")

df <- tidy_sigprof_SBS96_df(sigprofiler_results)

all_sig_plot <- plot_SBS96_signature(df)

## Counts can be normalized to proportions using the countsAsProportions argument
all_sig_plot_proportions <- plot_SBS96_signature(df, countsAsProportions=TRUE)

## You can apply the same y-axis limits to all subplots to make comparison between signatures easier.
all_sig_plot_proportions_norm <- plot_sbs96_signature(df, countsAsProportions=TRUE, ylimits=c(0,0.5)

## To plot a specific signature, you can filter using standard dplyr commands.
sig_96A_plot <- plot_SBS96_signature(df %>% dplyr::filter(Signature == "96A"))

## Plots can be saved using cowplot/ggplot2's save_plot function.
save_plot("all_sigs.pdf", all_sig_plot ,base_height = 10, base_width = 12)

## For single signatures, you can use the save_signature_plot function
save_signature_plot(sig_96A_plot, "sig_96A_plot.pdf")
```

## Citing the R package
You are free to use tidysig under the broadly-permissive MIT license. We ask 
that you cite it in the following manner:
```
tidysig: a tidyverse-style package for plotting mutational signatures. https://github.com/edawson/tidysig, Version <VERSION>. Eric T. Dawson. 2020.
```
