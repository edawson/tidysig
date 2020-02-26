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

**Nota bene:** There is a known incompatibility with some SigProfilerExtractor outputs,
which have a first column titled "MutationsType" instead of "MutationType."

This can be remedied with the following dplyr::rename call after reading in the file:
```
sigprofiler_results <- read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Signatures_SBS96.txt")


sigprofiler_results <- sigprofiler_results %>%
  rename(MutationType=MutationsType)
```

## Tidy signature represenation
Internally, tidysig converts SigProfiler outputs to a tidy data format with four variables for SBS96
signatures and six variables for ID83 signatures.

*SBS96 Columns:*
| *Column Name* | Signature | Change | Context | Amount |
|---------------|-------------------------------------|---------------------------------------|:-----------------------------------------:|--------------------------------------------------------|
| *Description* | The name of the signature or sample | The genomic change (i.e., T>N or C>N) | The trinucleotide context of the variant. | The amount, either as a raw counts or as a proportion. |


*ID83 Features*  
| *Column Name* | Signature | Type | Length | Motif | Motif Length | Amount |
|---------------|-------------------------------------|------------------------------------|:---------------------------------------:|------------------------------------------------------------------------------------------------------------|--------------------------------------------------------|--------------------------------------------------------|
| *Description* | The name of the signature or sample | INS or DEL (insertion or deletion) | The length of the insertion or deletion | The motif surrounding the variant (i.e., within a C/T homopolymer, within a repeat, within microhomology)  | The length of the motif (in basepairs or repeat units) | The amount, either as a raw counts or as a proportion. |
## Usage

Load a SigProfilerExtractor file as input and plot all signatures:  
```R
library(tidysig)
library(readr)

sigprofiler_results <- read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Signatures_SBS96.txt")

df <- tidy_sigprof_SBS96_df(sigprofiler_results)

all_sig_plot <- plot_SBS96_signature(df)
```
<object data="https://github.com/edawson/tidysig/blob/master/images/sbs96_example_plot.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="https://github.com/edawson/tidysig/blob/master/images/sbs96_example_plot.pdf">
        <p>Please download the PDF to view SBS96 plot: <a href="https://github.com/edawson/tidysig/blob/master/images/sbs96_example_plot.pdf">Download PDF</a>.</p>
    </embed>
</object>


In addition, the resulting plots can be modified:

```R
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

You can layer on standard ggplot2 layers. Here's a silly example
where we remove sample names and change the theme to theme\_bw():
```R
activ <- tidy_sigprof_sbs96_activities(
    read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Activities_SBS96.txt")
)

plot_SBS96_activity(activ %>%
       group_by(Sample) %>%
       mutate(high = ifelse(sum(Amount) > 1000, "High", "Low")),
    countsAsProportions = F,
    showSampleNames = T,
    facetGroupVariable = "high") +
theme(axis.text.x = element_blank()) +
theme_bw()
```

## Citing the R package
You are free to use tidysig under the broadly-permissive MIT license. We ask 
that you cite it in the following manner:
```
tidysig: a tidyverse-style package for plotting mutational signatures. https://github.com/edawson/tidysig, Version <VERSION>. Eric T. Dawson. 2020.
```
