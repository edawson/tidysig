tidysig
--------
Eric T. Dawson  
February 2020


![R-CMD-check](https://github.com/edawson/tidysig/workflows/R-CMD-check/badge.svg)

## Introduction
tidysig is an R package for plotting mutational
signatures / mutational contexts in
the tidyverse style.
It produces ggplot2 plots of SBS96
and ID83 features which can then
be modified with standard ggplot2 layers. It attempts to make
plotting signatures simpler by requiring strict
formatting and abstracting as much as possible.

## Installation

tidysig can be installed with devtools:  
```
library(devtools)
devtools::install_github("edawson/tidysig")
```

To build from the GitHub source:
```
git clone --recursive https://github.com/edawson/tidysig
cd tidysig
Rscript scripts/devtools_install.R
```

If you want compatibility with SignatureAnalyzer, you'll need to install HDF5.
If you've installed SignatureAnalyzer locally, this is already on your computer.
Otherwise, you can install is for linux:
```
sudo apt-get install libhdf5-serial-dev
```

Or Mac OS X:
```
 brew install hdf5
``` 

The required R packages are listed in DESCRIPTION. Most are standard TidyVerse packages
(plus cowplot). In addition, [hdf5r](https://cran.r-project.org/web/packages/hdf5r/index.html)
is needed for SigantureAnalyzer files.

## Compatibility
tidysig is currently compatible with SigProfilerExtractor (as of version 1.0.3),
with plans to support SignatureAnalyzer.

**Nota bene:** There is a known incompatibility with some SigProfilerExtractor outputs,
which have a first column titled "MutationsType" instead of "MutationType." This can be remedied with the following dplyr::rename call after reading in the file:
```
sigprofiler_results <- read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Signatures_SBS96.txt")


sigprofiler_results <- sigprofiler_results %>%
  rename(MutationType=MutationsType)
```

## Tidy signature representation
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

df <- transform_sigprofiler_df(sigprofiler_results)

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

## Or, use the %in% operator for multiple signatures:
sig_96A_96B_plot <- plot_SBS96_signature(df %>% dplyr::filter(Signature %in% c("96A", "96B")))

## Plots can be saved using cowplot/ggplot2's save_plot function.
save_plot("all_sigs.pdf", all_sig_plot ,base_height = 6, base_asp=2)

## For single signatures, you can use the save_signature_plot function
save_signature_plot(sig_96A_plot, "sig_96A_plot.pdf")
```

You can layer on standard ggplot2 layers. Here's an example
where we remove sample names and change the theme to theme\_bw():
```R
activ <- transform_sigprofiler_df(
    read_tsv("sigprofiler_results/SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Activities_SBS96.txt")
)

plot_signature_activities(activ %>%
       group_by(Sample) %>%
       mutate(high = ifelse(sum(Amount) > 1000, "High", "Low")),
    countsAsProportions = F,
    showSampleNames = T,
    facetGroupVariable = "high") +
theme(axis.text.x = element_blank()) +
theme_bw()
```

## Prerequisites
You'll need to run SigProfiler to generate the inputs for tidysig.
SigProfiler can be installed with PIP. Note, I've frozen on specific
versions - check pip for the latest ones if you want to try them.

```bash
## if on a compute cluster, run:
## module load python

## Install SigProfiler to user directory using pip:
pip install --user SigProfilerExtractor==1.0.3

## Install SigProfilerMatrixGenerator:
pip install --user SigProfilerMatrixGenerator==1.1.0
```

The SigProfilerHelper utilities can be used to run SigProfiler from the command
line, rather than running it in a python REPL:
```bash
git clone https://github.com/edawson/SigProfilerHelper sigprofilerhelper
```

You need to first install a reference genome, such as GRCh37 (hg19):
```bash
python sigprofilerhelper/install_reference.py -g GRCh37
```

You can then generate a mutational counts file:
```bash
python sigprofilerhelper/generate_matrix -m <maf_file>
```

This will produce a directory (default name: sigprof\_input) which contains 
the inputs for SigProfilerExtractor. Another helper script can take this as
input and produce mutational signatures:
```bash
## Run SigProfilerExtractor for an SBS96 counts matrix,
## for 1 to 7 signatures,
## using 16 cores and 1000 iterations
python sigprofilerhelper/run_sigrofiler.py -t sigprof_input/output/SBS/PROJECT.SBS96.all -s 1 -e 7 -i 1000 -c 16
```

If you're on Biowulf (or another cluster using SLURM, you can write the following wrapper script:
```bash
#!/bin/bash
module load python

python sigprofilerhelper/run_sigrofiler.py -t sigprof_input/output/SBS/PROJECT.SBS96.all -s 1 -e 7 -i 1000 -c ${SLURM_CPUS_PER_TASK}
```

Save this file (as an example, to "run\_sigpro.sh")
and submit it to a queue like so:
```bash
sbatch --cpus-per-task=16 --mem=20g --error=sigpro.err.txt --ouput=sigpro.out.txt run_sigpro.sh
```

In a few hours (usually 3-5), you'll get output in a directory called sigpro\_results, which
will contain the inputs for tidysig.

## Citing the R package
You are free to use tidysig under the broadly-permissive MIT license. We ask 
that you cite it in the following manner:
```
tidysig: a tidyverse-style package for plotting mutational signatures. https://github.com/edawson/tidysig, Version <VERSION>. Eric T. Dawson. 2020.
```
