
##
# make_sbs_plots.R
# Eric T. Dawson, April 2020
# Given a SigProfiler results directory,
# read in the de novo and decomposition results and
# generate plots/summary counts. These are saved to
# an outdir directory

library(GetoptLong)
outdir = "tidysigplots"
GetoptLong(
    "input=s", "A SigProfiler SBS/ID results directory.",
    "outdir=s", "A name for a directory within which to save plots (default: tidysigplots/)."
)
library(tidyverse)
library(cowplot)
library(tidysig)

dir.create(file.path(outdir), showWarnings = FALSE)

sig_dn_fi <- paste(input, "SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Signatures_SBS96.txt", sep ="/")
act_dn_fi <- paste(input, "SBS96/Suggested_Solution/De_Novo_Solution/De_Novo_Solution_Activities_SBS96.txt", sep="/")
sig_decomp_fi <- paste(input, "SBS96/Suggested_Solution/Decomposed_Solution/Decomposed_Solution_Signatures_SBS96.txt", sep="/")
act_decomp_fi <- paste(input, "SBS96/Suggested_Solution/Decomposed_Solution/Decomposed_Solution_Activities_SBS96.txt", sep="/")
message("Reading sigs from: ", sig_dn_fi)

sig_dn_fi <- read_tsv(sig_dn_fi)
act_dn_fi <- read_tsv(act_dn_fi)
sig_decomp_fi <- read_tsv(sig_decomp_fi)
act_decomp_fi <- read_tsv(act_decomp_fi)

## De novo signature plots
dn_sigs <- tidysig::transform_sigprofiler_df(sig_dn_fi)
dn_sigplot <- tidysig::plot_SBS96_signature(dn_sigs)
save_plot(paste(outdir, "denovo_SBS96_signature_plot.pdf", sep = "/"), dn_sigplot, base_asp = 2.6, base_height = 7)

## De novo activity plots
dn_acts <- tidysig::tidy_sigprof_activities(act_dn_fi)
dn_actplot <- tidysig::signature_activity_figure(dn_acts)
save_plot(paste(outdir, "denovo_SBS96_activity_figure.pdf", sep = "/"),dn_actplot, base_asp = 1.6, base_height = 6)

## De novo counts summary TSV
dn_summary <- tidysig::per_sample_summary(dn_acts)
write_tsv(dn_summary, paste(outdir, "denovo_SBS96_activity_summary.tsv", sep="/"))

## Decomposition signature plots
decomp_sigs <- tidysig::transform_sigprofiler_df(sig_decomp_fi)
decomp_sigplot <- tidysig::plot_SBS96_signature(decomp_sigs)
save_plot(paste(outdir, "decomposition_SBS96_signature_plot.pdf", sep="/"),decomp_sigplot, base_asp=3, base_height=7)

## Decomposition activity plots
decomp_acts <- tidysig::tidy_sigprof_activities(act_decomp_fi)
decomp_actplot <- tidysig::signature_activity_figure(decomp_acts)
save_plot(paste(outdir, "decomposition_SBS96_activity_plot.pdf", sep="/"), decomp_actplot, base_asp=1.6, base_height=6)

## Decomposition counts summary TSV
decomp_summary <- tidysig::per_sample_summary(decomp_acts)
write_tsv(decomp_summary, paste(outdir, "decomposition_SBS96_activity_summary.tsv", sep="/"))
