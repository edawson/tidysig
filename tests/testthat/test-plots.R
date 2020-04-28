
require("readr")
x_sigs_sbs <- tidysig::transform_sigprofiler_df(read_tsv("test_sigs.tsv"))
x_sigs_id83 <- tidysig::transform_sigprofiler_df(read_tsv("test_id83_sigs.tsv"))
x_activities <- tidysig::transform_sigprofiler_df(read_tsv("test-activities.tsv"))

test_that("plot_signature produces the expected plot for SBS96", {
  plot  <- plot_signature(x_sigs_sbs)
  vdiffr::expect_doppelganger("ID83 basic plot", plot)
})


test_that("plot_signature produces the expected plot for ID83", {
  plot  <- plot_signature(x_sigs_id83)
  vdiffr::expect_doppelganger("SBS basic plot", plot)
})

test_that("[plot_signature,  ID83] can use smart y-limits", {
  plot  <- plot_signature(x_sigs_id83, ylimits = "smart")
  vdiffr::expect_doppelganger("ID83 w/ smart ylimits", plot)
})

test_that("[plot_signature,  SBS96] can use smart y-limits", {
  plot  <- plot_signature(x_sigs_id83) %>% digest::digest(algo = "md5")
  vdiffr::expect_doppelganger("SBS96 w/ smart ylimits", plot)
})

test_that("plot_activity produces the expected plot for SBS96", {
  plot <- plot_activities(x_activities)
  vdiffr::expect_doppelganger("Signature Activity plot", plot)
})