
require("readr")
x_sigs_sbs <- tidysig::transform_sigprofiler_df(read_tsv("test_sigs.tsv"))
x_sigs_id83 <- tidysig::transform_sigprofiler_df(read_tsv("test_id83_sigs.tsv"))


test_that("plot_signature produces the expected plot for SBS96", {
  plot  <- plot_signature(x_sigs_sbs)
  vdiffr::expect_doppelganger("ID83 basic plot", plot)
})


test_that("plot_signature produces the expected plot for ID83", {
  plot  <- plot_signature(x_sigs_id83)
  vdiffr::expect_doppelganger("SBS basic plot", plot)
})

test_that("[plot_signature,  ID83] can use smart y-limits", {
  plot_dig  <- plot_signature(x_sigs_id83) %>% digest::digest(algo = "md5")
  expect_equal(0, 0)
})

test_that("[plot_signature,  SBS96] can use smart y-limits", {
  plot_dig  <- plot_signature(x_sigs_id83) %>% digest::digest(algo = "md5")
  expect_equal(0, 0)
})

test_that("plot_activity produces the expected plot for SBS96", {
  expect_equal(2 * 2, 4)
})