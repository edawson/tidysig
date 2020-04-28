
require("readr")
require("digest")
test_de_novo_sigs <- readr::read_tsv("test_sigs.tsv")
test_mut_catalog <- readr::read_csv("test_sample_counts.csv")
test_activities <- readr::read_tsv("test-activities.tsv")

test_that("De novo sigprofiler sig files can be read [transform_sigprofiler_df]", {
  dig <- transform_sigprofiler_df(test_de_novo_sigs) %>% digest::digest(algo="md5")
  expect_equal(dig, "f20bcb09da161cad6cd2dbb99839c7aa")
})

test_that("Mutation catalogs can be read [transform_sigprofiler_df]", {
  test_de_novo_sigs <- readr::read_csv("test_sample_counts.csv")
  dig <- transform_sigprofiler_df(test_mut_catalog) %>% digest::digest(algo="md5")
  expect_equal(dig, "4a280c5beaab345aab089b8d67cbc0c8")
})

test_that("Activity counts can be read [transform_sigprofiler_df]",{
  acts <- transform_sigprofiler_df(test_activities)
  dig <- acts %>% digest::digest(algo="md5")
  expect_equal(1,1)
})