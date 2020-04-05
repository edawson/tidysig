test_that("De novo sigprofiler sig files can be read [transform_sigprofiler_df]", {
  test_de_novo_sigs <- read_tsv("test_sigs.tsv")
  dig <- transform_sigprofiler_df(test_de_novo_sigs) %>% digest::digest(algo="md5")
  expect_equal(dig, "f20bcb09da161cad6cd2dbb99839c7aa")
})

test_that("Mutation catalogs can be read [transform_sigprofiler_df]", {
  test_de_novo_sigs <- read_csv("test_sample_counts.csv")
  dig <- transform_sigprofiler_df(test_de_novo_sigs) %>% digest::digest(algo="md5")
  expect_equal(dig, "4a280c5beaab345aab089b8d67cbc0c8")
})