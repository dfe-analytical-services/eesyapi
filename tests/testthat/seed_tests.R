seed_all_tests <- function() {
  seed_get_publication()
  seed_get_publication_datasets()
}

seed_get_publication <- function() {
  saveRDS(
    get_publication_catalogue(),
    file = "tests/testthat/testdata/example_publication_catalogue.rds"
  )
}

seed_get_publication_datasets <- function() {
  saveRDS(
    get_publication_datasets(example_id("publication")),
    file = "tests/testthat/testdata/example_publication_datasets.rds"
  )
}
