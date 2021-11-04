# Read huntington_disease_up as data.frame
huntington_disease_up <- as.data.frame.matrix(read.delim(
  system.file(
    package = "KeyPathwayMineR",
    "extdata/datasets",
    "huntington-gene-expression-UP.txt"
  ),
  header = TRUE
))

# The entries of the huntington_disease_up dataset are p-values. To create an indicator matrix the to_indicator_matrix()
# functio can be used
huntington_disease_up <- to_indicator_matrix(
  numerical_matrix = huntington_disease_up,
  operator = "<", threshold = 0.005
)

# Test 1 normal run -------------------------------------------------------------------
settings::reset(kpm_options)
kpm_options(
  execution = "Remote",
  strategy = "INES",
  remove_bens = TRUE,
  algorithm = "Greedy",
  l_min = 20,
  k_min = 5,
  graph_id = 10)

# Start run with huntington_disease_up dataset
example_1 <- kpm(indicator_matrices = huntington_disease_up)

test_that("Normal run", {
  expect_match(class(example_1), "Result")
})

# Test 2 ranged run -------------------------------------------------------------------
settings::reset(kpm_options)
kpm_options(
  execution = "Remote",
  strategy = "INES",
  remove_bens = TRUE,
  algorithm = "Greedy",
  use_range_k = T,
  l_min = 20,
  k_min = 5,
  k_step = 1,
  k_max = 6,
  graph_id = 10)

# Start run with huntington_disease_up dataset
example_2 <- kpm(indicator_matrices = huntington_disease_up)

test_that("Normal run", {
  expect_match(class(example_2), "Result")
})
# Test 3 multiple matrices ------------------------------------------------
settings::reset(kpm_options)
kpm_options(
  execution = "Remote",
  strategy = "INES",
  remove_bens = TRUE,
  algorithm = "Greedy",
  l_min = 20,
  k_min = 5,
  graph_id = 10)

# Start run with huntington_disease_up dataset
example_3 <- kpm(indicator_matrices = list(huntington_disease_up, huntington_disease_up))

test_that("Multiple matrices run", {
  expect_match(class(example_3), "Result")
})
