test_that("example works", {
  animal_codebook <- data.frame(
    animal = c("elephant", "pig", "lizard"),
    family = c("mammal","mammal","reptile"))
  vec <- c("elephant", "pig", "pig", "lizard")
  recoded <- rename_based_on_codebook(input = vec,
                           codebook = animal_codebook,
                           rawvar = "animal",
                           codevar = "family")
  expect_identical(recoded,setNames(c("mammal","mammal","mammal","reptile"), vec))
})
