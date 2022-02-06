test_that("Gives correct PAL status", {
  
  # Setting tested values
  pal_1_en <- get_pal_status(1.39, "en")
  pal_2_en <- get_pal_status(1.69, "en")
  pal_3_en <- get_pal_status(1.99, "en")
  pal_4_en <- get_pal_status(2.40, "en")
  pal_5_en <- get_pal_status(2.41, "en")
  pal_1_fr <- get_pal_status(1.39, "fr")
  pal_2_fr <- get_pal_status(1.69, "fr")
  pal_3_fr <- get_pal_status(1.99, "fr")
  pal_4_fr <- get_pal_status(2.40, "fr")
  pal_5_fr <- get_pal_status(2.41, "fr")
  
  # Tests
  expect_match(pal_1_en, "below the 'Sedentary or light activity lifestyle' category")
  expect_match(pal_2_en, "'Sedentary or light activity lifestyle'")
  expect_match(pal_3_en, "'Active or moderately active lifestyle'")
  expect_match(pal_4_en, "'Vigorous or vigorously active lifestyle'")
  expect_match(pal_5_en, "'Likely not sustainable'")
  
  expect_match(pal_1_fr, "en-dessous de la catégorie 'Style de vie sédentaire ou légèrement actif'")
  expect_match(pal_2_fr, "'Style de vie sédentaire ou légèrement actif'")
  expect_match(pal_3_fr, "'Style de vie actif ou modérément actif'")
  expect_match(pal_4_fr, "'Style de vie vigoureusement actif'")
  expect_match(pal_5_fr, "'Susceptible de ne pas pouvoir être maintenu'")
  
})
