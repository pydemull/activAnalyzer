test_that("Gives correct guidelines status", {
  
  # Setting tested values
  mets_hr_below_en <- get_guidelines_status(1.06, "en")
  mets_hr_within_en <- get_guidelines_status(2.0, "en")
  mets_hr_above_en <- get_guidelines_status(2.15, "en")
  mets_hr_below_fr <- get_guidelines_status(1.06, "fr")
  mets_hr_within_fr <- get_guidelines_status(2.0, "fr")
  mets_hr_above_fr <- get_guidelines_status(2.15, "fr")
  
  # Tests
  expect_match(mets_hr_below_en, "below the 2020 WHO physical activity guidelines")
  expect_match(mets_hr_within_en, "within the 2020 WHO physical activity guidelines")
  expect_match(mets_hr_above_en, "above the 2020 WHO physical activity guidelines")
  expect_match(mets_hr_below_fr, "en-dessous des recommandations d'AP 2020 de l'OMS")
  expect_match(mets_hr_within_fr, "within the 2020 WHO physical activity guidelines")
  expect_match(mets_hr_above_fr, "au-dessus des recommandations d'AP 2020 de l'OMS")
  
})
