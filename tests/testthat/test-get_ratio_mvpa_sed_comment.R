test_that("Gives correct comment about MPVA/SED ratio", {
  
  # Setting tested values
  comment_ratio_below_en <- get_ratio_mvpa_sed_comment(0.05, "en")
  comment_ratio_above_en <- get_ratio_mvpa_sed_comment(0.03, "en")
  comment_ratio_below_fr <- get_ratio_mvpa_sed_comment(0.05, "fr")
  comment_ratio_above_fr <- get_ratio_mvpa_sed_comment(0.03, "fr")

  
  # Tests
  expect_match(comment_ratio_below_en, "the patient likely already has most of the health benefits from their physical behavior")
  expect_match(comment_ratio_above_en, "the patient could get further health benefits by replacing more sedentary time by physical activity time")
  expect_match(comment_ratio_below_fr, "le patient obtient déjà probablement la plupart des bénéfices de santé liés à son comportement physique")
  expect_match(comment_ratio_above_fr, "le patient pourrait probablement obtenir davantage de bénéfices de santé en remplaçant du temps sédentaire par du temps d'activité physique")

  
})
