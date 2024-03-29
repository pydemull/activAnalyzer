
test_that("BMR is correctly computed for males", {
  
  bmr_man <- 14.2 * 67 + 593 # for men older than 30 yr
  bmr_func <- compute_bmr(age = 32, sex = "male", weight = 67)
  
  expect_equal(bmr_func, bmr_man)
})

test_that("BMR is correctly computed for females", {
  
  bmr_man <- 10.2 * 58 +  572 # for female older than 60 yr
  bmr_func <- compute_bmr(age = 65, sex = "female", weight = 58)  
  
  expect_equal(bmr_func, bmr_man)
})

test_that("BMR is correctly computed for undefined sex", {
  
  bmr_man <- 10.2 * 58 +  572 # person with undefined sex older than 60 yr
  bmr_func <- compute_bmr(age = 65, sex = "undefined", weight = 58)  
  
  expect_equal(bmr_func, bmr_man)
})


test_that("BMR is correctly computed for intersex", {
  
  bmr_man <- ((13.0 * 58 + 567) + (10.2 * 58 +  572)) / 2  # intersex person older than 60 yr
  bmr_func <- compute_bmr(age = 65, sex = "intersex", weight = 58)  
  
  expect_equal(bmr_func, bmr_man)
})

test_that("BMR is correctly computed for people reporting 'prefer not to say'", {
  
  bmr_man <- 10.2 * 58 +  572 # for female older than 60 yr
  bmr_func <- compute_bmr(age = 65, sex = "prefer not to say", weight = 58)  
  
  expect_equal(bmr_func, bmr_man)
})

