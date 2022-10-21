
test_that("METs are correctly computed with Santos-Lozano et al. equation [older adults]", {
  
  MET_santos_older_male <- 2.5878 + 0.00047 * 1500 - 0.6453 * 2 # computation for a male
  MET_santos_older_female <- 2.5878 + 0.00047 * 1500 - 0.6453 * 1 # computation for a female
  MET_santos_older_undefined <- 2.5878 + 0.00047 * 1500 - 0.6453 * 1 # computation for an undefined sex
  MET_santos_older_intersex <- mean(c(MET_santos_older_male, MET_santos_older_female)) # computation for intersex
  

  MET_santos_older_func_male <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                        equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "male")
  MET_santos_older_func_female <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                             equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "female")
  MET_santos_older_func_undefined <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                               equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "undefined")
  MET_santos_older_func_intersex <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                                  equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "intersex")
  MET_santos_older_func_no_answer <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                                 equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "prefer not to say")
  
  
  expect_equal(MET_santos_older_func_male, MET_santos_older_male)
  expect_equal(MET_santos_older_func_female, MET_santos_older_female)
  expect_equal(MET_santos_older_func_undefined, MET_santos_older_undefined)
  expect_equal(MET_santos_older_func_intersex, MET_santos_older_intersex)
  expect_equal(MET_santos_older_func_no_answer, MET_santos_older_female)
  
})


test_that("METs are correctly computed with Santos-Lozano et al. equation [Adults]", {
  
  MET_santos_adult_male <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 2  # computation for a male
  MET_santos_adult_female <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 1  # computation for a female
  MET_santos_adult_undefined <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 1  # computation for undefined sex
  MET_santos_adult_intersex <- mean(c(MET_santos_adult_male, MET_santos_adult_female)) # computation for intersex
  
  MET_santos_adult_func_male <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                               equation = "Santos-Lozano et al. (2013) [Adults]", 
                                               weight = 67,
                                               sex = "male")
  MET_santos_adult_func_female <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                        equation = "Santos-Lozano et al. (2013) [Adults]", 
                                        weight = 67,
                                        sex = "female")
  MET_santos_adult_func_undefined <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                               equation = "Santos-Lozano et al. (2013) [Adults]", 
                                               weight = 67,
                                               sex = "undefined")
  MET_santos_adult_func_intersex <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                                 equation = "Santos-Lozano et al. (2013) [Adults]", 
                                                 weight = 67,
                                                 sex = "intersex")
  MET_santos_adult_func_no_answer <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                                 equation = "Santos-Lozano et al. (2013) [Adults]", 
                                                 weight = 67,
                                                 sex = "prefer not to say")
  
  expect_equal(MET_santos_adult_func_male, MET_santos_adult_male)
  expect_equal(MET_santos_adult_func_female, MET_santos_adult_female)
  expect_equal(MET_santos_adult_func_undefined, MET_santos_adult_undefined)
  expect_equal(MET_santos_adult_func_intersex, MET_santos_adult_intersex)
  expect_equal(MET_santos_adult_func_no_answer, MET_santos_adult_female)
  
})

test_that("METs are correctly computed with Sasaki et al. equation [Adults]", {
  
  MET_sasaki_adult <- 0.668876 + 0.000863 * 1500
  MET_sasaki_adult_func_male <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "male")
  MET_sasaki_adult_func_female <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "female")
  MET_sasaki_adult_func_undefined <- compute_mets(data =data.frame(axis1 = 1995, vm = 1500),  
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "undefined")
  MET_sasaki_adult_func_intersex <- compute_mets(data =data.frame(axis1 = 1995, vm = 1500),  
                                                  equation = "Sasaki et al. (2011) [Adults]",
                                                  sex = "intersex")
  MET_sasaki_adult_func_no_answer <- compute_mets(data =data.frame(axis1 = 1995, vm = 1500),  
                                                 equation = "Sasaki et al. (2011) [Adults]",
                                                 sex = "prefer not to say")
  
  expect_equal(MET_sasaki_adult_func_male, MET_sasaki_adult)
  expect_equal(MET_sasaki_adult_func_female, MET_sasaki_adult)
  expect_equal(MET_sasaki_adult_func_undefined, MET_sasaki_adult)
  expect_equal(MET_sasaki_adult_func_intersex, MET_sasaki_adult)
  expect_equal(MET_sasaki_adult_func_no_answer, MET_sasaki_adult)
  
})

test_that("METs are correctly computed with Freedson et al. equation [Adults]", {
  
  MET_freedson_adult <-  1.439008 + 0.000795 * 1995 
  MET_freedson_adult_func_male <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                        equation = "Freedson et al. (1998) [Adults]",
                                        sex = "male")
  MET_freedson_adult_func_female <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500),  
                                          equation = "Freedson et al. (1998) [Adults]",
                                          sex = "female")
  MET_freedson_adult_func_undefined <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                          equation = "Freedson et al. (1998) [Adults]",
                                        sex = "undefined")
  MET_freedson_adult_func_intersex <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                                    equation = "Freedson et al. (1998) [Adults]",
                                                    sex = "intersex")
  MET_freedson_adult_func_no_answer <- compute_mets(data = data.frame(axis1 = 1995, vm = 1500), 
                                                   equation = "Freedson et al. (1998) [Adults]",
                                                   sex = "prefer not to say")
  
  expect_equal(MET_freedson_adult_func_male, MET_freedson_adult)
  expect_equal(MET_freedson_adult_func_female, MET_freedson_adult)
  expect_equal(MET_freedson_adult_func_undefined, MET_freedson_adult)
  expect_equal(MET_freedson_adult_func_intersex, MET_freedson_adult)
  expect_equal(MET_freedson_adult_func_no_answer, MET_freedson_adult)
})





