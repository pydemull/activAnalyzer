
test_that("METs are correctly computed with Santos-Lozano et al. equation [older adults]", {
  
  MET_santos_older_man_male <- 2.5878 + 0.00047 * 1500 - 0.6453 * 2 # computation for a male
  MET_santos_older_man_female <- 2.5878 + 0.00047 * 1500 - 0.6453 * 1 # computation for a female
  MET_santos_older_man_undefined <- 2.5878 + 0.00047 * 1500 - 0.6453 * 1 # computation for a undefined sex
  
  MET_santos_older_func_male <- compute_mets(data = data.frame(vm = 1500), 
                                        equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "male")
  MET_santos_older_func_female <- compute_mets(data = data.frame(vm = 1500), 
                                             equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "female")
  MET_santos_older_func_undefined <- compute_mets(data = data.frame(vm = 1500), 
                                               equation = "Santos-Lozano et al. (2013) [Older adults]", sex = "undefined")
    
  expect_equal(MET_santos_older_func_male, MET_santos_older_man_male)
  expect_equal(MET_santos_older_func_female, MET_santos_older_man_female)
  expect_equal(MET_santos_older_func_undefined, MET_santos_older_man_undefined)
  
})


test_that("METs are correctly computed with Santos-Lozano et al. equation [Adults]", {
  
  MET_santos_adult_man_male <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 2  # computation for a male
  MET_santos_adult_man_female <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 1  # computation for a female
  MET_santos_adult_man_undefined <- 2.8323 + 0.00054 * 1500 - 0.05912 * 67 + 1.4410 * 1  # computation for undefined sex
  
  MET_santos_adult_func_male <- compute_mets(data = data.frame(vm = 1500), 
                                               equation = "Santos-Lozano et al. (2013) [Adults]", 
                                               weight = 67,
                                               sex = "male")
  MET_santos_adult_func_female <- compute_mets(data = data.frame(vm = 1500), 
                                        equation = "Santos-Lozano et al. (2013) [Adults]", 
                                        weight = 67,
                                        sex = "female")
  MET_santos_adult_func_undefined<- compute_mets(data = data.frame(vm = 1500), 
                                               equation = "Santos-Lozano et al. (2013) [Adults]", 
                                               weight = 67,
                                               sex = "undefined")
  
  expect_equal(MET_santos_adult_func_male, MET_santos_adult_man_male)
  expect_equal(MET_santos_adult_func_female, MET_santos_adult_man_female)
  expect_equal(MET_santos_adult_func_undefined, MET_santos_adult_man_undefined)
  
})

test_that("METs are correctly computed with Sasaki et al. equation [Adults]", {
  
  MET_sasaki_adult_man <- 0.668876 + 0.000863 * 1500
  MET_sasaki_adult_func_male <- compute_mets(data = data.frame(vm = 1500), 
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "male")
  MET_sasaki_adult_func_female <- compute_mets(data = data.frame(vm = 1500), 
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "female")
  MET_sasaki_adult_func_undefined <- compute_mets(data = data.frame(vm = 1500), 
                                        equation = "Sasaki et al. (2011) [Adults]",
                                        sex = "undefined")
  
  expect_equal(MET_sasaki_adult_func_male, MET_sasaki_adult_man)
  expect_equal(MET_sasaki_adult_func_female, MET_sasaki_adult_man)
  expect_equal(MET_sasaki_adult_func_undefined, MET_sasaki_adult_man)
  
})

test_that("METs are correctly computed with Freedson et al. equation [Adults]", {
  
  MET_freedson_adult_man <-  1.439008 + 0.000795 * 1995 
  MET_freedson_adult_func_male <- compute_mets(data = data.frame(axis1 = 1995), 
                                        equation = "Freedson et al. (1998) [Adults]",
                                        sex = "male")
  MET_freedson_adult_func_female <- compute_mets(data = data.frame(axis1 = 1995), 
                                          equation = "Freedson et al. (1998) [Adults]",
                                          sex = "female")
  MET_freedson_adult_func_undefined <- compute_mets(data = data.frame(axis1 = 1995), 
                                          equation = "Freedson et al. (1998) [Adults]",
                                        sex = "undefined")
  
  expect_equal(MET_freedson_adult_func_male, MET_freedson_adult_man)
  expect_equal(MET_freedson_adult_func_female, MET_freedson_adult_man)
  expect_equal(MET_freedson_adult_func_undefined, MET_freedson_adult_man)
})





