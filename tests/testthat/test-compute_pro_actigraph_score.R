test_that("PROactive score is correct", {
  
  # C-PPAC / Steps score (median)
  expect_equal(compute_pro_actigraph_score(1000, "C-PPAC",  "steps", fun = "median") , 0)
  expect_equal(compute_pro_actigraph_score(1001, "C-PPAC",  "steps", fun = "median"), 1)
  expect_equal(compute_pro_actigraph_score(2000, "C-PPAC",  "steps", fun = "median"), 1)
  expect_equal(compute_pro_actigraph_score(2001, "C-PPAC",  "steps", fun = "median"), 2)
  expect_equal(compute_pro_actigraph_score(4000, "C-PPAC",  "steps", fun = "median"), 2)
  expect_equal(compute_pro_actigraph_score(4001, "C-PPAC",  "steps", fun = "median"), 3)
  expect_equal(compute_pro_actigraph_score(6000, "C-PPAC",  "steps", fun = "median"), 3)
  expect_equal(compute_pro_actigraph_score(6001, "C-PPAC",  "steps", fun = "median"), 4)
  expect_equal(compute_pro_actigraph_score(6799, "C-PPAC",  "steps", fun = "median"), 4)
  
  
  # C-PPAC / VMU score (median)
  expect_equal(compute_pro_actigraph_score(100, "C-PPAC",  "vmu", fun = "median") , 0)
  expect_equal(compute_pro_actigraph_score(101, "C-PPAC",  "vmu", fun = "median"), 1)
  expect_equal(compute_pro_actigraph_score(200, "C-PPAC",  "vmu", fun = "median"), 1)
  expect_equal(compute_pro_actigraph_score(201, "C-PPAC",  "vmu", fun = "median"), 2)
  expect_equal(compute_pro_actigraph_score(300, "C-PPAC",  "vmu", fun = "median"), 2)
  expect_equal(compute_pro_actigraph_score(301, "C-PPAC",  "vmu", fun = "median"), 3)
  expect_equal(compute_pro_actigraph_score(500, "C-PPAC",  "vmu", fun = "median"), 3)
  expect_equal(compute_pro_actigraph_score(501, "C-PPAC",  "vmu", fun = "median"), 4)
  expect_equal(compute_pro_actigraph_score(599, "C-PPAC",  "vmu", fun = "median"), 4)
  
  
  # C-PPAC / Steps score (mean)
  expect_equal(compute_pro_actigraph_score(1300, "C-PPAC",  "steps", fun = "mean") , 0)
  expect_equal(compute_pro_actigraph_score(1301, "C-PPAC",  "steps", fun = "mean"), 1)
  expect_equal(compute_pro_actigraph_score(2200, "C-PPAC",  "steps", fun = "mean"), 1)
  expect_equal(compute_pro_actigraph_score(2201, "C-PPAC",  "steps", fun = "mean"), 2)
  expect_equal(compute_pro_actigraph_score(4000, "C-PPAC",  "steps", fun = "mean"), 2)
  expect_equal(compute_pro_actigraph_score(4001, "C-PPAC",  "steps", fun = "mean"), 3)
  expect_equal(compute_pro_actigraph_score(5700, "C-PPAC",  "steps", fun = "mean"), 3)
  expect_equal(compute_pro_actigraph_score(5701, "C-PPAC",  "steps", fun = "mean"), 4)
  expect_equal(compute_pro_actigraph_score(5799, "C-PPAC",  "steps", fun = "mean"), 4)
  
  
  # C-PPAC / VMU score (mean)
  expect_equal(compute_pro_actigraph_score(180, "C-PPAC",  "vmu", fun = "mean") , 0)
  expect_equal(compute_pro_actigraph_score(181, "C-PPAC",  "vmu", fun = "mean"), 1)
  expect_equal(compute_pro_actigraph_score(260, "C-PPAC",  "vmu", fun = "mean"), 1)
  expect_equal(compute_pro_actigraph_score(261, "C-PPAC",  "vmu", fun = "mean"), 2)
  expect_equal(compute_pro_actigraph_score(350, "C-PPAC",  "vmu", fun = "mean"), 2)
  expect_equal(compute_pro_actigraph_score(351, "C-PPAC",  "vmu", fun = "mean"), 3)
  expect_equal(compute_pro_actigraph_score(490, "C-PPAC",  "vmu", fun = "mean"), 3)
  expect_equal(compute_pro_actigraph_score(491, "C-PPAC",  "vmu", fun = "mean"), 4)
  
  # D-PPAC / Steps score
  expect_equal(compute_pro_actigraph_score(1000, "D-PPAC",  "steps") , 0)
  expect_equal(compute_pro_actigraph_score(1001, "D-PPAC",  "steps"), 1)
  expect_equal(compute_pro_actigraph_score(3000, "D-PPAC",  "steps"), 1)
  expect_equal(compute_pro_actigraph_score(3001, "D-PPAC",  "steps"), 2)
  expect_equal(compute_pro_actigraph_score(5000, "D-PPAC",  "steps"), 2)
  expect_equal(compute_pro_actigraph_score(5001, "D-PPAC",  "steps"), 3)
  expect_equal(compute_pro_actigraph_score(7000, "D-PPAC",  "steps"), 3)
  expect_equal(compute_pro_actigraph_score(7001, "D-PPAC",  "steps"), 4)
  expect_equal(compute_pro_actigraph_score(8999, "D-PPAC",  "steps"), 4)
  
  
  # D-PPAC / VMU score
  expect_equal(compute_pro_actigraph_score(100, "D-PPAC",  "vmu") , 0)
  expect_equal(compute_pro_actigraph_score(101, "D-PPAC",  "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(200, "D-PPAC",  "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(201, "D-PPAC",  "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(300, "D-PPAC",  "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(301, "D-PPAC",  "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(400, "D-PPAC",  "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(401, "D-PPAC",  "vmu"), 4)
  expect_equal(compute_pro_actigraph_score(600, "D-PPAC",  "vmu"), 4)
  expect_equal(compute_pro_actigraph_score(601, "D-PPAC",  "vmu"), 5)
  expect_equal(compute_pro_actigraph_score(699, "D-PPAC",  "vmu"), 5)

})
