test_that("PROactive score is correct", {
  
  # Steps score
  expect_equal(compute_pro_actigraph_score(1300, "steps") , 0)
  expect_equal(compute_pro_actigraph_score(1301, "steps"), 1)
  expect_equal(compute_pro_actigraph_score(2200, "steps"), 1)
  expect_equal(compute_pro_actigraph_score(2201, "steps"), 2)
  expect_equal(compute_pro_actigraph_score(4000, "steps"), 2)
  expect_equal(compute_pro_actigraph_score(4001, "steps"), 3)
  expect_equal(compute_pro_actigraph_score(5700, "steps"), 3)
  expect_equal(compute_pro_actigraph_score(5701, "steps"), 4)
  expect_equal(compute_pro_actigraph_score(5799, "steps"), 4)
  
  
  # VMU score
  expect_equal(compute_pro_actigraph_score(180, "vmu") , 0)
  expect_equal(compute_pro_actigraph_score(181, "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(260, "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(261, "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(350, "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(351, "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(490, "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(491, "vmu"), 4)
  expect_equal(compute_pro_actigraph_score(499, "vmu"), 4)
  
})
