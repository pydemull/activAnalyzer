test_that("PROactive score is correct", {
  
  # Steps score
  expect_equal(compute_pro_actigraph_score(999, "steps") , 0)
  expect_equal(compute_pro_actigraph_score(1000, "steps"), 1)
  expect_equal(compute_pro_actigraph_score(1199, "steps"), 1)
  expect_equal(compute_pro_actigraph_score(2000, "steps"), 2)
  expect_equal(compute_pro_actigraph_score(3999, "steps"), 2)
  expect_equal(compute_pro_actigraph_score(4000, "steps"), 3)
  expect_equal(compute_pro_actigraph_score(5999, "steps"), 3)
  expect_equal(compute_pro_actigraph_score(6000, "steps"), 4)
  expect_equal(compute_pro_actigraph_score(6999, "steps"), 4)
  
  
  # VMU score
  expect_equal(compute_pro_actigraph_score(99, "vmu") , 0)
  expect_equal(compute_pro_actigraph_score(100, "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(199, "vmu"), 1)
  expect_equal(compute_pro_actigraph_score(200, "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(299, "vmu"), 2)
  expect_equal(compute_pro_actigraph_score(300, "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(499, "vmu"), 3)
  expect_equal(compute_pro_actigraph_score(500, "vmu"), 4)
  expect_equal(compute_pro_actigraph_score(599, "vmu"), 4)
  
})
