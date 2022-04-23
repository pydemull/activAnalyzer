test_that("Rasch transformation works", {
  
  # C-PPAC
    diff_score <- rasch_transform(x = 14, quest = "C-PPAC", score = "difficulty")
    amount_score <- rasch_transform(x = 3, quest = "C-PPAC", score = "quantity")
    total_score <- (diff_score + amount_score) / 2
    
    expect_equal(total_score, (48+33) / 2)
    
    
  # D-PPAC
    diff_score <- rasch_transform(x = 14, quest = "D-PPAC", score = "difficulty")
    amount_score <- rasch_transform(x = 3, quest = "D-PPAC", score = "quantity")
    total_score <- (diff_score + amount_score) / 2
    
    expect_equal(total_score, (65+25) / 2)
  
})
