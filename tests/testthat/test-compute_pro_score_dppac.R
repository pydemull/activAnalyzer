test_that("D-PPAC item scores are accurate", {
  
  # English version  R1 ---- 
    q1 <- compute_pro_score_dppac(x = "None at all",  question = "q1", language = "en")
    q2 <- compute_pro_score_dppac(x = "None at all",  question = "q2", language = "en")
    q3 <- compute_pro_score_dppac(x = "None at all",  question = "q3", language = "en")
    q4 <- compute_pro_score_dppac(x = "Not at all",  question = "q4", language = "en")
    q5 <- compute_pro_score_dppac(x = "Not at all",  question = "q5", language = "en")
    q6 <- compute_pro_score_dppac(x = "Not at all",  question = "q6", language = "en")
    q7 <- compute_pro_score_dppac(x = "Not at all",  question = "q7", language = "en")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 0+0+5*4)
  
  # English version  R2 ---- 
    q1 <- compute_pro_score_dppac(x = "A little bit (up to 10 minutes in total)",  question = "q1", language = "en")
    q2 <- compute_pro_score_dppac(x = "A few",  question = "q2", language = "en")
    q3 <- compute_pro_score_dppac(x = "A little bit",  question = "q3", language = "en")
    q4 <- compute_pro_score_dppac(x = "Rarely",  question = "q4", language = "en")
    q5 <- compute_pro_score_dppac(x = "A little bit",  question = "q5", language = "en")
    q6 <- compute_pro_score_dppac(x = "A little bit",  question = "q6", language = "en")
    q7 <- compute_pro_score_dppac(x = "Rarely",  question = "q7", language = "en")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 1+1+5*3)
  
  # English version  R3 ---- 
    q1 <- compute_pro_score_dppac(x = "Some (up to 30 minutes in total)",  question = "q1", language = "en")
    q2 <- compute_pro_score_dppac(x = "Some",  question = "q2", language = "en")
    q3 <- compute_pro_score_dppac(x = "Some",  question = "q3", language = "en")
    q4 <- compute_pro_score_dppac(x = "Sometimes",  question = "q4", language = "en")
    q5 <- compute_pro_score_dppac(x = "Moderately",  question = "q5", language = "en")
    q6 <- compute_pro_score_dppac(x = "Moderately",  question = "q6", language = "en")
    q7 <- compute_pro_score_dppac(x = "Sometimes",  question = "q7", language = "en")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 2+2+5*2)
  
  # English version  R4 ---- 
    q1 <- compute_pro_score_dppac(x = "A lot (up to 1 hour in total)",  question = "q1", language = "en")
    q2 <- compute_pro_score_dppac(x = "A lot",  question = "q2", language = "en")
    q3 <- compute_pro_score_dppac(x = "A lot",  question = "q3", language = "en")
    q4 <- compute_pro_score_dppac(x = "Frequently",  question = "q4", language = "en")
    q5 <- compute_pro_score_dppac(x = "Very",  question = "q5", language = "en")
    q6 <- compute_pro_score_dppac(x = "Very",  question = "q6", language = "en")
    q7 <- compute_pro_score_dppac(x = "Frequently",  question = "q7", language = "en")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 3+3+5*1)
  
  # English version  R5 ---- 
    q1 <- compute_pro_score_dppac(x = "A great deal (more than 1 hour in total)",  question = "q1", language = "en")
    q2 <- compute_pro_score_dppac(x = "A large amount",  question = "q2", language = "en")
    q3 <- compute_pro_score_dppac(x = "A great deal",  question = "q3", language = "en")
    q4 <- compute_pro_score_dppac(x = "All the time",  question = "q4", language = "en")
    q5 <- compute_pro_score_dppac(x = "Extremely",  question = "q5", language = "en")
    q6 <- compute_pro_score_dppac(x = "Extremely",  question = "q6", language = "en")
    q7 <- compute_pro_score_dppac(x = "All the time",  question = "q7", language = "en")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 4+4+5*0)
  
  # French version  R1 ---- 
    q1 <- compute_pro_score_dppac(x = "Pas du tout",  question = "q1", language = "fr")
    q2 <- compute_pro_score_dppac(x = "Aucune",  question = "q2", language = "fr")
    q3 <- compute_pro_score_dppac(x = "Pas du tout",  question = "q3", language = "fr")
    q4 <- compute_pro_score_dppac(x = "Jamais",  question = "q4", language = "fr")
    q5 <- compute_pro_score_dppac(x = "Pas du tout",  question = "q5", language = "fr")
    q6 <- compute_pro_score_dppac(x = "Pas du tout",  question = "q6", language = "fr")
    q7 <- compute_pro_score_dppac(x = "Jamais",  question = "q7", language = "fr")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 0+0+5*4)
  
  # French version  R2 ---- 
    q1 <- compute_pro_score_dppac(x = "Un petit peu (jusqu’à 10 minutes au total)",  question = "q1", language = "fr")
    q2 <- compute_pro_score_dppac(x = "Tr\u00e8s peu",  question = "q2", language = "fr")
    q3 <- compute_pro_score_dppac(x = "Un petit peu",  question = "q3", language = "fr")
    q4 <- compute_pro_score_dppac(x = "Rarement",  question = "q4", language = "fr")
    q5 <- compute_pro_score_dppac(x = "Un petit peu",  question = "q5", language = "fr")
    q6 <- compute_pro_score_dppac(x = "Un petit peu",  question = "q6", language = "fr")
    q7 <- compute_pro_score_dppac(x = "Rarement",  question = "q7", language = "fr")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 1+1+5*3)
  
  # French version  R3 ---- 
    q1 <- compute_pro_score_dppac(x = "Un peu (jusqu’à 30 minutes au total)",  question = "q1", language = "fr")
    q2 <- compute_pro_score_dppac(x = "Quelques-unes",  question = "q2", language = "fr")
    q3 <- compute_pro_score_dppac(x = "Quelques-unes",  question = "q3", language = "fr")
    q4 <- compute_pro_score_dppac(x = "Quelques fois",  question = "q4", language = "fr")
    q5 <- compute_pro_score_dppac(x = "Mod\u00e9r\u00e9ment",  question = "q5", language = "fr")
    q6 <- compute_pro_score_dppac(x = "Mod\u00e9r\u00e9ment",  question = "q6", language = "fr")
    q7 <- compute_pro_score_dppac(x = "Quelques fois",  question = "q7", language = "fr")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 2+2+5*2)
  
  # English version  R4 ---- 
    q1 <- compute_pro_score_dppac(x = "Beaucoup (jusqu’à 1 heure au total)",  question = "q1", language = "fr")
    q2 <- compute_pro_score_dppac(x = "Beaucoup",  question = "q2", language = "fr")
    q3 <- compute_pro_score_dppac(x = "Beaucoup",  question = "q3", language = "fr")
    q4 <- compute_pro_score_dppac(x = "Fr\u00e9quemment",  question = "q4", language = "fr")
    q5 <- compute_pro_score_dppac(x = "Tr\u00e8s",  question = "q5", language = "fr")
    q6 <- compute_pro_score_dppac(x = "Tr\u00e8s",  question = "q6", language = "fr")
    q7 <- compute_pro_score_dppac(x = "Fr\u00e9quemment",  question = "q7", language = "fr")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 3+3+5*1)
  
  # French version  R5 ---- 
    q1 <- compute_pro_score_dppac(x = "Enorm\u00e9ment (plus d\u00271 heure au total)",  question = "q1", language = "fr")
    q2 <- compute_pro_score_dppac(x = "Enorm\u00e9ment",  question = "q2", language = "fr")
    q3 <- compute_pro_score_dppac(x = "Enorm\u00e9ment",  question = "q3", language = "fr")
    q4 <- compute_pro_score_dppac(x = "Tout le temps",  question = "q4", language = "fr")
    q5 <- compute_pro_score_dppac(x = "Extr\u00eamement",  question = "q5", language = "fr")
    q6 <- compute_pro_score_dppac(x = "Extr\u00eamement",  question = "q6", language = "fr")
    q7 <- compute_pro_score_dppac(x = "Tout le temps",  question = "q7", language = "fr")
    
    expect_equal(sum(c(q1, q2, q3, q4, q5, q6, q7)), 4+4+5*0)
})
