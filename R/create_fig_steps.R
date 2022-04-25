#' Create a figure showing the mean daily step count
#' 
#' The function generates a figure showing the daily mean of the daily step count in correspondence with the Tudor-Locke et al. 
#'     (2011; doi: 10.1186/1479-5868-8-79) categories.
#'
#' @param score A numeric value for mean daily step count.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' create_fig_steps(score = 12500)
#' 
create_fig_steps <- function(score, language = c("en", "fr")) {
  
language <- match.arg(language)
  
# Data for figure / categories for all adults
  table_steps1 <-
    tibble::tibble(
      x = c(0, 0, 0.25, 0.5, 0.75, 1),
      label = as.factor(c(
        "Sedentary, basal physical activity", 
        "Sedentary, limited physical activity", 
        "Low active", 
        "Somewhat active",
        "Active",
        "Highly active"
      )),
      start = c(0, 2500, 5000, 7500, 10000, 12500),
      end = c(2500, 5000, 7500, 10000, 12500, 18000)
    ) %>%
    dplyr::mutate(label =  forcats::fct_relevel(label, "Sedentary, basal physical activity", 
                                                "Sedentary, limited physical activity",
                                                "Low active", 
                                                "Somewhat active",
                                                "Active",
                                                "Highly active")) 
  
# Data for figure / categories for specific populations
  table_steps2 <-
    tibble::tibble(
      label = as.factor(c(
        "Adults (20-65 ans)", 
        "Healthy older adults (65+ yr)", 
        "People with disability \n and/or chronic illness"
      )),
      y = c(7000, 7000, 6500),
      yend = c(8000, 10000, 8500),
      x = c(0.9, 0.70, 0.50),
      xend = c(0.9, 0.70, 0.50),
      xmin = x-0.03,
      xmax = x+0.03
    ) %>%
    dplyr::mutate(label =  forcats::fct_relevel(label, "Adults (20-65 ans)", 
                                                "Healthy older adults (65+ yr)", 
                                                "People with disability \n and/or chronic illness")) 
  band_color <- scales::seq_gradient_pal("white", "blue")(seq(0,1,length.out=6))
  
# Creating figure 
  if (language == "en") {
    
  g_steps <-
    ggplot(data = table_steps1, aes(x = x, y = start)) +
    geom_rect(aes(ymin = start, ymax = end, fill = label), xmin = -Inf, xmax = Inf, alpha = 0.5) +
    geom_vline(aes(xintercept = 0.40), size = 0.4, color = "grey30") +
    geom_vline(aes(xintercept = 0.20,), size = 0.5, color = "grey30", linetype ="dotted") +
    geom_segment(x = 0, xend = 0.4, y = score, yend = score, 
                 size = 0.5, color = "grey30", linetype ="dotted")   +
    geom_point(aes(x = 0.2, y = score), color = "red", size = 7, shape = 1) +
    geom_point(aes(x = 0.2, y = score), color = "red", size = 4, shape = 16) +
    geom_point(aes(x = 0.2, y = score), color = "red", size = 7, shape = 3) +
    geom_segment(data = table_steps2, aes(x = x, xend = xend, y = y, yend = yend, color = label), size = 1, 
                 arrow = arrow(length = unit(0.09, "inches"))) +
    geom_errorbarh(data = table_steps2, aes(y = y, xmin = xmin, xmax = xmax, color = label), size = 1, height = 0) + 
    scale_y_continuous(limits = c(0, 18000),
                       labels = as.character(c(0, 2500, 5000, 7500, 10000, 12500, 18000)), 
                       breaks = c(0, 2500, 5000, 7500, 10000, 12500, 18000)) +
    scale_fill_manual(values = band_color)  +
    scale_color_brewer(palette="Dark2") +
    theme_bw() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, 
         fill = "Activity levels for healthy adults (Tudor-Locke et al., 2O11)", 
         color = "Expected values when people reach \nMVPA guidelines (Tudor-Locke et al., 2011)") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.position = "none",
          legend.title = element_text(face = "bold", size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    guides(fill="none") +
    ggtitle("Daily steps") +
    annotate("text", label = "Expected values when \nmeeting MVPA guidelines:", x = 0.7, y = 1000, 
             hjust = 0, fontface = "bold", size = 6,   color = "grey30") +
    annotate("text", label = "Adults", x = 0.9, y = 8300, hjust = 0, fontface = "bold", size = 5) +
    annotate("text", label = "Healthy older adults", x = 0.7, y = 10300, hjust = 0, fontface = "bold", size = 5) +
    annotate("text", label = "People with disability and/or chronic disease", x = 0.5, y = 8800, hjust = 0, fontface = "bold", size = 5) +
    annotate("text", label = "Ref: Tudor-Locke et al. Int J Behav Nutr Phys Act 2011, 8 (1), 79", x = 0.9, y = 17900, hjust = 1)
  
  return(g_steps)
  
  }
  
  if (language == "fr") {
    
    g_steps <-
      ggplot(data = table_steps1, aes(x = x, y = start)) +
      geom_rect(aes(ymin = start, ymax = end, fill = label), xmin = -Inf, xmax = Inf, alpha = 0.5) +
      geom_vline(aes(xintercept = 0.40), size = 0.4, color = "grey30") +
      geom_vline(aes(xintercept = 0.20,), size = 0.5, color = "grey30", linetype ="dotted") +
      geom_segment(x = 0, xend = 0.4, y = score, yend = score, 
                   size = 0.5, color = "grey30", linetype ="dotted")   +
      geom_point(aes(x = 0.2, y = score), color = "red", size = 7, shape = 1) +
      geom_point(aes(x = 0.2, y = score), color = "red", size = 4, shape = 16) +
      geom_point(aes(x = 0.2, y = score), color = "red", size = 7, shape = 3) +
      geom_segment(data = table_steps2, aes(x = x, xend = xend, y = y, yend = yend, color = label), size = 1, 
                   arrow = arrow(length = unit(0.09, "inches"))) +
      geom_errorbarh(data = table_steps2, aes(y = y, xmin = xmin, xmax = xmax, color = label), size = 1, height = 0) + 
      scale_y_continuous(limits = c(0, 18000),
                         labels = as.character(c(0, 2500, 5000, 7500, 10000, 12500, 18000)), 
                         breaks = c(0, 2500, 5000, 7500, 10000, 12500, 18000)) +
      scale_fill_manual(values = band_color)  +
      scale_color_brewer(palette="Dark2") +
      theme_bw() +
      coord_flip(expand = FALSE) +
      labs(x = NULL, y = NULL, 
           fill = "Activity levels for healthy adults (Tudor-Locke et al., 2O11)", 
           color = "Expected values when people reach \nMVPA guidelines (Tudor-Locke et al., 2011)") +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 13),
            legend.position = "none",
            legend.title = element_text(face = "bold", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0.5, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      guides(fill="none") +
      ggtitle("Nombre de pas journalier") +
      annotate("text", label = "Valeurs attendues lorsque \nles personnes atteignent les recommandations d'AP :", x = 0.68, y = 300, 
               hjust = 0, vjust = 0.5, fontface = "bold", size = 5.2,   color = "grey30") +
      annotate("text", label = "Adultes", x = 0.9, y = 8300, hjust = 0, fontface = "bold", size = 5) +
      annotate("text", label = "Personnes \u00e2g\u00e9es en bonne sant\u00e9", x = 0.7, y = 10300, hjust = 0, fontface = "bold", size = 5) +
      annotate("text", label = "Personnes avec incapacit\u00e9 et/ou maladie chronique", x = 0.5, y = 8800, hjust = 0, fontface = "bold", size = 5) +
      annotate("text", label = "R\u00e9f: Tudor-Locke et al. Int J Behav Nutr Phys Act 2011, 8 (1), 79", x = 0.9, y = 17900, hjust = 1)
    
    return(g_steps)
    
  }
  
}