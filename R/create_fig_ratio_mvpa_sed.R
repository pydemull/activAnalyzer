#' Create a figure showing the mean daily MVPA/SED ratio
#' 
#' The function generates a figure showing mortality hazard ratio in correspondence with the daily mean of the MVPA/SED ratio. 
#'     The figure is based on data extracted from Chastin et al. paper (2021; doi: 10.1123/jpah.2020-0635).
#'
#' @param score A numeric value for mean daily MVPA/SED ratio.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' create_fig_ratio_mvpa_sed(score = 0.06)
#'
create_fig_ratio_mvpa_sed <- function(score, language = c("en", "fr", "de")) {
  
language <- match.arg(language)
  
# Building lines showing the estimate and the related confidence limits of mortality 
# hazard ratio (Ekelund et al., 2020)
  
  model_ratio <- loess(y ~ x, data = ratio_lines %>% dplyr::filter(line == "mid"), 
                       span = 0.35, control = loess.control(surface = "direct"))
  
  
  grid_ratio <-
    ratio_lines %>%
    modelr::data_grid(
      x = seq(0, 0.25, 0.001),
    ) %>%
    modelr::spread_predictions(model_ratio) %>%
    dplyr::rename(mid = model_ratio)
  
# Getting data for plotting patient's result  
  
  score_ratio <- data.frame(x = score) %>% 
    modelr::add_predictions(model_ratio)
  
# Creating figure
if (language == "en" && score <= 0.25) { 
  
  g_ratio <-
    ggplot() +
    geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
    geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
    geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
    coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
    labs(title = "Mortality hazard ratio vs. Daily MVPA/SED ratio", x = "", y = NULL) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "The curve shows the \nmortality hazard ratio in \n50- to 79-yr old adults.", 
             x = 0.155, y = 0.6, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 0.150, 
             y = 0.6, 
             xend = 0.12, 
             yend = 0.43, 
             curvature = .35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "Ref: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (modif.)", hjust = 0, x = 0.004, y = 0.145) +
    annotate("text", label = "Reference point", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold")

  return(g_ratio)
}
  
if (language == "fr" && score <= 0.25) {  
  
  g_ratio <-
    ggplot() +
    geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
    geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
    geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
    coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
    labs(title = "Risque de mortalit\u00e9 vs. Ratio MVPA/SED journalier", x = "", y = NULL) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de 50 \n\u00e0 79 ans", 
             x = 0.155, y = 0.6, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 0.150, 
             y = 0.6, 
             xend = 0.12, 
             yend = 0.43, 
             curvature = .35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "R\u00e9f: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (modifi\u00e9)", hjust = 0, x = 0.004, y = 0.145) +
    annotate("text", label = "Point de r\u00e9f\u00e9rence", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold")

  return(g_ratio)
  
}
  
  if (language == "de" && score <= 0.25) { 
    
    g_ratio <-
      ggplot() +
      geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
      geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
      geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
      geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
      scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
      coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
      labs(title = "Sterblichkeitsrisiko vs. T\xc3\xa4gliches MVPA/SED-Verh\xc3\xa4ltnis", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "Die Kurve zeigt den \nmortality hazard ratio \nbei  adults, die \xc3\xa4lter als \n40 Jahre alt sind.", 
               x = 0.155, y = 0.6, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 0.150, 
               y = 0.6, 
               xend = 0.12, 
               yend = 0.43, 
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "Ref: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (an.)", hjust = 0, x = 0.004, y = 0.145) +
      annotate("text", label = "Referenzpunkt", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold")
    
    return(g_ratio)
  }
  
  
  if (language == "en" && score > 0.25) { 
    
    g_ratio <-
      ggplot() +
      geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
      geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
      coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
      labs(title = "Mortality hazard ratio vs. Daily MVPA/SED ratio", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "The curve shows the \nmortality hazard ratio in \n50- to 79-yr old adults.", 
               x = 0.155, y = 0.6, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 0.150, 
               y = 0.6, 
               xend = 0.12, 
               yend = 0.43, 
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "Ref: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (modif.)", hjust = 0, x = 0.004, y = 0.145) +
      annotate("text", label = "Reference point", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold") +
      annotate("text", label = "The recorded score is beyond the upper \nlimit of the X axis of the original figure.", 
               hjust = 0, vjust = 1, x = 0.06, y = 1.20, size = 4, color = "red", fontface = "bold")
    
    return(g_ratio)
  }
  
  
  if (language == "fr" && score > 0.25) {  
    
    g_ratio <-
      ggplot() +
      geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
      geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
      coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
      labs(title = "Risque de mortalit\u00e9 vs. Ratio MVPA/SED journalier", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de 50 \n\u00e0 79 ans", 
               x = 0.155, y = 0.6, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 0.150, 
               y = 0.6, 
               xend = 0.12, 
               yend = 0.43,  
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "R\u00e9f: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (modifi\u00e9)", hjust = 0, x = 0.004, y = 0.145) +
      annotate("text", label = "Point de r\u00e9f\u00e9rence", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold") +
      annotate("text", label = "Le score mesur\u00e9 est au\u002ddel\u00e0 de la limite \nsup\u00e9rieure de l\u2019axe X de la figure originale.", 
               hjust = 0, vjust = 1, x = 0.06, y = 1.20, size = 4, color = "red", fontface = "bold")
    
    return(g_ratio)
    
  }
  
  if (language == "de" && score > 0.25) { 
    
    g_ratio <-
      ggplot() +
      geom_rect(data = grid_ratio, aes(xmin = 0, xmax = 0.25, ymin = 0.1, ymax = 1.25), fill = "white", color = "grey50") + 
      geom_line(data = grid_ratio, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_ratio, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_x_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
      coord_cartesian(xlim = c(0, 0.25), ylim = c(0.1, 1.25), expand = FALSE, clip = "off") +
      labs(title = "Sterblichkeitsrisiko vs. T\xc3\xa4gliches MVPA/SED-Verh\xc3\xa4ltnis", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "Die Kurve zeigt den \nmortality hazard ratio \nbei adults, die \xc3\xa4lter als \n40 Jahre alt sind.", 
               x = 0.155, y = 0.6, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 0.150, 
               y = 0.6, 
               xend = 0.12, 
               yend = 0.43, 
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "Ref: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637 (an.)", hjust = 0, x = 0.004, y = 0.145) +
      annotate("text", label = "Referenzpunkt", hjust = 0, x = 0.008, y = 1.02, color = "grey30", fontface = "bold") +
      annotate("text", label = "Der ermittelte Wert liegt \xc3\xbcber dem oberen \nlimit der X-Achse der urspr\xc3\xbcnglichen \nAbbildung.", 
               hjust = 0, vjust = 1, x = 0.06, y = 1.20, size = 4, color = "red", fontface = "bold")
    
    return(g_ratio)
  }
  
}