#' Create a figure showing the mean daily sedentary (SED) time
#' 
#' The function generates a figure showing mortality hazard ratio in correspondence with daily SED hours. The figure is based on
#'     data extracted from Ekelund et al. paper (2019; doi: 10.1136/bmj.l4570).
#'
#' @param score A numeric value for mean daily SED time in minutes.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' create_fig_sed(score = 400)
#' 
create_fig_sed <- function(score, language = c("en", "fr")) {
  
language <- match.arg(language)
  
  
# Building lines showing the estimate and the related confidence limits of mortality 
# hazard ratio (Ekelund et al., 2020, http://dx.doi.org/10.1136/bmj.l4570)

  model_sed <- loess(y ~ x, data = sed_lines %>% dplyr::filter(line == "mid"), 
                     control = loess.control(surface = "direct"))
  
    grid_sed <-
    sed_lines %>%
    modelr::data_grid(
      x = seq(6, 12, 0.1),
    ) %>%
    modelr::spread_predictions(model_sed) %>%
    dplyr::rename(mid = model_sed)

# Getting data for plotting patient's result  
  
  score_sed <- data.frame(x = score / 60) %>% 
    modelr::add_predictions(model_sed)
  
# Creating figure
if (language == "en" && score/60 >= 6 && score/60 <= 12) { 
  
  g_sed <-
    ggplot() +
    geom_rect(data = grid_sed, aes(xmin = 6, xmax = 12, ymin = 0.5,  ymax = 3.1), fill = "white", color = "grey50") + 
    geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_point(data = score_sed, aes(x = 7.5, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(1, 15), breaks = seq(6, 12, 1)) +
    coord_cartesian(xlim = c(6, 12), ylim = c(0.5, 3.1), expand = FALSE, clip = "off") +
    labs(title = "Mortality hazard ratio vs. SED hours / day", x = "", y = NULL) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "The curve shows the \nmortality hazard ratio in \nadults older than 40 yr old", 
             x = 9.4, y = 0.85, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 9.35, 
             y = 0.85, 
             xend = 8.9, 
             yend = 1.10, 
             curvature = -.35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570 (modified)", hjust = 0, x = 6.1, y = 0.54) +
    annotate("text", label = "Reference \npoint", hjust = 1, vjust = 0, x = 7.45, y = 1.13, color = "grey30", fontface = "bold")

  return(g_sed)
  
  }
  
if (language == "fr" && score/60 >= 6 && score/60 <= 12) {  
  
  g_sed <-
    ggplot() +
    geom_rect(data = grid_sed, aes(xmin = 6, xmax = 12, ymin = 0.5,  ymax = 3.1), fill = "white", color = "grey50") + 
    geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_point(data = score_sed, aes(x = 7.5, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(1, 15), breaks = seq(6, 12, 1)) +
    coord_cartesian(xlim = c(6, 12), ylim = c(0.5, 3.1), expand = FALSE, clip = "off") +
    labs(title = "Risque de mortalit\u00e9 vs. Heures SED / jour", x = "", y = NULL) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de plus de \n40 ans",  
             x = 9.4, y = 0.85, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 9.35, 
             y = 0.85, 
             xend = 8.9, 
             yend = 1.10, 
             curvature = -.35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "R\u00e9f: Ekelund et al. BMJ 2019, l4570 (modifi\u00e9)", hjust = 0, x = 6.1, y = 0.54) +
    annotate("text", label = "Point de \nr\u00e9f\u00e9rence", hjust = 1, vjust = 0, x = 7.45, y = 1.13, color = "grey30", fontface = "bold")

  return(g_sed)
  
}
  
  
  if (language == "en" && (score/60 < 6 | score/60 > 12)) { 
    
    g_sed <-
      ggplot() +
      geom_rect(data = grid_sed, aes(xmin = 6, xmax = 12, ymin = 0.5,  ymax = 3.1), fill = "white", color = "grey50") + 
      geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
      geom_point(data = score_sed, aes(x = 7.5, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_y_continuous(trans = scales::log2_trans()) +
      scale_x_continuous(limits = c(1, 15), breaks = seq(6, 12, 1)) +
      coord_cartesian(xlim = c(6, 12), ylim = c(0.5, 3.1), expand = FALSE, clip = "off") +
      labs(title = "Mortality hazard ratio vs. SED hours / day", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "The curve shows the \nmortality hazard ratio in \nadults older than 40 yr old", 
               x = 9.4, y = 0.85, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 9.35, 
               y = 0.85, 
               xend = 8.9, 
               yend = 1.10, 
               curvature = -.35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570 (modified)", hjust = 0, x = 6.1, y = 0.54) +
      annotate("text", label = "Reference \npoint", hjust = 1, vjust = 0, x = 7.45, y = 1.13, color = "grey30", fontface = "bold") +
      annotate("text", label = "The recorded score is outside the \nrange of the X axis of the original figure.", 
               hjust = 0, vjust = 1, x = 6.5, y = 2.9, size = 4, color = "red", fontface = "bold")
    
    return(g_sed)
    
  }
  
  if (language == "fr" && (score/60 < 6 | score/60 > 12)) {  
    
    g_sed <-
      ggplot() +
      geom_rect(data = grid_sed, aes(xmin = 6, xmax = 12, ymin = 0.5,  ymax = 3.1), fill = "white", color = "grey50") + 
      geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
      geom_point(data = score_sed, aes(x = 7.5, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_y_continuous(trans = scales::log2_trans()) +
      scale_x_continuous(limits = c(1, 15), breaks = seq(6, 12, 1)) +
      coord_cartesian(xlim = c(6, 12), ylim = c(0.5, 3.1), expand = FALSE, clip = "off") +
      labs(title = "Risque de mortalit\u00e9 vs. Heures SED / jour", x = "", y = NULL) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_text(face = "bold" , size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "beige", color = "beige"),
            plot.margin = margin(1, 1, 0, 1, "cm"),
            plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
      annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de plus de \n40 ans", 
               x = 9.4, y = 0.85, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 9.35, 
               y = 0.85, 
               xend = 8.9, 
               yend = 1.10, 
               curvature = -.35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "R\u00e9f: Ekelund et al. BMJ 2019, l4570 (modifi\u00e9)", hjust = 0, x = 6.1, y = 0.54) +
      annotate("text", label = "Point de \nr\u00e9f\u00e9rence", hjust = 1, vjust = 0, x = 7.45, y = 1.13, color = "grey30", fontface = "bold") +
      annotate("text", label = "Le score mesur\u00e9 est en-dehors de \nl\u2019\u00e9tendue des valeurs de l\u2019axe X de \nla figure originale.", 
               hjust = 0, vjust = 1, x = 6.5, y = 2.9, size = 4, color = "red", fontface = "bold")
    
    return(g_sed)
    
  }
}

