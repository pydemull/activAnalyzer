#' Create a figure showing the mean daily MVPA time
#' 
#' The function generates a figure showing mortality hazard ratio in correspondence with daily MVPA minutes. The figure is based on
#'     data extracted from Ekelund et al. paper (2019; doi: 10.1136/bmj.l4570).
#'
#' @param score A numeric value for mean daily MVPA time in minutes.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' create_fig_mvpa(score = 27)
#' 
create_fig_mvpa <- function(score, language = c("en", "fr")) {

language <- match.arg(language)
  
  
# Building lines showing the estimate and the related confidence limits of mortality 
# hazard ratio (Ekelund et al., 2020, http://dx.doi.org/10.1136/bmj.l4570)
  
  model_mvpa <- loess(y ~ x, data = mvpa_lines %>% dplyr::filter(line == "mid"), 
                      control = loess.control(surface = "direct"))
  
  grid_mvpa <-
    mvpa_lines %>%
    modelr::data_grid(
      x = seq(0, 65, 1),
    ) %>%
    modelr::spread_predictions(model_mvpa) %>%
    dplyr::rename(mid = model_mvpa)
  
# Getting data for plotting patient's result  
  score_mvpa <- data.frame(x = score) %>% 
    modelr::add_predictions(model_mvpa)
  
  
# Creating figure
if (language == "en" && score <= 65) {
  
  g_mvpa <-
    ggplot() +
    geom_rect(data = grid_mvpa, aes(xmin = 0, xmax = 65, ymin = 0.2, ymax = 2.1), fill = "white", color = "grey50") + 
    geom_line(data = grid_mvpa, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
    geom_point(data = score_mvpa, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans(), breaks = seq(0.2, 2.1, 0.1)) +
    scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 65, 13)) +
    theme_bw() +
    coord_cartesian(xlim = c(0, 65), ylim = c(0.2, 2.1), expand = FALSE, clip = "off") +
    labs(title = "Mortality hazard ratio vs. MVPA minutes / day", x = "", y = NULL) +
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
    annotate("text", label = "The curve shows the \nmortality hazard ratio in \nadults older than 40 yr old", 
             x = 31.5, y = 0.75, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 31, 
             y = 0.75, 
             xend = 24, 
             yend = 0.405, 
             curvature = .35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570 (modified)", hjust = 0, x = 1, y = 0.22) +
    annotate("text", label = "Reference point", hjust = 0, x = 2, y = 1.04, color = "grey30", fontface = "bold")

  return(g_mvpa)
}
  
if (language == "fr" && score <= 65) {
  
  g_mvpa <-
    ggplot() +
    geom_rect(data = grid_mvpa, aes(xmin = 0, xmax = 65, ymin = 0.2, ymax = 2.1), fill = "white", color = "grey50") + 
    geom_line(data = grid_mvpa, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
    geom_point(data = score_mvpa, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans(), breaks = seq(0.2, 2.1, 0.1)) +
    scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 65, 13)) +
    theme_bw() +
    coord_cartesian(xlim = c(0, 65), ylim = c(0.2, 2.1), expand = FALSE, clip = "off") +
    labs(title = "Risque de mortalit\u00e9 vs. Minutes MVPA / jour", x = "", y = NULL) +
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
    annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de plus de \n40 ans", 
             x = 31.5, y = 0.75, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 31, 
             y = 0.75, 
             xend = 24, 
             yend = 0.405, 
             curvature = .35, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "R\u00e9f: Ekelund et al. BMJ 2019, l4570 (modifi\u00e9)", hjust = 0, x = 1, y = 0.22) +
    annotate("text", label = "Point de r\u00e9f\u00e9rence", hjust = 0, x = 2, y = 1.04, color = "grey30", fontface = "bold")

  return(g_mvpa)
  
}
  
  if (language == "en" && score > 65) {
    g_mvpa <-
      ggplot() +
      geom_rect(data = grid_mvpa, aes(xmin = 0, xmax = 65, ymin = 0.2, ymax = 2.1), fill = "white", color = "grey50") + 
      geom_line(data = grid_mvpa, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_mvpa, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_y_continuous(trans = scales::log2_trans(), breaks = seq(0.2, 2.1, 0.1)) +
      scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 65, 13)) +
      theme_bw() +
      coord_cartesian(xlim = c(0, 65), ylim = c(0.2, 2.1), expand = FALSE, clip = "off") +
      labs(title = "Mortality hazard ratio vs. MVPA minutes / day", x = "", y = NULL) +
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
      annotate("text", label = "The curve shows the \nmortality hazard ratio in \nadults older than 40 yr old", 
               x = 31.5, y = 0.75, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 31, 
               y = 0.75, 
               xend = 24, 
               yend = 0.405, 
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570 (modified)", hjust = 0, x = 1, y = 0.22) +
      annotate("text", label = "Reference point", hjust = 0, x = 2, y = 1.04, color = "grey30", fontface = "bold") +
      annotate("text", label = "The recorded score is beyond the upper \nlimit of the X axis of the original figure.", 
               hjust = 0, vjust = 1, x = 5, y = 1.9, size = 4, color = "red", fontface = "bold")
    
    return(g_mvpa)
  }
  
  if (language == "fr" && score > 65) {
    
    g_mvpa <-
      ggplot() +
      geom_rect(data = grid_mvpa, aes(xmin = 0, xmax = 65, ymin = 0.2, ymax = 2.1), fill = "white", color = "grey50") + 
      geom_line(data = grid_mvpa, aes (x = x, y = mid), linewidth = 1, colour = "#3366FF") +
      geom_point(data = score_mvpa, aes(x = 0, y = 1), shape = 21, colour = "#3366FF", fill = "grey95", size = 5, stroke = 1.5) +
      scale_y_continuous(trans = scales::log2_trans(), breaks = seq(0.2, 2.1, 0.1)) +
      scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 65, 13)) +
      theme_bw() +
      coord_cartesian(xlim = c(0, 65), ylim = c(0.2, 2.1), expand = FALSE, clip = "off") +
      labs(title = "Risque de mortalit\u00e9 vs. Minutes MVPA / jour", x = "", y = NULL) +
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
      annotate("text", label = "La courbe montre le \nrisque de mortalit\u00e9 chez \ndes adultes de plus de \n40 ans", 
               x = 31.5, y = 0.75, hjust = 0, 
               fontface = "bold.italic", colour = "#3366FF") +
      annotate(geom = "curve", 
               x = 31, 
               y = 0.75, 
               xend = 24, 
               yend = 0.405, 
               curvature = .35, arrow = arrow(length = unit(2, "mm")),
               colour = "#3366FF") +
      annotate("text", label = "R\u00e9f: Ekelund et al. BMJ 2019, l4570 (modifi\u00e9)", hjust = 0, x = 1, y = 0.22) +
      annotate("text", label = "Point de r\u00e9f\u00e9rence", hjust = 0, x = 2, y = 1.04, color = "grey30", fontface = "bold") +
      annotate("text", label = "Le score mesur\u00e9 est au\u002ddel\u00e0 de la limite \nsup\u00e9rieure de l\u2019axe X de la figure originale.", 
               hjust = 0, vjust = 1, x = 5, y = 1.9, size = 4, color = "red", fontface = "bold")
    
    return(g_mvpa)
    
  }
  
}
