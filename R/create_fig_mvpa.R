#' Create a figure showing the mean daily MPVA minutes
#' 
#' The function generates a figure showing the mean of the MVPA minutes in correspondance with the Ekelund et al. (2019; doi: 10.1136/bmj.l4570)
#'     mortality hazard ratios.
#'
#' @param score A numeric value for daily MVPA time in minutes.
#' @param language A character value for setting the language with which the figure should be created: "en" for english; "fr" for french.
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
  
  model_mvpa_up <- loess(y ~ x, data = mvpa_lines %>% dplyr::filter(line == "up"), 
                         control = loess.control(surface = "direct")) 
  
  model_mvpa_low <- loess(y ~ x, data = mvpa_lines %>% dplyr::filter(line == "low"), 
                          control = loess.control(surface = "direct"))
  
  grid_mvpa <-
    mvpa_lines %>%
    modelr::data_grid(
      x = seq(0, 100, 1),
    ) %>%
    modelr::spread_predictions(model_mvpa, model_mvpa_up, model_mvpa_low) %>%
    dplyr::rename(mid = model_mvpa, up = model_mvpa_up, low = model_mvpa_low)
  
# Getting data for plotting patient's result  
  score_mvpa <- data.frame(x = score) %>% 
    modelr::add_predictions(model_mvpa)
  
  
# Creating figure
if (language == "en") {
  g_mvpa <-
    ggplot() +
    geom_ribbon(data = grid_mvpa, aes(x = x, y = up, ymin = low, ymax = up), fill = "grey95") +
    geom_line(data = grid_mvpa, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_mvpa, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_mvpa, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 24), linetype = "dotted") +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 100, 25)) +
    theme_bw() +
    coord_cartesian(xlim = c(0, 100), ylim = c(0.25, 1), expand = FALSE) +
    labs(title = "Daily MVPA minutes", x = "", y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(0, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "Mortality hazard \nratio", 
             x = 73, y = 0.57, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 73, 
             y = 0.5, 
             xend = 58, 
             yend = 0.53, 
             curvature = -.45, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "Uncertainty \nzone", 
             x = 60, y = 0.80, hjust = 0.5, 
             colour = "grey50", fontface = "bold.italic") +
    
    annotate("text", label = "Threshold above which maximum \nof health benefits may be obtained", 
             x = 35, y = 0.37, hjust = 0, vjust = 1, size = 5,
             colour = "black", fontface = "italic") +
    annotate(geom = "curve", 
             x = 35, 
             y = 0.34, 
             xend = 24, 
             yend = 0.36, 
             curvature = -.4, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570", hjust = 0, x = 1, y = 0.265)
  
  return(g_mvpa)
}
  
if (language == "fr") {
  
  g_mvpa <-
    ggplot() +
    geom_ribbon(data = grid_mvpa, aes(x = x, y = up, ymin = low, ymax = up), fill = "grey95") +
    geom_line(data = grid_mvpa, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_mvpa, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_mvpa, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 24), linetype = "dotted") +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_mvpa, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(-0.5, 100), breaks = seq(0, 100, 25)) +
    theme_bw() +
    coord_cartesian(xlim = c(0, 100), ylim = c(0.25, 1), expand = FALSE) +
    labs(title = "Minutes MVPA journalières", x = "", y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.title = element_text(face = "bold" , size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(0, 1, 0, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    annotate("text", label = "Rapport de risque \npour la mortalité", 
             x = 70, y = 0.57, hjust = 0, 
             fontface = "bold.italic", colour = "#3366FF") +
    annotate(geom = "curve", 
             x = 73, 
             y = 0.5, 
             xend = 58, 
             yend = 0.53, 
             curvature = -.45, arrow = arrow(length = unit(2, "mm")),
             colour = "#3366FF") +
    annotate("text", label = "Zone \nd'incertitude", 
             x = 60, y = 0.80, hjust = 0.5, 
             colour = "grey50", fontface = "bold.italic") +
    
    annotate("text", label = "Seuil au-dessus duquel le max. \nde bénéfices de santé \npourrait être obtenu", 
             x = 38, y = 0.42, hjust = 0, vjust = 1, size = 5,
             colour = "black", fontface = "italic") +
    annotate(geom = "curve", 
             x = 36.5, 
             y = 0.34, 
             xend = 24, 
             yend = 0.36, 
             curvature = -.4, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "Réf: Ekelund et al. BMJ 2019, l4570", hjust = 0, x = 1, y = 0.265)
  
  return(g_mvpa)
  
  }
  
}
