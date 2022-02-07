#' Create a figure showing the mean daily MVPA/SED ratio
#' 
#' The function generates a figure showing the daily mean of the MVPA/SED ratio in correspondence with the Chastin et al. (2021; doi: 10.1123/jpah.2020-0635)
#'     mortality hazard ratios.
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
create_fig_ratio_mvpa_sed <- function(score, language = c("en", "fr")) {
  
language <- match.arg(language)
  
# Building lines showing the estimate and the related confidence limits of mortality 
# hazard ratio (Ekelund et al., 2020)
  
  model_ratio <- loess(y ~ x, data = ratio_lines %>% dplyr::filter(line == "mid"), 
                       span = 0.35, control = loess.control(surface = "direct"))
  
  model_ratio_up <- loess(y ~ x, data = ratio_lines %>% dplyr::filter(line == "up"), 
                          span = 0.35, control = loess.control(surface = "direct")) 
  
  model_ratio_low <- loess(y ~ x, data = ratio_lines %>% dplyr::filter(line == "low"), 
                           span = 0.35, control = loess.control(surface = "direct"))
  
  grid_ratio <-
    ratio_lines %>%
    modelr::data_grid(
      x = seq(0, 0.4, 0.001),
    ) %>%
    modelr::spread_predictions(model_ratio, model_ratio_up, model_ratio_low) %>%
    dplyr::rename(mid = model_ratio, up = model_ratio_up, low = model_ratio_low)
  
# Getting data for plotting patient's result  
  
  score_ratio <- data.frame(x = score) %>% 
    modelr::add_predictions(model_ratio)
  
# Creating figure
if (language == "en") { 
  
  g_ratio <-
    ggplot() +
    geom_ribbon(data = grid_ratio, aes(x = x, y = up, 
                                       ymin = low, ymax = up), fill = "grey95") +
    geom_line(data = grid_ratio, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_ratio, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_ratio, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 0.04), linetype = "dotted") +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_x_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
    coord_cartesian(xlim = c(0, 0.4), ylim = c(0.1, 1.3), expand = FALSE) +
    labs(title = "Daily MVPA/SED ratio", x = "", y = NULL) +
    theme_bw() +
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
    annotate("text", label = "Threshold above which most of \nhealth benefits may be obtained", 
             x = 0.068, y = 1.12, hjust = 0, size = 5,
             colour = "black", fontface = "italic") +
    annotate(geom = "curve", 
             x = 0.065, 
             y = 1.17, 
             xend = 0.043, 
             yend = 1.15, 
             curvature = .5, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "Ref: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637", hjust = 0, x = 0.005, y = 0.15)
  
  return(g_ratio)
}
  
if (language == "fr") {  
  
  g_ratio <-
    ggplot() +
    geom_ribbon(data = grid_ratio, aes(x = x, y = up, 
                                       ymin = low, ymax = up), fill = "grey95") +
    geom_line(data = grid_ratio, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_ratio, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_ratio, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 0.04), linetype = "dotted") +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_ratio, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_x_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
    coord_cartesian(xlim = c(0, 0.4), ylim = c(0.1, 1.3), expand = FALSE) +
    labs(title = "Ratio MVPA/SED journalier", x = "", y = NULL) +
    theme_bw() +
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
    annotate("text", label = "Seuil au-dessus duquel la plupart des \nb\u00e9n\u00e9fices de sant\u00e9 pourraient \u00eAtre obtenus", 
             x = 0.068, y = 1.13, hjust = 0,
             size = 5,
             colour = "black", 
             fontface = "italic") +
    annotate(geom = "curve", 
             x = 0.065, 
             y = 1.17, 
             xend = 0.043, 
             yend = 1.15, 
             curvature = .5, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "R\u00e9f: Chastin et al. J Phys Act Health 2021, 18 (6), 631\u2013637", hjust = 0, x = 0.005, y = 0.15)
  
  return(g_ratio)
  
  }
  
}