#' Create a figure showing the mean daily sedentary (SED) time
#' 
#' The function generates a figure showing the daily mean of SED time in correspondence with the Ekelund et al. (2019; doi: 10.1136/bmj.l4570)
#'     mortality hazard ratios.
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
  
  model_sed_up <- loess(y ~ x, data = sed_lines %>% dplyr::filter(line == "up"), 
                        control = loess.control(surface = "direct")) 
  
  model_sed_low <- loess(y ~ x, data = sed_lines %>% dplyr::filter(line == "low"), 
                         control = loess.control(surface = "direct"))
  
  grid_sed <-
    sed_lines %>%
    modelr::data_grid(
      x = seq(1, 15, 0.1),
    ) %>%
    modelr::spread_predictions(model_sed, model_sed_up, model_sed_low) %>%
    dplyr::rename(mid = model_sed, up = model_sed_up, low = model_sed_low)
  
# Getting data for plotting patient's result  
  
  score_sed <- data.frame(x = score / 60) %>% 
    modelr::add_predictions(model_sed)
  
# Creating figure
if (language == "en") {  
  g_sed <-
    ggplot() +
    geom_ribbon(data = grid_sed, aes(x = x, y = up, ymin = low, ymax = up), fill = "grey95") +
    geom_ribbon(data = grid_sed %>% dplyr::filter(x < 5), 
                aes(x = x, y = up, ymin = rep(0, 40), ymax = up), fill = "grey95") +
    geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_sed, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_sed, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 9.5), linetype = "dotted") +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, 2)) +
    coord_cartesian(xlim = c(1, 15), ylim = c(0.35, 5), expand = FALSE) +
    labs(title = "Daily SED hours", x = "", y = NULL) +
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
    annotate("text", 
             label = "Threshold above \nwhich risk \nis significant",
             x = 10.3, 
             y = 0.55, 
             hjust = 0,
             size = 5,
             colour = "black", 
             fontface = "italic") +
    annotate(geom = "curve", 
             x = 10.2, 
             y = 0.76, 
             xend = 9.5, 
             yend = 0.71, 
             curvature = .5, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "Ref: Ekelund et al. BMJ 2019, l4570", hjust = 0, x = 1.2, y = 0.387)
  
  return(g_sed)
  
  }
  
if (language == "fr") {  
  
  g_sed <-
    ggplot() +
    geom_ribbon(data = grid_sed, aes(x = x, y = up, ymin = low, ymax = up), fill = "grey95") +
    geom_ribbon(data = grid_sed %>% dplyr::filter(x < 5), 
                aes(x = x, y = up, ymin = rep(0, 40), ymax = up), fill = "grey95") +
    geom_line(data = grid_sed, aes (x = x, y = mid), size = 1, colour = "#3366FF") +
    geom_line(data = grid_sed, aes (x = x, y = up), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_line(data = grid_sed, aes (x = x, y = low), size = 1, colour = "#3366FF", linetype = "dashed") +
    geom_vline(aes(xintercept = 9.5), linetype = "dotted") +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 1) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 4, shape = 16) +
    geom_point(data = score_sed, aes(x = x, y = pred), color = "red", size = 7, shape = 3) +
    scale_y_continuous(trans = scales::log2_trans()) +
    scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, 2)) +
    coord_cartesian(xlim = c(1, 15), ylim = c(0.35, 5), expand = FALSE) +
    labs(title = "Heures SED journali\u00e8res", x = "", y = NULL) +
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
    annotate("text", 
             label = "Seuil au-dessus \nduquel le risque \nest significatif",
             x = 10.3, 
             y = 0.55, 
             hjust = 0,
             size = 5, 
             colour = "black", 
             fontface = "italic") +
    annotate(geom = "curve", 
             x = 10.2, 
             y = 0.76, 
             xend = 9.5, 
             yend = 0.71, 
             curvature = .5, arrow = arrow(length = unit(2, "mm")),
             colour = "black") +
    annotate("text", label = "R\u00e9f: Ekelund et al. BMJ 2019, l4570", hjust = 0, x = 1.2, y = 0.387)
  
  return(g_sed)
  
  }
}

