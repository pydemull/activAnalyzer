#' Create a figure showing the mean daily Physical Activity Level (PAL)
#' 
#' The function generates a figure showing the daily mean of PAL in correspondence with the FAO 
#'     (2004; http://www.fao.org/3/y5686e/y5686e07.htm#bm07.3) categories. 
#'
#' @param score A numeric value for mean daily PAL.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' create_fig_pal(score = 1.8)
#'
create_fig_pal <- function(score, language = c("en", "fr", "de")) {
  
language <- match.arg(language)

# Data for figure
table_pal <-
  tibble::tibble(
    x = c(0, 0.25, 0.5, 0.75, 1),
    label = as.factor(c(
      "Undefined", 
      "Sedentary or light activity lifestyle", 
      "Active or moderately active lifestyle", 
      "Vigorous or vigorously active lifestyle",
      "Difficult to maintain over a long period of time"
    )),
    start = c(1.1, 1.40, 1.70, 2.00, 2.40),
    end = c(1.40, 1.70, 2.00, 2.40, 2.8)
  ) %>%
  dplyr::mutate(label = forcats::fct_relevel(label,  "Undefined",
                                             "Sedentary or light activity lifestyle", 
                                             "Active or moderately active lifestyle", 
                                             "Vigorous or vigorously active lifestyle",
                                             "Difficult to maintain over a long period of time")) 

# Creating figure
if (language == "en") {

  g_pal <-
    ggplot(data = table_pal, aes(x = x, y = start)) +
    geom_rect(aes(ymin = start,  ymax = end, fill = label), xmin = -Inf, xmax = Inf, alpha = 0.5) +
    geom_vline(aes(xintercept = 0.51), linewidth = 0.4, color = "grey30") +
    geom_vline(aes(xintercept = 0.25), linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_segment(x = 0, xend = 0.51, y = score, yend = score, linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_point(aes(x = 0.25, y = score, color = "Patient's result (daily mean)"), size = 7, shape = 1) +
    geom_point(aes(x = 0.25, y = score, color = "Patient's result (daily mean)"), size = 4, shape = 16) +
    geom_point(aes(x = 0.25, y = score, color = "Patient's result (daily mean)"), size = 7, shape = 3) +
    scale_y_continuous(labels = as.character(format(round(c(1.1, 1.4, 1.7, 2.0, 2.4, 2.8), 2), nsmall = 1)), 
                       breaks = c(1.10, 1.40, 1.70, 2.00, 2.40, 2.80)) +
    scale_fill_manual(values = c("lightgoldenrodyellow", "lightgoldenrod1", "yellow", "gold1", "gold2")) +
    scale_color_manual(values = c("red")) +
    theme_bw() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, 
         fill = "FAO/WHO/UNU categories", 
         color = "") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.position = c(0.5, 1.4),
          legend.title = element_text(face = "bold" , size = 10),
          legend.text = element_text(face = "bold", size = 17),
          legend.background = element_rect(fill = "beige"),
          legend.key = element_rect(fill = "beige", linewidth = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    guides(fill= "none") +
    ggtitle("Daily PAL") +
    annotate("text", label = "Sedentary \nor lightly active", x = 0.75, y = 1.55, size = 5) +
    annotate("text", label = "Moderately \nactive", x = 0.75, y = 1.85, size = 5) +
    annotate("text", label = "Vigorously \nactive", x = 0.75, y = 2.2, size = 5) +
    annotate("text", label = "Difficult to maintain \nover a long period of time", x = 0.75, y = 2.6, size = 5) +
    annotate("text", label = "Ref: FAO 2004 | http://www.fao.org/3/y5686e/y5686e07.htm#bm07.3", hjust = 1, x = 0.1, y = 2.79)
  
  return(g_pal)
  
}

if (language == "fr") {
  
  g_pal <-
    ggplot(data = table_pal, aes(x = x, y = start)) +
    geom_rect(aes(ymin = start,  ymax = end, fill = label), xmin = -Inf, xmax = Inf, alpha = 0.5) +
    geom_vline(aes(xintercept = 0.51), linewidth = 0.4, color = "grey30") +
    geom_vline(aes(xintercept = 0.25), linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_segment(x = 0, xend = 0.51, y = score, yend = score, linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_point(aes(x = 0.25, y = score, color = "R\u00e9sultat du patient (moyenne journali\u00e8re)"), size = 7, shape = 1) +
    geom_point(aes(x = 0.25, y = score, color = "R\u00e9sultat du patient (moyenne journali\u00e8re)"), size = 4, shape = 16) +
    geom_point(aes(x = 0.25, y = score, color = "R\u00e9sultat du patient (moyenne journali\u00e8re)"), size = 7, shape = 3) +
    scale_y_continuous(labels = as.character(format(round(c(1.1, 1.4, 1.7, 2.0, 2.4, 2.8), 2), nsmall = 1)), 
                       breaks = c(1.10, 1.40, 1.70, 2.00, 2.40, 2.80)) +
    scale_fill_manual(values = c("lightgoldenrodyellow", "lightgoldenrod1", "yellow", "gold1", "gold2")) +
    scale_color_manual(values = c("red")) +
    theme_bw() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, 
         fill = "FAO/WHO/UNU categories", 
         color = "") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.position = c(0.5, 1.4),
          legend.title = element_text(face = "bold" , size = 10),
          legend.text = element_text(face = "bold", size = 17),
          legend.background = element_rect(fill = "beige"),
          legend.key = element_rect(fill = "beige", linewidth = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    guides(fill= "none") +
    ggtitle("NAP journalier") +
    annotate("text", label = "S\u00e9dentaire \nou l\u00e9g\u00e8rement actif", x = 0.75, y = 1.55, size = 5) +
    annotate("text", label = "Mod\u00e9r\u00e9ment \nactif", x = 0.75, y = 1.85, size = 5) +
    annotate("text", label = "Vigoureusement \nactif", x = 0.75, y = 2.2, size = 5) +
    annotate("text", label = "Difficile \u00e0 maintenir \nsur une longue p\u00e9riode de temps", x = 0.75, y = 2.6, size = 5) +
    annotate("text", label = "R\u00e9f: FAO 2004 | http://www.fao.org/3/y5686e/y5686e07.htm#bm07.3", hjust = 1, x = 0.1, y = 2.79)
  
  return(g_pal)
  
}


if (language == "de") {
  
  g_pal <-
    ggplot(data = table_pal, aes(x = x, y = start)) +
    geom_rect(aes(ymin = start,  ymax = end, fill = label), xmin = -Inf, xmax = Inf, alpha = 0.5) +
    geom_vline(aes(xintercept = 0.51), linewidth = 0.4, color = "grey30") +
    geom_vline(aes(xintercept = 0.25), linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_segment(x = 0, xend = 0.51, y = score, yend = score, linewidth = 0.5, color = "grey30", linetype ="dotted") +
    geom_point(aes(x = 0.25, y = score, color = "Ergebnisse der Bewegungsmessung (Tagesmittelwert)"), size = 7, shape = 1) +
    geom_point(aes(x = 0.25, y = score, color = "Ergebnisse der Bewegungsmessung (Tagesmittelwert)"), size = 4, shape = 16) +
    geom_point(aes(x = 0.25, y = score, color = "Ergebnisse der Bewegungsmessung (Tagesmittelwert)"), size = 7, shape = 3) +
    scale_y_continuous(labels = as.character(format(round(c(1.1, 1.4, 1.7, 2.0, 2.4, 2.8), 2), nsmall = 1)), 
                       breaks = c(1.10, 1.40, 1.70, 2.00, 2.40, 2.80)) +
    scale_fill_manual(values = c("lightgoldenrodyellow", "lightgoldenrod1", "yellow", "gold1", "gold2")) +
    scale_color_manual(values = c("red")) +
    theme_bw() +
    coord_flip(expand = FALSE) +
    labs(x = NULL, y = NULL, 
         fill = "FAO/WHO/UNU-Kategorien", 
         color = "") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.position = c(0.5, 1.4),
          legend.title = element_text(face = "bold" , size = 10),
          legend.text = element_text(face = "bold", size = 17),
          legend.background = element_rect(fill = "beige"),
          legend.key = element_rect(fill = "beige", linewidth = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    guides(fill= "none") +
    ggtitle("Tägliches PAL") +
    annotate("text", label = "Sitzender oder leicht \naktiver Lebensstil", x = 0.75, y = 1.55, size = 5) +
    annotate("text", label = "mäßig aktiver \nLebensstil", x = 0.75, y = 1.85, size = 5) +
    annotate("text", label = "Stark aktiver \nLebensstil", x = 0.75, y = 2.2, size = 5) +
    annotate("text", label = "Schwierig, über einen langen Zeitraum \nhinweg aufrecht zu erhalten", x = 0.75, y = 2.6, size = 5) +
    annotate("text", label = "Ref: FAO 2004 | http://www.fao.org/3/y5686e/y5686e07.htm#bm07.3", hjust = 1, x = 0.1, y = 2.79)
  
  return(g_pal)
  
}

}
