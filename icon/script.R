# Loading package
library(ggplot2)
library(magrittr)

# Create icon
df_raw <- 
  read.gt3x::read.gt3x("icon/raw.gt3x") %>%
  as.data.frame() %>%
  tidyr::separate("time", c("Date", "Time"), sep = " ") %>%
  dplyr::filter(Date == "2020-10-01" & Time >= "15:56:00" & Time <= "16:30:00") %>%
  dplyr::mutate(Time = hms::as_hms(Time)) 

g <-
  ggplot(data = df_raw, aes(x = Time, y = X, color = as.numeric(Time))) +
  geom_line(size = 0.1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0.2,0.2)) +
  labs(x=NULL, y=NULL, title=NULL) +
  coord_cartesian(ylim = c(-3.54, 1.795), expand = FALSE) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border=element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill=rgb(250, 250, 250, maxColorValue = 255), 
                                        color = rgb(250, 250, 250, maxColorValue = 255)),
        plot.background = element_rect(fill=rgb(250, 250, 250, maxColorValue = 255), 
                                        color = rgb(250, 250, 250, maxColorValue = 255))
  )
g
ggsave("icon/image.png", height = 1080, width = 2825, units = "px")
