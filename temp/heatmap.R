library(RColorBrewer)
df <- df %>% rename(point = "...2")
ggplot() +
  geom_area(data = bind_rows(df %>% filter(lines %in% c("c7")),
                            data.frame(X = 90, Y = 900)), 
                            aes(X, Y), fill = "#FF6633") + 
  geom_area(data = bind_rows(data.frame(X = 90, Y = 900), 
                                    df %>% filter(lines %in% c("c6"))), aes(x = X, y = Y), fill = "#FF9933") + 
  geom_area(data = df %>% filter(lines %in% c("c5")), aes(x = X, y = Y), fill = "#FF9900") + 
  geom_area(data = df %>% filter(lines %in% c("c4")), aes(x = X, y = Y), fill = "#FFCC66") + 
  geom_area(data = df %>% filter(lines %in% c("c3")), aes(x = X, y = Y), fill = "#FFFF33") +
  geom_area(data = df %>% filter(lines %in% c("c2")), aes(x = X, y = Y), fill = "#CCFF66") +
  geom_area(data = df %>% filter(lines %in% c("c1")), aes(x = X, y = Y), fill = "#CCFF33") +
  coord_cartesian(xlim = c(0, 90), ylim = c(360, 840), expand = FALSE) +
  geom_line(data = df %>% filter(point %in% c("start", "end")), aes(x = X, y = Y, group = lines)) +
  geom_line(data = df %>% filter(lines %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8")), 
            aes(X, Y, group = lines), color = "red")


  
  geom_smooth(data = df %>% filter(lines %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8")), 
              aes(X, Y, group = lines), method = "lm", formula = y ~ log(x), se = F)
  