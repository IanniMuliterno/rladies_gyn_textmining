annotate <- ggplot2::annotate

theme_pessoal <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 19.6), 
                      plot.caption = element_text(color = "gray65", size = 12.4), legend.text = element_text(size = 19.6, colour = "gray34"), 
                      axis.title = element_text(size = 19.6, face = "bold", color = "gray25"), legend.title = element_text(size = 20.1, face = "bold", colour = "gray34"), 
                      axis.line = element_line(size = 0.5), plot.title = element_text(size = 28.1, face = "bold", colour = "gray30"), 
                      panel.grid.major = element_line(colour = "gray80", size = 0.15), plot.subtitle = element_text(size = 21.8, face = "bold", colour = "gray34"),
                      strip.text = element_text(size = 18.9, face = "bold"), panel.grid.minor = element_line(size = 0))