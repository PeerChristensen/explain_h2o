


#BBC605 # yellow
##323132 # grey

theme_h2o_dark <- function () { 
  theme_bw(base_size=14, base_family="Roboto Condensed") %+replace% 
    theme(
      axis.text = element_text(size = rel(1),colour="white"), 
      axis.title.x = element_text(size = rel(1.4), colour = "white",
                                margin = margin(t = .7,b=.7, unit = "cm"),hjust=.95),
      axis.title.y = element_text(size = rel(1.4), colour = "white",
                                  margin = margin(l = .7,r=.7, unit = "cm"),angle=90,hjust=.95),
      axis.ticks = element_line(colour = "white",size = 0.1), 
      legend.key = element_rect(colour = "white"), 
      plot.background = element_rect(fill = "black"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      panel.background = element_rect(fill = "black", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "#323132"), 
      panel.grid.major = element_line(colour = "white", size = 0.03), 
      panel.grid.minor = element_line(colour = "white", size = 0.03), 
      strip.background = element_rect(fill = "#323132", colour = "#323132", 
                                      size = 0.2,),
      strip.text = element_text(size = rel(1.4),colour = "white", margin = margin(t = .25,b=.25, unit = "cm"))
    )
}
