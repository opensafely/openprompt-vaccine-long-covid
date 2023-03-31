# note: element_line should have option `linewidth` not `size` as of 3.4.1 but OS uses 3.3.2 so need to keep it as `size` for now and get annoying error messages locally
theme_ali <- function (base_size = 11, base_family = ""){
  half_line <- base_size/2
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = rel(1), hjust=0.5), 
        panel.background = element_rect(fill = "white",
                                        colour = "gray70",
                                        size = 0.5, linetype = "solid"),
        axis.line = element_line(size=rel(1), colour = alpha(1,0.8)),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.4)) ,
        panel.grid.minor.y = element_blank(), #element_line(linetype = 1, linewidth=.15, color=rgb(0,0,0,0.2)) ,
        panel.grid.minor.x = element_blank(), #element_line(linetype = 1, linewidth=.15, color=rgb(0,0,0,0.2)),
        axis.ticks = element_line(size=.1, color=rgb(0,0,0,0.4)),
        #line = element_line(colour = "gray70"),
        rect = element_rect(fill = "white"),
        legend.background = element_blank(), legend.key = element_blank(),
        strip.background = element_blank()
  )
}
