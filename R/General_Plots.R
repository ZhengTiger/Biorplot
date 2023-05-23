
Bior_LinePlot <- function(
    data, line.size=1.5, labs.x='', labs.y='', title='',
                      text.size=20, title.size=30, legend.position='right',
                      col=category20, vline=NULL, theme=theme_minimal()){
  p <- ggplot() +
    geom_line(aes(x=data[,1], y=data[,2], color=data[,3]), size=line.size) +
    theme +
    labs(x=labs.x,y=labs.y,title=title) +
    theme(text=element_text(size=text.size),
          plot.title=element_text(size=title.size,hjust=0.5),
          legend.title=element_blank(), legend.position=legend.position) +
    scale_color_manual(values = col) +
    geom_vline(aes(xintercept= vline),colour="#9192ab",linetype="dashed",size = 1.5)
  return(p)
}










