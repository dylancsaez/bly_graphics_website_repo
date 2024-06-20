#--------------------------------------
# In this script, I create theme_ybl_style - which is the standard for any production charts
# theme_ybl_style() can be added to charts to eliminate redundancy for any themes
#--------------------------------------
theme_bly_style <- function() {
  small_margins <- ggplot2::theme(plot.margin = unit(c(2,2,2,3.25), 'mm'))
  
  theme_ybl_style <- ggplot2::theme_classic() +
    
    theme(
      axis.ticks.length = unit(-0.2, "cm"),
      panel.border = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(0.4, 0.175, 0, 0.175), "in"),
      
      axis.text = ggplot2::element_text(size = 8, family = "Mallory"),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 3), color = "black", size = 8, family = "Mallory"),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 3), color = "black", size = 8, family = "Mallory"),
      axis.text.x.bottom = ggplot2::element_text(size = 8 , family = "Mallory"),
      axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 3), color = "black", size = 8, hjust = 1, family = "Mallory"),
      axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 3), color = "black", size = 8, hjust = 1, family = "Mallory"),
      axis.title = ggplot2::element_text(size = 8, face = "plain", family = "Mallory"),
      
      plot.title = ggplot2::element_text(hjust = 0, size = 10, face = "bold",family = "Mallory", color = "black"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0,family = "Mallory", color = "black"),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0,family = "Mallory", color = "black", margin = margin(0,0,0,0,"pt")),
      
      
      axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.line = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.line.x = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.line.y = ggplot2::element_line(linewidth = 0.3, color = "black"),
      axis.ticks.y = ggplot2::element_line(linewidth = 0.3, color = "black"),
      
      legend.text = ggplot2::element_text(size = 5, family = "Mallory"),
      legend.key.size = ggplot2::unit(0.1, 'cm'),
      legend.spacing.x = unit(0.01, 'cm'),
      legend.spacing.y = unit(-0.07, "cm"),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank() 
      
    )
  return(theme_ybl_style)
}