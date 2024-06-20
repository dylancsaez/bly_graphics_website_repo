#This script helps to set the y_axis nicely...
#More description to come, but that's about it...

set_y_axis <- function(ymin, ymax, y_delta){
  n_breaks <- (ymax - ymin) / y_delta + 1
  scale_y_continuous(breaks = seq(ymin, ymax, y_delta),
                     labels = rep("", n_breaks),
                     limits = c(ymin, ymax),
                    expand = c(0,0),
                    sec.axis = dup_axis(labels = seq(ymin, ymax, y_delta)))
}




