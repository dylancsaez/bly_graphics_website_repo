set_y_axis_left <- function(ymin, ymax, y_delta){
  n_breaks <- (ymax - ymin) / y_delta + 1
  scale_y_continuous(limits = c(ymin, ymax),
                     breaks = seq(ymin, ymax, y_delta),
                     labels = rep("", n_breaks),
                     expand = c(0,0),
                     sec.axis = dup_axis())
}
