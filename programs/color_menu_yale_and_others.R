#----This R script serves as the color menu for charts, etc.

#Color Scheme......

nice_rainbow_colors <- c(
  "#c5e4f9",
  "#9bc2e6",
  "#91d598",
  "#d2e060",
  "#f8e43f",
  "#fad232",
  "#eb9c15",
  "#e66b0a",
  "#e00505",
  "#ab0000"
)
na_color <- "gray75"
rainbow_invert <- rev(nice_rainbow_colors)

bly_colors <- c(
  "#101F5B", #Budget Lab Blue #Links, Labels, Layout Backgrounds
  "#1B3499", #Egyptian Blue #Layout backgrounds
  "#286dc0", #Yale medium blue #links on white backgrounds
  "#63AAFF", #Yale bright blue #Navigation, links on dark backgrounds
  "#D9EAFF", #Pale Blue #Layout backgrounds
  "#4A4A4A", #Quartz #Text
  "#978D85", #Pewter #Form elements
  "#DDDDDD", #Light Gray #Layout backgrounds
  "#F9F9F9", #Ghost #Layout backgrounds
  "#FFFFFF" #White #Text color, headlines, navigation active state
)

#Something to note: There is no black in this design
#If anything is labeled with "black" (#000000 or #222222), please change to #4A4A4A