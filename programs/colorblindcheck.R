# We want to bring in a package that helps a grapher identify colors accessible for colorblind users.
# Directly from the colorblind check page:
#Deciding if a color palette is a colorblind friendly is a hard task. This cannot be done in an entirely automatic fashion,
#as the decisions needs to be confirmed by visual judgments.
#The goal of colorblindcheck is to provide tools to decide if the selected color palette is colorblind friendly.

#palette_dist() - Calculation of the distances between the colors in the input palette and between the colors in sinulations
#of the color vision deficiences:
#deutranopia: red-green
#protanopia: blindness to red
#tritanopia: blue-green and purple-red-yellow-pink

#palette_plot(): Plotting of the original input palette and simulations of color vision deficiencies
#palette_check() - Creating summary statistics comparing the original input palette and simulations of color vision deficiencies

#install.packages("colorblindcheck") #This will be added directly to the bly_packages
library(colorblindcheck)

rainbow_pal = rainbow(n=7)
rainbow_pal #Results in the hex colors of the rainbow with seven choices...

palette_check(rainbow_pal, plot = TRUE)

#name: original input color palette
#n: number of colors
#tolerance: minimal value of the acceptable difference between the colors to distinguish between them
#ncp: number of color pairs
#ndcp: number of differential color pairs (color pairs with distances above the tolerance value)
#min_dist: minimal distance between colors
#mean_dist: average distance between colors
#max_dist: maximal distance between colors


#Let's try Budget Lab at Yale colors!
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")

bly_palette = bly_colors
bly_palette


palette_check(bly_palette, plot = TRUE)

#....not GREAT!!!!!

#However, I assume that we won't be graphing ten colors, including black on a chart...
#If we do want to graph with these colors, we choose those with larger differences..such as differentiate by 3

bly_palette_to_graph_with = c("#101F5B", "#63AAFF", "#978D85", "#000000")
#To be clear, the black cannot be just solid line in the graph - it will not be distinguishable form the dark blue...
#Thus, we would make this a dashed line...
palette_check(bly_palette_to_graph_with, plot = TRUE)

#This would be just to gauge how the color swill come out to different CVDs.
