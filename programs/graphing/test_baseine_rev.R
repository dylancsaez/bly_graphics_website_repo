#This script is to test the source(theme_ybl_style.R)
#We will use baseline_rev.csv from the tcja_ext FRBUS output
#Author: Dylan Saez
#Date: February 26, 2024
#---------------------------------------------------------------------------
#Installing our tools....
rm(list = ls()); cat("\014")
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")

#Check out colors..
palette_check(nice_rainbow_colors, plot = TRUE)
#actually...pretty good!
#---------------------------------------------------------------------------
#Bring in our data...
DATA <- read.csv("/gpfs/gibbs/project/sarin/shared/model_data/FRBUS/tcja_ext/2024022211/baseline/baseline_rev.csv")
#Okay so in this dataset we have
#X (year)
# TPN
# TCIN
# total_dynamic
# iit_rev
# corp_rev
# total_conventional

create_val_line <- function(charttype, chart_title, short_chart_title, subtitle_freq_label, subtitle_title,caption_title, ymin = "use_default", ymax = "use_default", ybreak = "use_default", legend_xpos = 0.5, legend_ypos = 0.8) {
  #------
  #Pulling data
  #-----
  if(charttype == "TPN"){
    data_cols <- paste0(c("TPN"))
  } else if(charttype == "TCIN"){
    data_cols <- paste0(c("TCIN"))
  } else if(charttype == "total_dynamic"){
    data_cols <- paste0(c("total_dynamic"))
  } else if(charttype == "iit_rev"){
    data_cols <- paste0(c("iit_rev"))
  } else if(charttype == "corp_rev"){
    data_cols <- paste0(c("corp_rev"))
  } else if(charttype == "total_conventional") {
    data_cols <- paste0(c("total_conventional"))
  } else {
    stop("Chart type must be specified.")
  }
  
  #Test
  # charttype <- "total_conventional"
  # data_cols <- "total_conventional"
  DATA <- DATA %>%
    select(X, data_cols)
  
  setnames(DATA, new = c("date", "data"))
  DATA <- DATA %>%
    filter(date != is.na(date))
  
  line_colors <- scale_color_manual(values = c("data" = "black"))
  
  DATA <- setDT(melt(DATA, id.vars = "date"))
  DATA$date <- lubridate::ymd(DATA$date, truncated = 2L)
  #---------
  #Charting
  #---------
  
  chart <- ggplot(DATA) +
    geom_line(aes(x = date, y = value),color = "black", linewidth = 1) +
    theme_bly_style() +
    theme(legend.position = c(.5, .8),
          legend.key.width = unit(0.8, "cm"),
          legend.key.height = unit(0.32, "cm"),
          axis.text.x.bottom = element_text(size = 6),
          axis.text.y.left = element_blank(),
          axis.title.y.left = element_blank(),
          axis.title.x.bottom = element_blank(),
          plot.caption = element_text(hjust = 0),
          plot.subtitle = element_text(hjust = c(0,1), vjust = 1.25)) +
    #Let's define some axes...
    scale_y_continuous(
      expand = c(0,0),
      limits = c(ymin, ymax),
      breaks = seq(ymin, ymax, ybreak),
      position = "right",
      sec.axis = dup_axis()) +
    scale_x_date(date_breaks = "12 months",
                 limits = c(as.Date("2023-01-01"), as.Date("2054-01-01")),
                 expand = c(0.02, 0),
                 date_labels = "%Y") +
    #Defining titles and notes
    xlab(NULL) +
    ylab(NULL) +
    labs(
      title = chart_title,
      subtitle = c(subtitle_freq_label, subtitle_title),
      caption = caption_title
    )
  cowplot <- cowplot::plot_grid(plot, NULL,
                                nrow = 1, ncol = 2,
                                rel_widths = c(0.85, 0.15))
  #Formatting x-axis dates
  chart <- chart %>%
    customXAxis(
      center = T, skip = 1, rmlasttick = F, rmfirsttick = F,
      minorticks = 3) %>% ggdraw()
  
  return(chart)

}

create_val_heat <- function(charttype,title = NULL, include_legend = F){
  #------
  #Pulling data
  #-----
  if(charttype == "TPN"){
    data_cols <- paste0(c("TPN"))
  } else if(charttype == "TCIN"){
    data_cols <- paste0(c("TCIN"))
  } else if(charttype == "total_dynamic"){
    data_cols <- paste0(c("total_dynamic"))
  } else if(charttype == "corp_rev"){
    data_cols <- paste0(c("corp_rev"))
  } else if(charttype == "iit_rev"){
    data_cols <- paste0(c("iit_rev"))
  } else if(charttype == "total_conventional") {
    data_cols <- paste0(c("total_conventional"))
  } else {
    stop("Chart type must be specified.")
  }
  
  #Test
  # charttype <- "total_conventional"
  # data_cols <- "total_conventional"
  DATA <- DATA %>%
    select(X, data_cols)
  
  DATA <- as.data.table(DATA)
  setnames(DATA, new = c("date", "data"))
  DATA <- DATA %>%
    filter(date != is.na(date))
  
  DATA[,data := 100 * ecdf(data)(data)]
  #------------------
  # Generating Labels
  #------------------
  latest_year <- max(DATA[!is.na(data)]$date, na.rm = T)
  latest_val <- DATA[date == latest_year]$data
  latest_date_sentence <- paste0("Data as of ", gsub(" ", ":", latest_year), ".")
  latest_value_sentence <- paste0("Lastest percentile: ", format(round(latest_val, 1), nsmall = 1), "%.")
  latest_label <- ggdraw() + draw_label(paste0(latest_value_sentence, " ", latest_date_sentence), size = 6, hjust = 0)
  label <- cowplot::plot_grid(latest_label, nrow = 1, ncol = 1)
  DATA <- subset(DATA, select = c(date, data))
  
  setnames(DATA, new = c("date", "value"))
  strip_theme <- theme(strip.text.x = element_text(hjust = 0))
  
  #Melting Data
  #Not needed here....
  xend <- max(DATA$date) + 1
  DATA$date <- lubridate::ymd(DATA$date, truncated = 2L)
  
  plot <- ggplot(DATA) +
    geom_tile(aes(x = date, fill = value, y = "")) +
    scale_fill_gradientn(name = "Percentile Index", colors = nice_rainbow_colors, na.value = na_color,
                         limits = c(0,100), breaks = c(seq(0,100, length = 6))) +
    theme_minimal()+
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 10, family = "Mallory", color = "black"),
          legend.background = element_blank(),
          plot.title = NULL,
          plot.subtitle = NULL,
          axis.title.x.bottom = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x.bottom = element_blank()) +
    scale_x_date(date_breaks = "2 year",
                 limits = c(as.Date("2023-01-01"), as.Date("2054-01-01")),
                 expand = c(0.02, 0),
                 date_labels = "%Y") +
    strip_theme +
    labs(x = NULL, y = NULL, caption = NULL, title = NULL)
  
  if(include_legend == F){
    plot <- plot + theme(legend.position = "none")
    
    #Adding footnote to the bar
    cowplot <- cowplot::plot_grid(plot, label, nrow = 1, ncol = 2,
                                  rel_widths = c(0.85, 0.15))
    
    total_row_label <- plot_grid(label, NULL,
                                 nrow = 2,
                                 ncol = 1,
                                 rel_heights = c(0.35, 0.65),
                                 align = "hv")
    total_row_plot <- plot_grid(plot, NULL,
                                nrow = 1, ncol = 2,
                                rel_widths = c(.965, .035),
                                align = "hv")
    cowplot <- cowplot::plot_grid(total_row_plot,
                                  total_row_label,
                                  nrow = 2, ncol = 1,
                                  rel_heights = c(0.35, 0.65))
    return(cowplot)
    }else{
      plot <- plot + theme(legend.position = "bottom")
      return(plot)
    }
}
#---------------------------------------------Charting-----------------------------------#
full_name = "Header"
#----------------------/
charttype_select = "TPN"
#----------------------/
tpn_chart <- create_val_line(charttype = charttype_select, chart_title = "TPN",subtitle_freq_label = "Frequency Label", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 10000, ybreak = 2500)
grid.draw(tpn_chart)
tpn_heat <- create_val_heat(charttype = charttype_select, title = NULL)
grid.draw(tpn_heat)
#----------------------/
charttype_select = "TCIN"
#----------------------/
tcin_chart <- create_val_line(charttype = charttype_select, chart_title = "TCIN", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 1000, ybreak = 250)
grid.draw(tcin_chart)
tcin_heat <- create_val_heat(charttype = charttype_select, title = NULL)
grid.draw(tcin_heat)
#----------------------/
charttype_select = "total_dynamic"
#----------------------/
total_dynamic_chart <- create_val_line(charttype = charttype_select, chart_title = "Total Dynamic", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 10000, ybreak = 2500)
grid.draw(total_dynamic_chart)
total_dynamic_heat <- create_val_heat(charttype = charttype_select, title = NULL)
grid.draw(total_dynamic_heat)
#----------------------/
charttype_select = "iit_rev"
#----------------------/
iit_rev_chart <- create_val_line(charttype = charttype_select, chart_title = "IIT Rev.", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 10000, ybreak = 2500)
grid.draw(iit_rev_chart)
iit_rev_heat <- create_val_heat(charttype = charttype_select, title = NULL)
grid.draw(iit_rev_heat)
#----------------------/
charttype_select = "corp_rev"
#----------------------/
corp_rev_chart <- create_val_line(charttype = charttype_select, chart_title = "Corp. Rev.", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 1000, ybreak = 250)
grid.draw(corp_rev_chart)
corp_rev_heat <- create_val_heat(charttype = charttype_select, title = NULL)
#----------------------/
charttype_select = "total_conventional"
#----------------------/
total_conventional_chart <- create_val_line(charttype = charttype_select, chart_title = "Total Conventional", subtitle_title = "Add Unit", caption_title = "Source: Placeholder", ymin = 0, ymax = 10000, ybreak = 2500)
grid.draw(total_conventional_chart)
total_convetional_heat <- create_val_heat(charttype = charttype_select, title = NULL)
grid.draw(total_convetional_heat)

legend_heat <- cowplot::get_legend(create_val_heat(charttype = charttype_select, title = NULL, include_legend = T))
#----PDF Export
#First Column of Page One
#Creating a one-pager from six charts
title <- ggdraw() + draw_label(full_name, fontface = 'bold', size = 16, fontfamily = 'Helvetica', x = .5, hjust = .5)
heat_row_p1_1 <- plot_grid(NULL, tpn_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))
heat_row_p1_2 <- plot_grid(NULL, tcin_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))
heat_row_p1_3 <- plot_grid(NULL, total_dynamic_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))

p1 <- cowplot::plot_grid(tpn_chart, heat_row_p1_1,
                         tcin_chart, heat_row_p1_2,
                         total_dynamic_chart, heat_row_p1_3,
                         legend_heat,
                         ncol = 1,
                         rel_heights = c(rep(c(1.75, .6),3),.05))
p1 <- plot_grid(date, title, p1, NULL, ncol = 1,
                rel_heights = c(0.02, .06, 1, 0.02))
grid.draw(p1)

#Second Column of Page One
title <- ggdraw() + draw_label(full_name, fontface = 'bold', size = 16, fontfamily = 'Helvetica', x = .5, hjust = .5)
heat_row_p1_1 <- plot_grid(NULL,  iit_rev_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))
heat_row_p1_2 <- plot_grid(NULL, corp_rev_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))
heat_row_p1_3 <- plot_grid(NULL, total_convetional_heat, NULL, ncol = 3, nrow = 1, align = "hv", rel_widths = c(.02, .98, .0265))

p2 <- cowplot::plot_grid(iit_rev_chart, heat_row_p1_1,
                         corp_rev_chart, heat_row_p1_2,
                         total_conventional_chart, heat_row_p1_3,
                         legend_heat,
                         ncol = 1,
                         rel_heights = c(rep(c(1.75, .6),3),.05))
p2 <- plot_grid(date, title, p2, NULL, ncol = 1,
                rel_heights = c(0.02, .06, 1, 0.02))
grid.draw(p2)


#----------------
p4 <- plot_grid(p1, p2, ncol = 2,
                align = "hv")
#Save
pdf(paste0("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/output/TEST.pdf"), height = 11, width = 8.5, paper = "letter")
p4
dev.off()


