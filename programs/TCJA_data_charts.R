#Author: Dylan Saez
#Date: March 28, 2024
#---------------------------------------------------------------------------
#Installing our tools....
rm(list = ls()); cat("\014")
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")

reorder_1 <- c("Bottom quintile", "Second quintile", "Middle quintile", "Fourth quintile", "80% - 90%", "90% - 99%", "99% - 99.9%", "Top 0.1%")
reorder_age <- c("24 and under", "25 - 29","30 - 39","40 - 49","50 - 64","65+")
#---------------------------------------------------------------------------


stacked_rev_full <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/website_data.xlsx",sheet = "stacked_rev_full")


stacked_rev_full_left <- stacked_rev_full %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_full_dots <- stacked_rev_full %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_full_left <- merge(stacked_rev_full_left, stacked_rev_full_dots, by = "Year", all.x = TRUE)
stacked_rev_full_left <- stacked_rev_full_left %>% 
  filter(Provision != "Conventional revenue estimate" 
         & Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")
stacked_rev_full_dots <- stacked_rev_full_left %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")

# Stacked
stacked_rev_full_chart <- 
  ggplot(stacked_rev_full_left, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_full_left, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  # scale_y_continuous(sec.axis = dup_axis()) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Full Extension Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "USD Billions", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("#08306B" ,
                               "#08519C", "#2171B5",
                               "#4292C6","#6BAED6",
                               "steelblue2", "#9ECAE1",
                               "#C6DBEF",
                               "#DEEBF7", "#F7FBFF")) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-1000, 1000),
                     #breaks = c(-0.02, 0.00, 0.02),
                     expand = c(0,0))

stacked_rev_full_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_full_chart.png", width=1640, height=1800, res=300)
stacked_rev_full_chart
dev.off()
#----Share of GDP
stacked_rev_full_right <- stacked_rev_full %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_full_dots <- stacked_rev_full %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_full_right <- merge(stacked_rev_full_right, stacked_rev_full_dots, by = "Year", all.x = TRUE)
stacked_rev_full_right <- stacked_rev_full_right %>% 
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade") %>%
  filter(Provision != "Conventional revenue estimate")
stacked_rev_full_dots <- stacked_rev_full_right %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade")

stacked_rev_full_right$Year <-replace(stacked_rev_full_right$Year, stacked_rev_full_right$Year == "Budget window percent", "Budget window") 
# Stacked
stacked_rev_full_chart_gdp <- 
  ggplot(stacked_rev_full_right, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_full_right, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Full Extension Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "Percent of GDP", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("#08306B" ,
                               "#08519C", "#2171B5",
                               "#4292C6","#6BAED6",
                               "steelblue2", "#9ECAE1",
                               "#C6DBEF",
                               "#DEEBF7", "#F7FBFF")) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.text.y.left = element_text(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-0.02, 0.02),
                     #breaks = c(-0.02, 0.00, 0.02),
                     expand = c(0,0))

stacked_rev_full_chart_gdp

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_full_chart_gdp.png", width=1640, height=1800, res=300)
stacked_rev_full_chart_gdp
dev.off()

#----------------------------------Stacked Rev Partial Charts

stacked_rev_partial <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "stacked rev partial")

colnames(stacked_rev_partial) <- c("Provision", "2025", "2026", "2027", "2028",
                                   "2029", "2030", "2031", "2032", "2033", "2034",
                                   "Budget window", "Budget window percent", "Second decade", "Third decade")


stacked_rev_partial_left <- stacked_rev_partial %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_partial_dots <- stacked_rev_partial %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_partial_left <- merge(stacked_rev_partial_left, stacked_rev_partial_dots, by = "Year", all.x = TRUE)
stacked_rev_partial_left <- stacked_rev_partial_left %>% 
  filter(Provision != "Conventional revenue estimate" 
         & Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")
stacked_rev_partial_dots <- stacked_rev_partial_left %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")
# Stacked
stacked_rev_partial_chart <- 
  ggplot(stacked_rev_partial_left, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_partial_left, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Partial Extension Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "USD Billions", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("goldenrod","#FEC44F", "#FEE391",
                               "#FFF7BC")) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.text.y.left = element_text(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-400, 400),
                     breaks = c(-400, -200, 0, 200, 400),
                     expand = c(0,0))
stacked_rev_partial_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_partial_chart.png", width=1640, height=1750, res=300)
stacked_rev_partial_chart
dev.off()

#----Share of GDP
stacked_rev_partial_right <- stacked_rev_partial %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_partial_dots <- stacked_rev_partial %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_partial_right <- merge(stacked_rev_partial_right, stacked_rev_partial_dots, by = "Year", all.x = TRUE)
stacked_rev_partial_right <- stacked_rev_partial_right %>% 
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade") %>%
  filter(Provision != "Conventional revenue estimate")
stacked_rev_partial_dots <- stacked_rev_partial_right %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade")

stacked_rev_partial_right$Year <-replace(stacked_rev_partial_right$Year, stacked_rev_partial_right$Year == "Budget window percent", "Budget window") 

# Stacked
stacked_rev_partial_chart_gdp <- 
  ggplot(stacked_rev_partial_right, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_partial_right, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Partial Extension Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "Percent of GDP", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("goldenrod","#FEC44F", "#FEE391",
                               "#FFF7BC")) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.text.y.left = element_text(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-0.01, 0.01),
                     #breaks = c(-0.01,-0.005, 0.00, 0.0025),
                     expand = c(0,0))

stacked_rev_partial_chart_gdp


png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_partial_chart_gdp.png", width=1640, height=1750, res=300)
stacked_rev_partial_chart_gdp
dev.off()
#--------------------------------------------------Stacked Revenue CS Charts
stacked_rev_cs <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "stacked_rev_cs")
colnames(stacked_rev_cs) <- c("Provision", "2025", "2026", "2027", "2028",
                              "2029", "2030", "2031", "2032", "2033", "2034",
                              "Budget window", "Budget window percent", "Second decade", "Third decade")

stacked_rev_cs_left <- stacked_rev_cs %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_cs_dots <- stacked_rev_cs %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_cs_left <- merge(stacked_rev_cs_left, stacked_rev_cs_dots, by = "Year", all.x = TRUE)
stacked_rev_cs_left <- stacked_rev_cs_left %>% 
  filter(Provision != "Conventional revenue estimate" 
         & Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")
stacked_rev_cs_dots <- stacked_rev_cs_left %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")
# Stacked
stacked_rev_cs_chart <- 
  ggplot(stacked_rev_cs_left, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_cs_left, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Clausing-Sarin Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "USD Billions", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("#67001F", 
                               "#980043", "#CE1256",
                               "#E7298A", "#DF65B0",
                               "#C994C7", "#D4B9DA",
                               "#E7E1EF", "#F7F4F9",
                               "#DADAEB", "#BCBDDC",
                               "#9E9AC8","#807DBA",
                               "mediumorchid1", "mediumorchid3",
                               "mediumorchid4", "#6A51A3",
                               "purple", "purple2",
                               "purple3",
                               "#3F007D","#54278F"
                               )) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.text.y.left = element_text(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "right") +
  guides(fill = guide_legend(ncol = 1, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-1500, 1500),
                     expand = c(0,0))

stacked_rev_cs_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_cs_chart.png", width=2200, height=1400, res=300)
stacked_rev_cs_chart
dev.off()

#----Share of GDP
stacked_rev_cs_right <- stacked_rev_cs %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_rev_cs_dots <- stacked_rev_cs %>%
  filter(Provision == "Conventional revenue estimate") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Conventional Revenue Estimate") %>%
  select(-c(Provision))

stacked_rev_cs_right <- merge(stacked_rev_cs_right, stacked_rev_cs_dots, by = "Year", all.x = TRUE)
stacked_rev_cs_right <- stacked_rev_cs_right %>% 
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade") %>%
  filter(Provision != "Conventional revenue estimate")
stacked_rev_cs_dots <- stacked_rev_cs_right %>% 
  select(c(Year, `Conventional Revenue Estimate`)) %>%
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade")

stacked_rev_cs_right$Year <-replace(stacked_rev_cs_right$Year, stacked_rev_cs_right$Year == "Budget window percent", "Budget window") 

# Stacked
stacked_rev_cs_chart_gdp <- 
  ggplot(stacked_rev_cs_right, mapping = aes(fill=Provision, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_rev_cs_right, mapping = aes(x = Year, y = `Conventional Revenue Estimate`, color = "Conventional Revenue Estimate")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Revenue Effects of\n the Clausing-Sarin Scenario by Provision, FY2025-2054",
       subtitle = "Billions of Dollars", y = "Percent of GDP", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("#67001F", 
                               "#980043", "#CE1256",
                               "#E7298A", "#DF65B0",
                               "#C994C7", "#D4B9DA",
                               "#E7E1EF", "#F7F4F9",
                               "#DADAEB", "#BCBDDC",
                               "#9E9AC8","#807DBA",
                               "mediumorchid1", "mediumorchid3",
                               "mediumorchid4", "#6A51A3",
                               "purple", "purple2",
                               "purple3",
                               "#3F007D","#54278F"
  )) + 
  scale_color_manual(labels = c("Conventional Revenue Estimate"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.text.y.left = element_text(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "right") +
  guides(fill = guide_legend(ncol = 1, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-0.03, 0.03),
                     breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
                     expand = c(0,0))

stacked_rev_cs_chart_gdp

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_rev_cs_chart_gdp.png", width=2200, height=1400, res=300)
stacked_rev_cs_chart_gdp
dev.off()
# 
# #--------------------------------------------------Partial Dynamic Offset
# partial_dynamic_offset <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "partial_dynamic_offset")
# 
# 
# #----Share of GDP
# partial_dynamic_offset_right <- partial_dynamic_offset %>%
#   pivot_longer(!Scenario, names_to = "Year", values_to = "value")
# 
# # Stacked
# partial_dynamic_offset_chart <- 
#   ggplot(partial_dynamic_offset_right, mapping = aes(fill=Scenario, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Partial Dynamic Offset", subtitle = "[PLEASE ADD SUBTITLE]") +
#   scale_fill_manual(values = c("hotpink2", "#6C2830","goldenrod")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-120, 120),
#                      breaks = c(-120, -80, -40, 0, 40, 80, 120),
#                      expand = c(0,0))
# partial_dynamic_offset_chart
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/partial_dynamic_offset_chart.png", width=2500, height=1500, res=300)
# partial_dynamic_offset_chart
# dev.off()
# 
# 
# #--------------------------------------------------Dynamic Offset
# dynamic_offset <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "dynamic_offset")
# 
# dynamic_offset_right <- dynamic_offset %>%
#   pivot_longer(!Scenario, names_to = "Year", values_to = "value")
# 
# # Stacked
# dynamic_offset_chart <- 
#   ggplot(dynamic_offset_right, mapping = aes(fill=Scenario, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Dynamic Offset", subtitle = "[PLEASE ADD SUBTITLE]") +
#   scale_fill_manual(values = c("hotpink2", "#6C2830","goldenrod")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-1000, 1000),
#                      breaks = c(-1000, -500, 0, 500, 1000),
#                      expand = c(0,0))
# 
# dynamic_offset_chart
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/dynamic_offset_chart.png", width=2500, height=1500, res=300)
# dynamic_offset_chart
# dev.off()
# 
# 
# #-----------------------Average Burden
# average_burden <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "average_burden")
# 
# average_burden_left <- average_burden %>%
#   pivot_longer(!`Income Group`, names_to = "Year", values_to = "value")
# 
# average_burden_dots <- average_burden %>%
#   filter(`Income Group` == "Average") %>%
#   pivot_longer(!`Income Group`, names_to = "Year", values_to = "Average") %>%
#   select(-c(`Income Group`))
# 
# average_burden_left <- merge(average_burden_left, average_burden_dots, by = "Year", all.x = TRUE)
# average_burden_left <- average_burden_left %>% 
#   filter(`Income Group` != "Average")
# 
# average_burden_left_burden <- average_burden_left %>% 
#   filter(Year == "Clausing-Sarin Burden" | Year == "Current Law Burden" |
#            Year == "Full Extension Burden" | Year == "Partial Extension Burden")
# 
# average_burden_left_difference <- average_burden_left %>% 
#   filter(Year == "Clausing-Sarin Difference" | Year == "Full Extension Difference" |
#            Year == "Partial Extension Difference")
# 
# # Stacked
# average_burden_chart_difference <- 
#   ggplot(average_burden_left_difference, mapping = aes(fill=`Income Group`, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(average_burden_left_difference, mapping = aes(x = Year, y = `Average`, color = "Average")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Average Difference", subtitle = "PLEASE ADD SUBTTILE") +
#   scale_fill_manual(values = c("#FFEFCA", "#EDA16A" ,
#                                "#C83741", "#6C283D",
#                                "goldenrod","darkgoldenrod",
#                                "darkorange", "darkorange3",
#                                "gray59", "gray48",
#                                "lemonchiffon", "lemonchiffon2",
#                                "salmon2", "salmon4",
#                                "tomato", "tomato3",
#                                "mediumorchid3", "mediumorchid4",
#                                "burlywood1", "burlywood4",
#                                "brown2", "brown4")) + 
#   scale_color_manual(labels = c("Average"), values = c("black")) +
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-18, 18),
#                      breaks = c(-18, -12, -6, 0, 6, 12, 18),
#                      expand = c(0,0))
# 
# average_burden_chart_difference
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/average_burden_chart_difference.png", width=3200, height=1650, res=300)
# average_burden_chart_difference
# dev.off()
# 
# 
# average_burden_chart_burden <- 
#   ggplot(average_burden_left_burden, mapping = aes(fill=`Income Group`, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(average_burden_left_burden, mapping = aes(x = Year, y = `Average`, color = "Average")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Average Burden", subtitle = "PLEASE ADD SUBTTILE") +
#   scale_fill_manual(values = c("#FFEFCA", "#EDA16A" ,
#                                "#C83741", "#6C283D",
#                                "goldenrod","darkgoldenrod",
#                                "darkorange", "darkorange3",
#                                "gray59", "gray48",
#                                "lemonchiffon", "lemonchiffon2",
#                                "salmon2", "salmon4",
#                                "tomato", "tomato3",
#                                "mediumorchid3", "mediumorchid4",
#                                "burlywood1", "burlywood4",
#                                "brown2", "brown4")) + 
#   scale_color_manual(labels = c("Average"), values = c("black")) +
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(0, 200),
#                      #breaks = c(-0.02, 0.00, 0.02),
#                      expand = c(0,0))
# 
# average_burden_chart_burden
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/average_burden_chart_burden.png", width=3200, height=1650, res=300)
# average_burden_chart_burden
# dev.off()

# #---------------------SHARE PREFILLED
# share_prefilled <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/TCJA_data.xlsx",sheet = "share_prefilled")
# 
# share_prefilled_left <- share_prefilled %>%
#   pivot_longer(!`Income Group`, names_to = "Year", values_to = "value")
# 
# share_prefilled_dots <- share_prefilled %>%
#   filter(`Income Group` == "Total") %>%
#   pivot_longer(!`Income Group`, names_to = "Year", values_to = "Total") %>%
#   select(-c(`Income Group`))
# 
# share_prefilled_left <- merge(share_prefilled_left, share_prefilled_dots, by = "Year", all.x = TRUE)
# share_prefilled_left <- share_prefilled_left %>% 
#   filter(`Income Group` != "Total")
# 
# # Stacked
# share_prefilled_chart_difference <- 
#   ggplot(share_prefilled_left, mapping = aes(fill=`Income Group`, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(share_prefilled_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
#   scale_y_continuous(sec.axis = dup_axis()) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Share Prefilled", subtitle = "PLEASE ADD SUBTTILE") +
#   scale_fill_manual(values = c("#FFEFCA", "#EDA16A" ,
#                                "#C83741", "#6C283D",
#                                "goldenrod","darkgoldenrod",
#                                "darkorange", "darkorange3",
#                                "gray59", "gray48",
#                                "lemonchiffon", "lemonchiffon2",
#                                "salmon2", "salmon4",
#                                "tomato", "tomato3",
#                                "mediumorchid3", "mediumorchid4",
#                                "burlywood1", "burlywood4",
#                                "brown2", "brown4")) + 
#   scale_color_manual(labels = c("Total"), values = c("black")) +
#   theme(axis.title.x.bottom = element_blank(),
#         axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   guides(fill = guide_legend(override.aes = list(shape = NA)))+
#   set_y_axis(ymin = 0, ymax = 2, y_delta = 0.5)
# 
# share_prefilled_chart_difference
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/share_prefilled_chart_difference.png", width=2400, height=1400, res=300)
# share_prefilled_chart_difference
# dev.off()


#----stacked_dist
stacked_dist_y_full <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_y_full")
stacked_dist_y_full_left <- stacked_dist_y_full %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_y_full_dots <- stacked_dist_y_full %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_y_full_left <- merge(stacked_dist_y_full_left, stacked_dist_y_full_dots, by = "Year", all.x = TRUE)
stacked_dist_y_full_left <- stacked_dist_y_full_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_y_full_chart <- 
  ggplot(stacked_dist_y_full_left, mapping = aes(fill=Provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bly_style() +
  geom_point(size = 4, stacked_dist_y_full_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Full Extension: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#08306B" ,
                               "#08519C", "#2171B5",
                               "#4292C6","#6BAED6",
                               "steelblue2", "#9ECAE1",
                               "#C6DBEF",
                               "#DEEBF7", "#F7FBFF")) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-5, 5),
                     breaks = c(-5, -2.5, 0, 2.5, 5),
                     expand = c(0,0))

stacked_dist_y_full_chart
png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_y_full_chart.png", width=1640, height=1800, res=300)
stacked_dist_y_full_chart
dev.off()

#----Stacked Y Partial
stacked_dist_y_partial <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_y_partial")

stacked_dist_y_partial_left <- stacked_dist_y_partial %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_y_partial_dots <- stacked_dist_y_partial %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_y_partial_left <- merge(stacked_dist_y_partial_left, stacked_dist_y_partial_dots, by = "Year", all.x = TRUE)
stacked_dist_y_partial_left <- stacked_dist_y_partial_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_y_partial_chart <- 
  ggplot(stacked_dist_y_partial_left, mapping = aes(fill=Provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_dist_y_partial_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Partial Extension: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#FEC44F", "#FEE391",
                               "#FFF7BC", "#FFFFE5")) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-4, 4),
                     breaks = c(-4, -2, 0, 2, 4),
                     expand = c(0,0))
stacked_dist_y_partial_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_y_partial_chart.png", width=1640, height=2000, res=300)
stacked_dist_y_partial_chart
dev.off()
#----------------------------------Stacked Dist Y CS
stacked_dist_y_cs <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_y_cs")

stacked_dist_y_cs_left <- stacked_dist_y_cs %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_y_cs_dots <- stacked_dist_y_cs %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_y_cs_left <- merge(stacked_dist_y_cs_left, stacked_dist_y_cs_dots, by = "Year", all.x = TRUE)
stacked_dist_y_cs_left <- stacked_dist_y_cs_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_y_cs_chart <- 
  ggplot(stacked_dist_y_cs_left, mapping = aes(fill=Provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_dist_y_cs_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Clausing-Sarin: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#67001F", 
                               "#980043", "#CE1256",
                               "#E7298A", "#DF65B0",
                               "#C994C7", "#D4B9DA",
                               "#E7E1EF", "#F7F4F9",
                               "#DADAEB", "#BCBDDC",
                               "#9E9AC8","#807DBA",
                               "mediumorchid1", "mediumorchid3",
                               "mediumorchid4", "#6A51A3",
                               "purple", "purple2",
                               "purple3",
                               "#3F007D","#54278F"
  )) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-10, 10),
                     breaks = c(-10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10),
                     expand = c(0,0))

stacked_dist_y_cs_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_y_cs_chart.png", width=1640, height=2000, res=300)
stacked_dist_y_cs_chart
dev.off()

#--------------------------------------------------Stacked Dist Age Full
stacked_dist_age_full <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_age_full")

stacked_dist_age_full_left <- stacked_dist_age_full %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_age_full_dots <- stacked_dist_age_full %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_age_full_left <- merge(stacked_dist_age_full_left, stacked_dist_age_full_dots, by = "Year", all.x = TRUE)
stacked_dist_age_full_left <- stacked_dist_age_full_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_age_full_chart <- 
  ggplot(stacked_dist_age_full_left, mapping = aes(fill=Provision, x=factor(Year, reorder_age), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_dist_age_full_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Full Extension: Contribution to Percent Change\n in After-Tax Income by Age Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#08306B" ,
                               "#08519C", "#2171B5",
                               "#4292C6","#6BAED6",
                               "steelblue2", "#9ECAE1",
                               "#C6DBEF",
                               "#DEEBF7", "#F7FBFF")) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-5, 5),
                     breaks = c(-5, -2.5, 0, 2.5, 5),
                     expand = c(0,0))

stacked_dist_age_full_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_age_full.png", width=1640, height=2000, res=300)
stacked_dist_age_full_chart
dev.off()
#-----------------------------------------------Stacked Dist Age Partial
stacked_dist_age_partial <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_age_partial")

stacked_dist_age_partial_left <- stacked_dist_age_partial %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_age_partial_dots <- stacked_dist_age_partial %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_age_partial_left <- merge(stacked_dist_age_partial_left, stacked_dist_age_partial_dots, by = "Year", all.x = TRUE)
stacked_dist_age_partial_left <- stacked_dist_age_partial_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_age_partial_chart <- 
  ggplot(stacked_dist_age_partial_left, mapping = aes(fill=Provision, x=factor(Year, reorder_age), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_dist_age_partial_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Partial Extension: Contribution to Percent Change\n in After-Tax Income by Age Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#FEC44F", "#FEE391",
                               "#FFF7BC", "#FFFFE5")) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-3, 3),
                     breaks = c(-3, -1.5, 0, 1.5, 3),
                     expand = c(0,0))

stacked_dist_age_partial_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_age_partial_chart.png", width=1640, height=2000, res=300)
stacked_dist_age_partial_chart
dev.off()

#------------------------Stacked Dist Age CS
library(ggtext)
stacked_dist_age_cs <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/stacked_dist_TCJA.xlsx",sheet = "stacked_dist_age_cs")

stacked_dist_age_cs_left <- stacked_dist_age_cs %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "value")

stacked_dist_age_cs_dots <- stacked_dist_age_cs %>%
  filter(Provision == "Total") %>%
  pivot_longer(!Provision, names_to = "Year", values_to = "Total") %>%
  select(-c(Provision))

stacked_dist_age_cs_left <- merge(stacked_dist_age_cs_left, stacked_dist_age_cs_dots, by = "Year", all.x = TRUE)
stacked_dist_age_cs_left <- stacked_dist_age_cs_left %>% 
  filter(Provision != "Total")


# Stacked
stacked_dist_age_cs_chart <- 
  ggplot(stacked_dist_age_cs_left, mapping = aes(fill=Provision, x=factor(Year, reorder_age), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, stacked_dist_age_cs_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Clausing-Sarin: Contribution to Percent Change\n in After-Tax Income by Age Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points", caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:<br></br>above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_fill_manual(values = c("#67001F", 
                               "#980043", "#CE1256",
                               "#E7298A", "#DF65B0",
                               "#C994C7", "#D4B9DA",
                               "#E7E1EF", "#F7F4F9",
                               "#DADAEB", "#BCBDDC",
                               "#9E9AC8","#807DBA",
                               "mediumorchid1", "mediumorchid3",
                               "mediumorchid4", "#6A51A3",
                               "purple", "purple2",
                               "purple3",
                               "#3F007D","#54278F"
  )) + 
  scale_color_manual(labels = c("Total"), values = c("black")) +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_text(size = 5),
        plot.caption = element_markdown(size = 6),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-4, 4),
                     breaks = c(-4, -2, 0, 2, 4),
                     expand = c(0,0))

stacked_dist_age_cs_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA/stacked_dist_age_cs_chart.png", width=1640, height=2000, res=300)
stacked_dist_age_cs_chart
dev.off()



# #---Export PDF to check them....
# 
# p1 <- cowplot::plot_grid(stacked_rev_full_chart,NULL,
#                          stacked_rev_full_chart_gdp,NULL,
#                          NULL,NULL,
#                          ncol = 1,
#                          rel_heights = c(1,1,1))
# 
# p2 <- cowplot::plot_grid(stacked_rev_partial_chart,NULL,
#                          stacked_rev_partial_chart_gdp,NULL,
#                          NULL,NULL,
#                          ncol = 1,
#                          rel_heights = c(1,1,1))
# p3 <- cowplot::plot_grid(stacked_rev_cs_chart,NULL,
#                          stacked_rev_cs_chart_gdp,NULL,
#                          NULL,NULL,
#                          ncol = 1,
#                          rel_heights = c(1,1,1))
# 
# p4 <- cowplot::plot_grid(stacked_dist_y_full_chart,NULL,
#                          stacked_dist_y_partial_chart,NULL,
#                          stacked_dist_y_cs_chart,NULL,
#                          ncol = 1,
#                          rel_heights = c(1,1,1))
# 
# p5 <- cowplot::plot_grid(stacked_dist_age_full_chart,NULL,
#                          stacked_dist_age_partial_chart,NULL,
#                          stacked_dist_age_cs_chart,NULL,
#                          ncol = 1,
#                          rel_heights = c(1,1,1))
# 
# pdf(paste0("/gpfs/gibbs/project/sarin/ds3228/production/output/TCJA_charts_", format(Sys.Date(), format = "%Y_%m_%d"),".pdf"))
# p1
# p2
# p3
# p4
# p5
# dev.off()
# 
# 
# 
# 
# 
# 
# 

#----Export PDF
# p1 <- cowplot::plot_grid(stacked_rev_full_chart, NULL, stacked_rev_full_chart_gdp,
#                          nrow = 3, ncol = 3,
#                          rel_widths = c(1, .05, 1))
# 
# 
# 
# 
# pdf("/gpfs/gibbs/project/sarin/ds3228/production/output/sample_memo_TCJA_charts.pdf", height = 11, width = 8.5, paper = "letter")
# p1
# p2
# dev.off()
