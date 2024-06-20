#---------------------------------------------------------------------------
#Installing our tools....
rm(list = ls()); cat("\014")
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")
options(scipen=10000)
reorder_1 <- c("Bottom quintile", "Second quintile", "Middle quintile", "Fourth quintile", "80% - 90%", "90% - 99%", "99% - 99.9%", "Top 0.1%")
reorder_age <- c("24 and under", "25 - 29","30 - 39","40 - 49","50 - 64","65+")
#---------------------------------------------------------------------------

rev_estimates <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/CTC_data.xlsx",sheet = "revenue_estimates")
colnames(rev_estimates) <- c("CTC design", "2025", "2026", "2027", "2028", "2029", "2030",
                             "2031", "2032", "2033", "2034", "Budget window", "Budget window percent", "Second decade", "Third decade")

rev_estimates_left <- rev_estimates %>%
  pivot_longer(!`CTC design`, names_to = "Year", values_to = "value")


rev_estimates_left <- rev_estimates_left %>% 
  filter(Year != "Budget window" & Year != "Budget window percent" & Year != "Second decade" & Year != "Third decade")

# Stacked
rev_estimates_chart <- 
  ggplot(rev_estimates_left, mapping = aes(fill=`CTC design`, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Budget Effects, FY2025-2054", subtitle = "Billions of Dollars", y = "USD Billions", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("Current policy" = "dodgerblue",
                               "Full FSA" = "tomato",
                               "CTC FSA" = "firebrick",
                               "2021 law" = "goldenrod",
                               "Edelberg-Kearney" = "forestgreen")) + 
  # briefing_theme()
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 8, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-700, 700),
                     breaks = c(-700, -350, 0, 350, 700),
                     expand = c(0,0))

rev_estimates_chart
png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/rev_estimates_chart.png", width=1640, height=1400, res=300)
rev_estimates_chart
dev.off()

#----Share of GDP
rev_estimates_right <- rev_estimates %>%
  pivot_longer(!`CTC design`, names_to = "Year", values_to = "value")


rev_estimates_right <- rev_estimates_right %>% 
  filter(Year != "Budget window" & Year == "Budget window percent" | Year == "Second decade" | Year == "Third decade")
rev_estimates_right$Year <-replace(rev_estimates_right$Year, rev_estimates_right$Year == "Budget window percent", "Budget window") 

# Stacked
rev_estimates_chart_gdp <- 
  ggplot(rev_estimates_right, mapping = aes(fill=`CTC design`, x=Year, y=value)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Estimated Conventional Budget Effects, FY2025-2054", subtitle = "Billions of Dollars", y = "Percent of GDP", caption = "Source: The Budget Lab") +
  scale_fill_manual(values = c("Current policy" = "dodgerblue",
                               "Full FSA" = "tomato",
                               "CTC FSA" = "firebrick",
                               "2021 law" = "goldenrod",
                               "Edelberg-Kearney" = "forestgreen")) + 
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 8, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-.015, .015),
                     breaks = c(-0.015,-0.0075, 0, 0.0075, 0.015),
                     expand = c(0,0))

rev_estimates_chart_gdp

png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/rev_estimates_chart_gdp.png", width=1640, height=1400, res=300)
rev_estimates_chart_gdp
dev.off()

# #---------------------Employment
# employment <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/CTC_data.xlsx",sheet = "employment")
# 
# employment_right <- employment %>%
#   pivot_longer(!`Reform option`, names_to = "Year", values_to = "value") %>%
#   filter(Year == "Change in employment")
# 
# employment_perc <- employment %>%
#   pivot_longer(!`Reform option`, names_to = "Year", values_to = "value") %>%
#   filter(Year != "Change in employment")
# # Stacked
# employment_chart <- 
#   ggplot(employment_right, mapping = aes(fill=`Reform option`, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Change in Employment", subtitle = "", y = "") +
#   scale_fill_manual(values = c("#FFEFCA", "#EDA16A" ,
#                                "#C83741", "#6C283D",
#                                "goldenrod","darkgoldenrod",
#                                "darkorange", "darkorange3",
#                                "gray59", "gray48")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         legend.position = "bottom") +
#   guides(fill = guide_legend(override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-200000, 20000),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      expand = c(0,0))
# 
# employment_chart
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/employment_chart.png", width=2400, height=1400, res=300)
# employment_chart
# dev.off()
# 
# employment_chart_perc <- 
#   ggplot(employment_perc, mapping = aes(fill=`Reform option`, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_y_continuous(sec.axis = dup_axis()) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Change in Employment", subtitle = "[ADD SUBTITLE]") +
#   scale_fill_manual(values = c("darkorange", "#6C2830","goldenrod",
#                                "#FFEFCA", "brown")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         axis.title.y.left = element_blank(),
#         axis.title.y.right = element_blank(),
#         axis.text.y.left = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(shape = NA))) +
#   set_y_axis(ymin = -.001, ymax = .006,y_delta = .001)
# 
# employment_chart_perc
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/employment_chart_perc.png", width=2400, height=1400, res=300)
# employment_chart_perc
# dev.off()
# #---------Partially Dynamic Offset
# partial_dynamic_offset <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/CTC_data.xlsx",sheet = "partially_dynamic_offset")
# 
# 
# partial_dynamic_offset_right <- partial_dynamic_offset %>%
#   pivot_longer(!Scenario, names_to = "Year", values_to = "value")
# 
# # Stacked
# partial_dynamic_offset_chart <- 
#   ggplot(partial_dynamic_offset_right, mapping = aes(fill=Scenario, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_y_continuous(sec.axis = dup_axis()) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Partial Dynamic Offset", subtitle = "[PLEASE ADD SUBTITLE]") +
#   scale_fill_manual(values = c("darkorange", "#6C2830","goldenrod",
#                                "#FFEFCA", "brown")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         axis.title.y.left = element_blank(),
#         axis.title.y.right = element_blank(),
#         axis.text.y.left = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(shape = NA))) +
#   set_y_axis(ymin = -50, ymax = 400, y_delta = 50)
# 
# partial_dynamic_offset_chart
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/partial_dynamic_offset_chart.png", width=2400, height=1400, res=300)
# partial_dynamic_offset_chart
# dev.off()
# 
# #---------Partially Dynamic Offset
# dynamic_offset <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/CTC_data.xlsx",sheet = "dynamic_offset")
# 
# 
# dynamic_offset_right <- dynamic_offset %>%
#   pivot_longer(!Scenario, names_to = "Year", values_to = "value")
# 
# # Stacked
# dynamic_offset_chart <- 
#   ggplot(dynamic_offset_right, mapping = aes(fill=Scenario, x=Year, y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_y_continuous(sec.axis = dup_axis()) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Partial Dynamic Offset", subtitle = "[PLEASE ADD SUBTITLE]") +
#   scale_fill_manual(values = c("darkorange", "#6C2830","goldenrod",
#                                "#FFEFCA", "brown")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         axis.title.y.left = element_blank(),
#         axis.title.y.right = element_blank(),
#         axis.text.y.left = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(shape = NA))) +
#   set_y_axis(ymin = -1200, ymax = 300, y_delta = 500)
# 
# dynamic_offset_chart
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/dynamic_offset_chart.png", width=2400, height=1400, res=300)
# dynamic_offset_chart
# dev.off()

#--------------Y_DIST_TCJA
y_dist_tcja <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "y_dist_tcja")

y_dist_tcja_left <- y_dist_tcja %>%
  pivot_longer(!provision, names_to = "Year", values_to = "value")

y_dist_tcja_dots <- y_dist_tcja %>%
  filter(provision == "Total") %>%
  pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
  select(-c(provision))

y_dist_tcja_left <- merge(y_dist_tcja_left, y_dist_tcja_dots, by = "Year", all.x = TRUE)
y_dist_tcja_left <- y_dist_tcja_left %>% 
  filter(provision != "Total")

library(ggtext)
# Stacked
y_dist_tcja_chart <- 
  ggplot(y_dist_tcja_left, mapping = aes(fill=provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, y_dist_tcja_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Current Policy: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026", subtitle = "Percentage Points", y = "Percentage Points",
       caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_color_manual(labels = c("Total"), values = c("black")) +
  scale_fill_manual(values = c("1) Impose SSN requirement" = "skyblue4",
                               "2) Increase maximum value" = "skyblue3",
                               "3) Limit refund" = "skyblue2",
                               "4) Increase the phase-out threshold" = "skyblue1")) + 
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        axis.text.x.bottom = element_text(size = 5.5),
        #axis.text.y.left = element_text(),
        plot.caption = element_markdown(size = 6),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 8, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-1, 1),
                     breaks = c(-1,-.5, 0, .5, 1),
                     expand = c(0,0))

y_dist_tcja_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/y_dist_tcja_chart.png", width=1800, height=1400, res=300)
y_dist_tcja_chart
dev.off()

#--------------Y_DIST_2021
y_dist_2021 <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "y_dist_2021")

y_dist_2021_left <- y_dist_2021 %>%
  pivot_longer(!provision, names_to = "Year", values_to = "value")

y_dist_2021_dots <- y_dist_2021 %>%
  filter(provision == "Total") %>%
  pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
  select(-c(provision))

y_dist_2021_left <- merge(y_dist_2021_left, y_dist_2021_dots, by = "Year", all.x = TRUE)
y_dist_2021_left <- y_dist_2021_left %>% 
  filter(provision != "Total")


# Stacked
y_dist_2021_chart <- 
  ggplot(y_dist_2021_left, mapping = aes(fill=provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, y_dist_2021_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "2021 Law: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026", subtitle = "Percentage Points", y = "Percentage Points",
       caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab')+
  scale_color_manual(labels = c("Total"), values = c("black")) +
  scale_fill_manual(values = c("1) Increase age limit" = "darkgoldenrod4",
                               "2) Increase maximum value" = "darkgoldenrod3",
                               "3) Eliminate phase-in" = "darkgoldenrod2",
                               "4) Increase phase-out threshold" = "darkgoldenrod1")) + 
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        axis.text.x.bottom = element_text(size = 5.5),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        plot.caption = element_markdown(size = 6),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 7, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-5, 5),
                     breaks = c(-5,-2.5, 0, 2.5, 5),
                     expand = c(0,0))
y_dist_2021_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/y_dist_2021_chart.png", width=1800, height=1400, res=300)
y_dist_2021_chart
dev.off()

#--------------Y_DIST_EK
y_dist_ek <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "y_dist_ek")

y_dist_ek_left <- y_dist_ek %>%
  pivot_longer(!provision, names_to = "Year", values_to = "value")

y_dist_ek_dots <- y_dist_ek %>%
  filter(provision == "Total") %>%
  pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
  select(-c(provision))

y_dist_ek_left <- merge(y_dist_ek_left, y_dist_ek_dots, by = "Year", all.x = TRUE)
y_dist_ek_left <- y_dist_ek_left %>% 
  filter(provision != "Total")


# Stacked
y_dist_ek_chart <- 
  ggplot(y_dist_ek_left, mapping = aes(fill=provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, y_dist_ek_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Edelberg-Kearney: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026", subtitle = "Percentage Points", y = "Percentage Points",
       caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_color_manual(labels = c("Total"), values = c("black")) +
  scale_fill_manual(values = c("1) Increase age limit" = "darkolivegreen4",
                               "2) Increase maximum value" = "darkolivegreen3",
                               "3) Allow half of credit at $0 of earnings and increase phase-in rate" = "darkolivegreen2",
                               "4) Reduce phase-out rate" = "darkolivegreen1")) + 
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        axis.title.y.right = element_blank(),
        plot.caption = element_markdown(size = 6),
        axis.text.x.bottom = element_text(size = 5.5),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 7, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-5, 5),
                     breaks = c(-5,-2.5, 0, 2.5, 5),
                     expand = c(0,0))

y_dist_ek_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/y_dist_ek_chart.png", width=1800, height=1400, res=300)
y_dist_ek_chart
dev.off()

#--------------Y_DIST_FSA
y_dist_fsa <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "y_dist_fsa")

y_dist_fsa_left <- y_dist_fsa %>%
  pivot_longer(!provision, names_to = "Year", values_to = "value")

y_dist_fsa_dots <- y_dist_fsa %>%
  filter(provision == "Total") %>%
  pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
  select(-c(provision))

y_dist_fsa_left <- merge(y_dist_fsa_left, y_dist_fsa_dots, by = "Year", all.x = TRUE)
y_dist_fsa_left <- y_dist_fsa_left %>% 
  filter(provision != "Total")

y_dist_fsa_left$provision[y_dist_fsa_left$provision == "7) Eliminate Child and Dependent Care Tax Credut"] <- "7) Eliminate Child and Dependent Care Tax Credit"

# Stacked
y_dist_fsa_chart <- 
  ggplot(y_dist_fsa_left, mapping = aes(fill=provision, x=factor(Year, reorder_1), y=value)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(size = 4, y_dist_fsa_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
  theme_bly_style() +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
  labs(title = "Family Security Act 2.0: Contribution to Percent Change\n in After-Tax Income by Income Group, 2026",
       subtitle = "Percentage Points", y = "Percentage Points",
       caption = '<i>Estimate universe is nondependent tax units, including nonfilers. "Income" is measured as AGI plus:</br><br> above-the-line deductions, nontaxable interest, nontaxable pension income (including OASI benefits),</br><br> and employer-side payroll taxes, Income percentile thresholds are calculated with respect to positive income only</br><br> and are adult-weighted.</i></br><br>Source: The Budget Lab') +
  scale_color_manual(labels = c("Total"), values = c("black")) +
  scale_fill_manual(values = c("1) Raise age limit" = "firebrick4",
                               "2) Increase maximum value" = "firebrick3",
                               "3) Increase phase-in rate" = "firebrick2",
                               "4) Increase phase-out threshold" = "firebrick1",
                               "5) Reform the EITC" = "tomato",
                               "6) Eliminate Head Of Household filing status" = "tomato2",
                               "7) Eliminate Child and Dependent Care Tax Credit" = "tomato3",
                               "8) Eliminate deduction for state and local taxes" = "tomato4")) + 
  theme(axis.title.x.bottom = element_blank(),
        #axis.title.y.left = element_text(),
        plot.caption = element_markdown(size = 6),
        axis.title.y.right = element_blank(),
        axis.text.x.bottom = element_text(size = 5.5),
        #axis.text.y.left = element_text(),
        axis.text.y.right = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "right",
        legend.text = ggplot2::element_text(size = 6, family = "Mallory"),
        legend.key.size = ggplot2::unit(0.3, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-4, 4),
                     breaks = c(-4,-2, 0, 2, 4),
                     expand = c(0,0))

y_dist_fsa_chart

png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/y_dist_fsa_chart.png", width=2500, height=1400, res=300)
y_dist_fsa_chart
dev.off()

# #--------------AGE_DIST_TCJA
# age_dist_tcja <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "age_dist_tcja")
# 
# age_dist_tcja_left <- age_dist_tcja %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "value")
# 
# age_dist_tcja_dots <- age_dist_tcja %>%
#   filter(provision == "Total") %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
#   select(-c(provision))
# 
# age_dist_tcja_left <- merge(age_dist_tcja_left, age_dist_tcja_dots, by = "Year", all.x = TRUE)
# age_dist_tcja_left <- age_dist_tcja_left %>% 
#   filter(provision != "Total")
# 
# 
# # Stacked
# age_dist_tcja_chart <- 
#   ggplot(age_dist_tcja_left, mapping = aes(fill=provision, x=factor(Year, reorder_age), y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(size = 4, age_dist_tcja_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Age Dist. TCJA", subtitle = "", y = "Percentage Points") +
#   scale_color_manual(labels = c("Total"), values = c("black")) +
#   scale_fill_manual(values = c("1) Impose SSN requirement" = "dodgerblue",
#                                "2) Increase maximum value" = "firebrick",
#                                "3) Limit refund" = "goldenrod",
#                                "4) Increase the phase-out threshold" = "forestgreen")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         legend.position = "bottom",
#         legend.text = ggplot2::element_text(size = 8, family = "Mallory"),
#         legend.key.size = ggplot2::unit(0.3, 'cm')) +
#   guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-1.5, 1.5),
#                      breaks = c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5),
#                      expand = c(0,0))
# 
# age_dist_tcja_chart
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/age_dist_tcja_chart.png", width=1640, height=1400, res=300)
# age_dist_tcja_chart
# dev.off()
# #-------------AGE_DIST_2021
# age_dist_2021 <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "age_dist_2021")
# 
# age_dist_2021_left <- age_dist_2021 %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "value")
# 
# age_dist_2021_dots <- age_dist_2021 %>%
#   filter(provision == "Total") %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
#   select(-c(provision))
# 
# age_dist_2021_left <- merge(age_dist_2021_left, age_dist_2021_dots, by = "Year", all.x = TRUE)
# age_dist_2021_left <- age_dist_2021_left %>% 
#   filter(provision != "Total")
# 
# 
# # Stacked
# age_dist_2021_chart <- 
#   ggplot(age_dist_2021_left, mapping = aes(fill=provision, x=factor(Year, reorder_age), y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(size = 4, age_dist_2021_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Age Dist. 2021", subtitle = "", y = "Percentage Points") +
#   scale_color_manual(labels = c("Total"), values = c("black")) +
#   scale_fill_manual(values = c("1) Increase age limit" = "dodgerblue",
#                                "2) Increase maximum value" = "firebrick",
#                                "3) Eliminate phase-in" = "goldenrod",
#                                "4) Increase phase-out threshold" = "forestgreen")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         legend.position = "bottom",
#         legend.text = ggplot2::element_text(size = 8, family = "Mallory"),
#         legend.key.size = ggplot2::unit(0.3, 'cm')) +
#   guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-3, 3),
#                      breaks = c(-3,-1.5, 0, 1.5, 3),
#                      expand = c(0,0))
# 
# age_dist_2021_chart
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/age_dist_2021_chart.png", width=1640, height=1400, res=300)
# age_dist_2021_chart
# dev.off()
# 
# 
# #---------------AGE_DIST_EK
# age_dist_ek <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "age_dist_ek")
# 
# age_dist_ek_left <- age_dist_ek %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "value")
# 
# age_dist_ek_dots <- age_dist_ek %>%
#   filter(provision == "Total") %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
#   select(-c(provision))
# 
# age_dist_ek_left <- merge(age_dist_ek_left, age_dist_ek_dots, by = "Year", all.x = TRUE)
# age_dist_ek_left <- age_dist_ek_left %>% 
#   filter(provision != "Total")
# 
# 
# # Stacked
# age_dist_ek_chart <- 
#   ggplot(age_dist_ek_left, mapping = aes(fill=provision, x=factor(Year, reorder_age), y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(size = 4, age_dist_ek_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Age Dist. EK", subtitle = "", y = "Percentage Points") +
#   scale_color_manual(labels = c("Total"), values = c("black")) +
#   scale_fill_manual(values = c("1) Increase age limit" = "dodgerblue",
#                                "2) Increase maximum value" = "firebrick",
#                                "3) Allow half of credit at $0 of earnings and increase phase-in rate" = "goldenrod",
#                                "4) Reduce phase-out rate" = "forestgreen")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         legend.position = "bottom",
#         legend.text = ggplot2::element_text(size = 6, family = "Mallory"),
#         legend.key.size = ggplot2::unit(0.3, 'cm')) +
#   guides(fill = guide_legend(override.aes = list(shape = NA), ncol = 2)) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-3, 3),
#                      breaks = c(-3,-1.5, 0, 1.5, 3),
#                      expand = c(0,0))
# 
# age_dist_ek_chart
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/age_dist_ek_chart.png", width=1640, height=1400, res=300)
# age_dist_ek_chart
# dev.off()
# #--------------AGE_DIST_FSA
# age_dist_fsa <- readxl::read_xlsx("/gpfs/gibbs/project/sarin/ds3228/production/data/ctc_y_dist_age_dist.xlsx",sheet = "age_dist_fsa")
# 
# age_dist_fsa_left <- age_dist_fsa %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "value")
# 
# age_dist_fsa_dots <- age_dist_fsa %>%
#   filter(provision == "Total") %>%
#   pivot_longer(!provision, names_to = "Year", values_to = "Total") %>%
#   select(-c(provision))
# 
# age_dist_fsa_left <- merge(age_dist_fsa_left, age_dist_fsa_dots, by = "Year", all.x = TRUE)
# age_dist_fsa_left <- age_dist_fsa_left %>% 
#   filter(provision != "Total")
# 
# 
# # Stacked
# age_dist_fsa_chart <- 
#   ggplot(age_dist_fsa_left, mapping = aes(fill=provision, x=factor(Year, reorder_age), y=value)) + 
#   geom_bar(position="stack", stat="identity") +
#   geom_point(size = 4, age_dist_fsa_left, mapping = aes(x = Year, y = `Total`, color = "Total")) +
#   theme_bly_style() +
#   geom_hline(yintercept=0, linetype="solid", color = "black", size=.1) +
#   labs(title = "Add Title: Age Dist. FSA", subtitle = "", y = "Percentage Points") +
#   scale_color_manual(labels = c("Total"), values = c("black")) +
#   scale_fill_manual(values = c("1) Raise age limit" = "black",
#                                "2) Increase maximum value" = "dodgerblue",
#                                "3) Increase phase-in rate" = "red2",
#                                "4) Increase phase-out threshold" = "goldenrod",
#                                "5) Reform the EITC" = "forestgreen",
#                                "6) Eliminate Head Of Household filing status" = "mediumorchid",
#                                "7) Eliminate Child and Dependent Care Tax Credit" = "deeppink2",
#                                "8) Eliminate deduction for state and local taxes" = "darkorange")) + 
#   theme(axis.title.x.bottom = element_blank(),
#         #axis.title.y.left = element_text(),
#         axis.title.y.right = element_blank(),
#         #axis.text.y.left = element_text(),
#         axis.text.y.right = element_blank(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         legend.position = "right",
#         legend.text = ggplot2::element_text(size = 6, family = "Mallory"),
#         legend.key.size = ggplot2::unit(0.3, 'cm')) +
#   guides(fill = guide_legend(override.aes = list(shape = NA))) +
#   scale_y_continuous(sec.axis = dup_axis(),
#                      limits = c(-3, 3),
#                      breaks = c(-3,-1.5, 0, 1.5, 3),
#                      expand = c(0,0))
# 
# age_dist_fsa_chart
# 
# png("/gpfs/gibbs/project/sarin/ds3228/production/output/CTC/age_dist_fsa_chart.png", width=2400, height=1400, res=300)
# age_dist_fsa_chart
# dev.off()

#----Export PDF
# p1 <- cowplot::plot_grid(NULL, NULL, NULL,
#                          y_dist_tcja_chart, NULL, y_dist_2021_chart,
#                          y_dist_ek_chart, NULL, y_dist_fsa_chart,
#                          nrow = 3, ncol = 3,
#                          rel_widths = c(1, .05, 1))
# 
# p2 <- cowplot::plot_grid(y_dist_2021_chart, NULL, y_dist_ek_chart,
#                          y_dist_fsa_chart, NULL, NULL,
#                          NULL, NULL, NULL,
#                          nrow = 3, ncol = 3,
#                          rel_widths = c(1, .05, 1))
# 
# 
# pdf("/gpfs/gibbs/project/sarin/ds3228/production/output/sample_memo_charts.pdf", height = 11, width = 8.5, paper = "letter")
# p1
# p2
# dev.off()




