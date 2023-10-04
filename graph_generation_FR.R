library(DaisTheme)
library(ggplot2)
library(data.table)
library(ggpp)
library(scales)
library(tidyverse)
library(utils)

#########################################################
#Load in graph spread sheets
graph.data <-  fread("Graphs_data/Graphs_spreadsheet.csv")
translation <- fread("Graphs_data/graph_translation_fr.csv")

figure_1_data_FR <- read.csv("Graphs_data/Figure_1.csv")
translation_f1 <- translation %>%
  filter(str_detect(Figure, "Figure 1,"))
figure_1_data_FR <- figure_1_data_FR%>%
  mutate(tech_job =  case_when(
         str_detect(tech_job, translation_f1$English[1]) ~ translation_f1$French[1],
         str_detect(tech_job, translation_f1$English[2]) ~ translation_f1$French[2]))%>%
  mutate(GEO =  case_when(
    str_detect(GEO, translation_f1$English[3]) ~ translation_f1$French[3],
    str_detect(GEO, translation_f1$English[4]) ~ translation_f1$French[4]))

figure_1_data_FR <- as.data.table(figure_1_data_FR)

figure.1 <- plot.column.dais(figure_1_data_FR, m, tech_job, group.by = GEO, order.bar="ascending", label=TRUE, language = "FR",
                           plot.fig.num = "Figure 1",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 1",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 1", Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 1",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GEO, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_2_data_FR <- read.csv("Graphs_data/Figure_2.csv")
translation_f2 <- translation %>%
  filter(str_detect(Figure, "Figure 2"))
figure_2_data_FR <- figure_2_data_FR%>%
  mutate(tech_job =  case_when(
    str_detect(tech_job, translation_f2$English[1]) ~ translation_f2$French[1],
    str_detect(tech_job, translation_f2$English[2]) ~ translation_f2$French[2]))%>%
  mutate(GEO =  case_when(
    str_detect(GEO, translation_f2$English[3]) ~ translation_f2$French[3],
    str_detect(GEO, translation_f2$English[4]) ~ translation_f2$French[4]))

figure_2_data_FR <- as.data.table(figure_2_data_FR)

wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

figure.2 <- plot.column.dais(figure_2_data_FR, m, tech_job, group.by = GEO, label=TRUE, language = "FR",
                             plot.fig.num = "Figure 2",
                             plot.title= wrapper(graph.data[graph.data$Figure_number=="Figure 2",Figure_title_FR],55),
                             y.axis= graph.data[graph.data$Figure_number=="Figure 2",Y_Axis_FR],
                             caption = graph.data[graph.data$Figure_number=="Figure 2",Caption_FR],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis_Ticks],
                             label.adjust = 0.025)+
                             aes(x = reorder(GEO, m))+facet_grid(.~tech_job, switch = "x")+
                             theme(axis.text.x = element_blank())

figure_3_data_FR <- read.csv("Graphs_data/Figure_3.csv")
translation_f3 <- translation %>%
  filter(str_detect(Figure, "Figure 3"))
figure_3_data_FR <- figure_3_data_FR%>%
  mutate(tech_job =  case_when(
    str_detect(tech_job, translation_f3$English[1]) ~ translation_f3$French[1],
    str_detect(tech_job, translation_f3$English[2]) ~ translation_f3$French[2]))%>%
  mutate(GENDER =  case_when(
    str_detect(GENDER, translation_f3$English[3]) ~ translation_f3$French[3],
    str_detect(GENDER, translation_f3$English[4]) ~ translation_f3$French[4]))

figure_3_data_FR <- as.data.table(figure_3_data_FR)

figure.3 <- plot.column.dais(figure_3_data_FR, m, tech_job, group.by = GENDER, label=TRUE, language = "FR",
                           plot.fig.num = "Figure 3",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 3",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 3",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GENDER, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_4_data_FR <- read.csv("Graphs_data/Figure_4.csv")
translation_f4 <- translation %>%
  filter(str_detect(Figure, "Figure 4"))
figure_4_data_FR <- figure_4_data_FR%>%
  mutate(tech_job =  case_when(
    str_detect(tech_job, translation_f4$English[1]) ~ translation_f4$French[1],
    str_detect(tech_job, translation_f4$English[2]) ~ translation_f4$French[2]))%>%
  mutate(GENDER =  case_when(
    str_detect(GENDER, translation_f4$English[3]) ~ translation_f4$French[3],
    str_detect(GENDER, translation_f4$English[4]) ~ translation_f4$French[4]))

figure_4_data_FR <- as.data.table(figure_4_data_FR)

figure.4 <- plot.column.dais(figure_4_data_FR, m, tech_job, group.by = GENDER,  label=TRUE, language = "FR",
                           plot.fig.num = "Figure 4",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 4",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 4",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GENDER, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_5_data_FR <- read.csv("Graphs_data/Figure_5.csv")

translation_f5 <- translation %>%
  filter(str_detect(Figure, "Figure 5"))
figure_5_data_FR <- figure_5_data_FR%>%
  mutate(tech_job =  case_when(
    str_detect(tech_job, translation_f5$English[1]) ~ translation_f5$French[1],
    str_detect(tech_job, translation_f5$English[2]) ~ translation_f5$French[2]))%>%
  mutate(EDU =  case_when(
    str_detect(EDU, translation_f5$English[3]) ~ translation_f5$French[3],
    str_detect(EDU, translation_f5$English[4]) ~ translation_f5$French[4],
    str_detect(EDU, translation_f5$English[5]) ~ translation_f5$French[5],
    str_detect(EDU, translation_f5$English[6]) ~ translation_f5$French[6],
    str_detect(EDU, translation_f5$English[7]) ~ translation_f5$French[7],
    str_detect(EDU, translation_f5$English[8]) ~ translation_f5$French[8]))

figure_5_data_FR <- as.data.table(figure_5_data_FR)

figure.5 <- plot.column.dais(figure_5_data_FR, m, tech_job, group.by = EDU, order.bar="ascending", label=TRUE, language = "FR",
                           plot.fig.num = "Figure 5",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 5",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 5",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(EDU, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_6_data_FR <- read.csv("Graphs_data/Figure_6.csv")

translation_f6 <- translation %>%
  filter(str_detect(Figure, "Figure 6"))
figure_6_data_FR <- figure_6_data_FR%>%
  mutate(tech_job =  case_when(
    str_detect(tech_job, translation_f6$English[1]) ~ translation_f6$French[1],
    str_detect(tech_job, translation_f6$English[2]) ~ translation_f6$French[2]))%>%
  mutate(EDU =  case_when(
    str_detect(EDU, translation_f6$English[3]) ~ translation_f6$French[3],
    str_detect(EDU, translation_f6$English[4]) ~ translation_f6$French[4],
    str_detect(EDU, translation_f6$English[5]) ~ translation_f6$French[5],
    str_detect(EDU, translation_f6$English[6]) ~ translation_f6$French[6],
    str_detect(EDU, translation_f6$English[7]) ~ translation_f6$French[7],
    str_detect(EDU, translation_f6$English[8]) ~ translation_f6$French[8]))

figure_6_data_FR <- as.data.table(figure_6_data_FR)

figure.6 <-plot.column.dais(figure_6_data_FR, m, tech_job, group.by = EDU, order.bar="ascending", label=TRUE, language = "FR",
                            plot.fig.num = "Figure 6",
                            plot.title= graph.data[graph.data$Figure_number=="Figure 6",Figure_title_FR],
                            y.axis= graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_FR],
                            caption = graph.data[graph.data$Figure_number=="Figure 6",Caption_FR],
                            label.unit = graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks],
                            label.adjust = 0.025)+
                            aes(x = reorder(EDU, m))+facet_grid(.~tech_job, switch = "x")+
                            theme(axis.text.x = element_blank())
                          
figure_7_data_FR <- read.csv("Graphs_data/Figure_7_tech.csv")

translation_f7 <- translation %>%
  filter(str_detect(Figure, "Figure 7"))
figure_7_data_FR <- figure_7_data_FR%>%
  mutate(Visible_Minority =  case_when(
    str_detect(Visible_Minority, translation_f7$English[1]) ~ translation_f7$French[1],
    str_detect(Visible_Minority, translation_f7$English[2]) ~ translation_f7$French[2],
    str_detect(Visible_Minority, translation_f7$English[3]) ~ translation_f7$French[3],
    str_detect(Visible_Minority, translation_f7$English[4]) ~ translation_f7$French[4],
    str_detect(Visible_Minority, translation_f7$English[5]) ~ translation_f7$French[5],
    str_detect(Visible_Minority, translation_f7$English[6]) ~ translation_f7$French[6],
    str_detect(Visible_Minority, translation_f7$English[7]) ~ translation_f7$French[7],
    str_detect(Visible_Minority, translation_f7$English[8]) ~ translation_f7$French[8],
    str_detect(Visible_Minority, translation_f7$English[9]) ~ translation_f7$French[9],
    str_detect(Visible_Minority, translation_f7$English[10]) ~ translation_f7$French[10],
    str_detect(Visible_Minority, translation_f7$English[11]) ~ translation_f7$French[11]))

figure_7_data_FR <- as.data.table(figure_7_data_FR)

figure.7 <- plot.column.dais(figure_7_data_FR, INC, Visible_Minority,order.bar = 'ascending', label=TRUE, language = "FR",
                             plot.fig.num = "Figure 7",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 7",Figure_title_FR],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_FR],
                             caption = graph.data[graph.data$Figure_number=="Figure 7",Caption_FR],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks],
                             label.adjust = 0.025)+
                             theme(axis.text.x = element_text(angle = 30, vjust=1))

figure_8_data_FR <- read.csv("Graphs_data/Figure_8_tech.csv")

translation_f8 <- translation %>%
  filter(str_detect(Figure, "Figure 8"))
figure_8_data_FR <- figure_8_data_FR%>%
  mutate(RACE =  case_when(
    str_detect(RACE, translation_f8$English[1]) ~ translation_f8$French[1],
    str_detect(RACE, translation_f8$English[2]) ~ translation_f8$French[2],
    str_detect(RACE, translation_f8$English[3]) ~ translation_f8$French[3],
    str_detect(RACE, translation_f8$English[4]) ~ translation_f8$French[4],
    str_detect(RACE, translation_f8$English[5]) ~ translation_f8$French[5],
    str_detect(RACE, translation_f8$English[6]) ~ translation_f8$French[6],
    str_detect(RACE, translation_f8$English[7]) ~ translation_f8$French[7],
    str_detect(RACE, translation_f8$English[8]) ~ translation_f8$French[8],
    str_detect(RACE, translation_f8$English[9]) ~ translation_f8$French[9],
    str_detect(RACE, translation_f8$English[10]) ~ translation_f8$French[10],
    str_detect(RACE, translation_f8$English[11]) ~ translation_f8$French[11]))

figure_8_data_FR <- as.data.table(figure_8_data_FR)

figure.8 <- plot.column.dais(figure_8_data_FR,m, RACE, label=TRUE, order.bar="ascending", language = "FR", 
                           plot.fig.num = "Figure 8",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 8",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 8",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           theme(axis.text.x = element_text(angle = 60, vjust=1))

figure_9_data_FR <- read.csv("Graphs_data/Figure_9.csv")
load("Graphs_data/Figure_9_bold_FR.rdata")

translation_f9 <- translation %>%
  filter(str_detect(Figure, "Figure 9"))
figure_9_data_FR <- figure_9_data_FR%>%
  mutate(tech =  case_when(
    str_detect(tech, translation_f9$English[1]) ~ translation_f9$French[1],
    str_detect(tech, translation_f9$English[2]) ~ translation_f9$French[2]))

figure_9_data_FR <- as.data.table(figure_9_data_FR)

figure.9 <- plot.column.dais(figure_9_data_FR, estimate, term ,label=FALSE, group.by = tech, order.bar = "ascending", language = "FR",
                           plot.fig.num = "Figure 9",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 9",Figure_title_FR],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_FR],
                           caption = graph.data[graph.data$Figure_number=="Figure 9",Caption_FR],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           theme(axis.text.x = element_blank())+
                           geom_text(label=Figure_9_bold_FR[c(2,1)],
                                     parse=TRUE,
                                     position=position_dodgenudge(width=0.6,y=700), 
                                     vjust=0.6)

figure_10_data_FR <- read.csv("Graphs_data/Figure_10.csv")
load("Graphs_data/Figure_10_bold_FR.rdata")
load("Graphs_data/Figure_10_signs.rdata")

translation_f10 <- translation %>%
  filter(str_detect(Figure, "Figure 10"))
figure_10_data_FR <- figure_10_data_FR%>%
  mutate(tech =  case_when(
    str_detect(tech, translation_f10$English[1]) ~ translation_f10$French[1],
    str_detect(tech, translation_f10$English[2]) ~ translation_f10$French[2]))%>%
  mutate(GEO =  case_when(
    str_detect(GEO, translation_f10$English[3]) ~ translation_f10$French[3],
    str_detect(GEO, translation_f10$English[4]) ~ translation_f10$French[4]))

figure_10_data_FR <- as.data.table(figure_10_data_FR)

figure.10 <- plot.column.dais(figure_10_data_FR, estimate, tech ,label=FALSE, group.by = GEO, language = "FR",
                              plot.fig.num = "Figure 10",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 10",Figure_title_FR],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 10",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              scale_y_continuous(limits=c(min(figure_10_data_FR$estimate)-200,0), labels = unit_format(prefix = "", sep = " ", suffix = " $"))+
                              geom_text(label=Figure_10_bold_FR[c(2,4,1,3)],
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=-20),
                                        vjust=Figure_10_signs)+
                              theme(axis.text.x = element_text(angle = 0, hjust = 0.2))

figure_11_data_FR <- read.csv("Graphs_data/Figure_11.csv")
load("Graphs_data/Figure_11_bold_FR.rdata")
load("Graphs_data/Figure_11_signs.rdata")

translation_f11 <- translation %>%
  filter(str_detect(Figure, "Figure 11"))
translation_f11$English <- replace(translation_f11$English, translation_f11$English == "College","College diploma" )
figure_11_data_FR <- figure_11_data_FR%>%
  mutate(tech =  case_when(
    str_detect(tech, translation_f11$English[1]) ~ translation_f11$French[1],
    str_detect(tech, translation_f11$English[2]) ~ translation_f11$French[2]))%>%
  mutate(term =  case_when(
    str_detect(term, translation_f11$English[3]) ~ translation_f11$French[3],
    str_detect(term, translation_f11$English[4]) ~ translation_f11$French[4],
    str_detect(term, translation_f11$English[5]) ~ translation_f11$French[5],
    str_detect(term, translation_f11$English[6]) ~ translation_f11$French[6],
    str_detect(term, translation_f11$English[7]) ~ translation_f11$French[7]))

figure_11_data_FR <- as.data.table(figure_11_data_FR)

figure.11 <- plot.column.dais(figure_11_data_FR, estimate, tech ,label=FALSE, group.by = term, 
                             plot.fig.num = "Figure 11",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 11",Figure_title_FR],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_FR],
                             caption = graph.data[graph.data$Figure_number=="Figure 11",Caption_FR],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks],
                             label.adjust = 0.025)+
                             aes(x = reorder(term, estimate))+facet_grid(.~tech, switch = "x")+
                             scale_y_continuous(limits=c(min(figure_11_data_FR$estimate)-1000,max(figure_11_data_FR$estimate)+800),
                                                labels = unit_format(prefix = "", sep = " ", suffix = " $"))+
                             geom_text(label=Figure_11_bold_FR[c(9,7,6,10,8,4,2,1,5,3)],
                                      parse=TRUE,
                                      position=position_dodgenudge(width=0.6,y=700),
                                      vjust=Figure_11_signs)+
                             theme(axis.text.x = element_blank())

figure_12_data_FR <- read.csv("Graphs_data/Figure_12.csv")
load("Graphs_data/Figure_12_bold_FR.rdata")

translation_f12 <- translation %>%
  filter(str_detect(Figure, "Figure 12"))
translation_f12$English <- replace(translation_f12$English, translation_f12$English == "College","College diploma" )
figure_12_data_FR <- figure_12_data_FR%>%
  mutate(tech =  case_when(
    str_detect(tech, translation_f12$English[1]) ~ translation_f12$French[1],
    str_detect(tech, translation_f12$English[2]) ~ translation_f12$French[2]))%>%
  mutate(term =  case_when(
    str_detect(term, translation_f12$English[3]) ~ translation_f12$French[3],
    str_detect(term, translation_f12$English[4]) ~ translation_f12$French[4],
    str_detect(term, translation_f12$English[5]) ~ translation_f12$French[5],
    str_detect(term, translation_f12$English[6]) ~ translation_f12$French[6],
    str_detect(term, translation_f12$English[7]) ~ translation_f12$French[7]))

figure_12_data_FR <- as.data.table(figure_12_data_FR)

figure.12 <- plot.column.dais(figure_12_data_FR, estimate, tech ,label=FALSE, group.by = term, language = "FR",
                              plot.fig.num = "Figure 12",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 12",Figure_title_FR],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 12",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 12",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 12",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              aes(x = reorder(term, estimate))+facet_grid(.~tech, switch = "x")+
                              geom_text(label=Figure_12_bold_FR[c(9,7,6,10,8,4,2,1,5,3)],
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=2000))+
                              theme(axis.text.x = element_blank())

figure_13_data_FR <- read.csv("Graphs_data/Figure_13_tech.csv")
load("Graphs_data/Figure_13_tech_bold_FR.rdata")
load("Graphs_data/Figure_13_tech_signs.rdata")

translation_f13 <- translation %>%
  filter(str_detect(Figure, "Figure 13"))
figure_13_data_FR <- figure_13_data_FR%>%
  mutate(term =  case_when(
    str_detect(term, translation_f13$English[1]) ~ translation_f13$French[1],
    str_detect(term, translation_f13$English[2]) ~ translation_f13$French[2],
    str_detect(term, translation_f13$English[3]) ~ translation_f13$French[3],
    str_detect(term, translation_f13$English[4]) ~ translation_f13$French[4],
    str_detect(term, translation_f13$English[5]) ~ translation_f13$French[5],
    str_detect(term, translation_f13$English[6]) ~ translation_f13$French[6],
    str_detect(term, translation_f13$English[7]) ~ translation_f13$French[7],
    str_detect(term, translation_f13$English[8]) ~ translation_f13$French[8],
    str_detect(term, translation_f13$English[9]) ~ translation_f13$French[9],
    str_detect(term, translation_f13$English[10]) ~ translation_f13$French[10]))

figure_13_data_FR <- as.data.table(figure_13_data_FR)

figure.13 <- plot.column.dais(figure_13_data_FR, estimate, term ,label=FALSE, order.bar = 'ascending',
                              plot.fig.num = "Figure 13",
                              plot.title= wrapper(graph.data[graph.data$Figure_number=="Figure 13",Figure_title_FR],75),
                              y.axis= graph.data[graph.data$Figure_number=="Figure 13",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 13",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 13",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(min(figure_13_data_FR$estimate)-1000,max(figure_13_data_FR$estimate)+1000),
                     labels = unit_format(prefix = "", sep = " ", suffix = " $"))+
  geom_text(label=Figure_13_tech_bold_FR[c(1,8,2,4,9,10,3,7,6,5)],
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=700),
            vjust=Figure_13_tech_signs)+
  theme(axis.text.x = element_text(angle=30, vjust=1))

figure_14_data_FR <- read.csv("Graphs_data/Figure_14_tech.csv")
load("Graphs_data/Figure_14_tech_bold_FR.rdata")
load("Graphs_data/Figure_14_tech_signs.rdata")

translation_f14 <- translation %>%
  filter(str_detect(Figure, "Figure 14"))
figure_14_data_FR <- figure_14_data_FR%>%
  mutate(term =  case_when(
    str_detect(term, translation_f14$English[1]) ~ translation_f14$French[1],
    str_detect(term, translation_f14$English[2]) ~ translation_f14$French[2],
    str_detect(term, translation_f14$English[3]) ~ translation_f14$French[3],
    str_detect(term, translation_f14$English[4]) ~ translation_f14$French[4],
    str_detect(term, translation_f14$English[5]) ~ translation_f14$French[5],
    str_detect(term, translation_f14$English[6]) ~ translation_f14$French[6],
    str_detect(term, translation_f14$English[7]) ~ translation_f14$French[7],
    str_detect(term, translation_f14$English[8]) ~ translation_f14$French[8],
    str_detect(term, translation_f14$English[9]) ~ translation_f14$French[9],
    str_detect(term, translation_f14$English[10]) ~ translation_f14$French[10]))

figure_14_data_FR <- as.data.table(figure_14_data_FR)

figure.14 <- plot.column.dais(figure_14_data_FR, estimate, term ,label=FALSE, order.bar = 'ascending',
                              plot.fig.num = "Figure 14",
                              plot.title= wrapper(graph.data[graph.data$Figure_number=="Figure 14",Figure_title_FR],75),
                              y.axis= graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 14",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(min(figure_14_data_FR$estimate)-1000,max(figure_14_data_FR$estimate)+1000),
                     labels = unit_format(prefix = "", sep = " ", suffix = " $"))+
  geom_text(label=Figure_14_tech_bold_FR[c(7,2,3,8,10,6,3,4,5,9)],
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=800),
            vjust=Figure_14_tech_signs)+
  theme(axis.text.x = element_text(angle=30, vjust=1))

figure_15_data_FR <- read.csv("Graphs_data/Figure_15_tech.csv")
load("Graphs_data/Figure_15_tech_bold_FR.rdata")

translation_f15 <- translation %>%
  filter(str_detect(Figure, "Figure 15"))
figure_15_data_FR <- figure_15_data_FR%>%
  mutate(GEO =  case_when(
    str_detect(GEO, translation_f15$English[1]) ~ translation_f15$French[1],
    str_detect(GEO, translation_f15$English[2]) ~ translation_f15$French[2]))

figure_15_data_FR <- as.data.table(figure_15_data_FR)

figure.15 <- plot.column.dais(figure_15_data_FR, estimate, term ,label=FALSE, order.bar = 'ascending',  group.by= GEO,
                              plot.fig.num = "Figure 15",
                              plot.title= wrapper(graph.data[graph.data$Figure_number=="Figure 15",Figure_title_FR],75),
                              y.axis= graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 15",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              scale_y_continuous(limits=c(0,max(figure_15_data_FR$estimate)+800),
                                                 labels = unit_format(prefix = "", sep = " ", suffix = " $"),
                                                 expand= c(0,0))+
                              theme(axis.text.x = element_blank())+
                              geom_text(label=Figure_15_tech_bold_FR,
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=200))

figure_16_data_FR <- read.csv("Graphs_data/Figure_16_tech.csv")
load("Graphs_data/Figure_16_tech_bold_FR.rdata")

translation_f16 <- translation %>%
  filter(str_detect(Figure, "Figure 16"))
figure_16_data_FR <- figure_16_data_FR%>%
  mutate(GEO =  case_when(
    str_detect(GEO, translation_f16$English[1]) ~ translation_f16$French[1],
    str_detect(GEO, translation_f16$English[2]) ~ translation_f16$French[2]))

figure_16_data_FR <- as.data.table(figure_16_data_FR)

figure.16 <- plot.column.dais(figure_16_data_FR, estimate, term ,label=FALSE, order.bar = 'ascending',  group.by= GEO,
                              plot.fig.num = "Figure 16",
                              plot.title= wrapper(graph.data[graph.data$Figure_number=="Figure 16",Figure_title_FR],70),
                              y.axis= graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 16",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(0,max(figure_16_data_FR$estimate)+800),
                     labels = unit_format(prefix = "", sep = " ", suffix = " $"),
                     expand= c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_text(label=Figure_16_tech_bold_FR,
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=100))

figure_17_data_FR <- read.csv("Graphs_data/Figure_17.csv")

translation_f17 <- translation %>%
  filter(str_detect(Figure, "Figure 17"))
figure_17_data_FR <- figure_17_data_FR%>%
  mutate(Country =  case_when(
    str_detect(Country, translation_f17$English[1]) ~ translation_f17$French[1],
    str_detect(Country, translation_f17$English[2]) ~ translation_f17$French[2]))%>%
  mutate(Compensation =  case_when(
    str_detect(Compensation, translation_f17$English[3]) ~ translation_f17$French[3],
    str_detect(Compensation, translation_f17$English[4]) ~ translation_f17$French[4],
    str_detect(Compensation, translation_f17$English[5]) ~ translation_f17$French[5]))

figure_17_data_FR <- as.data.table(figure_17_data_FR)

figure.17 <- plot.column.dais(figure_17_data_FR, Value, Country, group.by= Compensation ,label=TRUE, language = "FR",
                              plot.fig.num = "Figure 17",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 17",Figure_title_FR],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_FR],
                              caption = graph.data[graph.data$Figure_number=="Figure 17",Caption_FR],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              theme(axis.text.x = element_text(angle=0))

export.dais.plot("dais_graphs_FR/f1_FR.pdf",figure.1)
export.dais.plot("dais_graphs_FR/f2_FR.pdf",figure.2)
export.dais.plot("dais_graphs_FR/f3_FR.pdf",figure.3)
export.dais.plot("dais_graphs_FR/f4_FR.pdf",figure.4)
export.dais.plot("dais_graphs_FR/f5_FR.pdf",figure.5, p.width = 8)
export.dais.plot("dais_graphs_FR/f6_FR.pdf",figure.6, p.width = 8)
export.dais.plot("dais_graphs_FR/f7_FR.pdf",figure.7, p.height = 6, p.width = 8.5)
export.dais.plot("dais_graphs_FR/f8_FR.pdf",figure.8, p.height = 7, p.width = 8.5)
export.dais.plot("dais_graphs_FR/f9_FR.pdf",figure.9)
export.dais.plot("Dais_graphs_FR/f10_FR.pdf",figure.10)
export.dais.plot("dais_graphs_FR/f11_FR.pdf",figure.11)
export.dais.plot("dais_graphs_FR/f12_FR.pdf",figure.12)
export.dais.plot("dais_graphs_FR/f13_FR.pdf",figure.13, p.width = 7.5)
export.dais.plot("dais_graphs_FR/f14_FR.pdf",figure.14, p.width = 7.5)
export.dais.plot("dais_graphs_FR/f15_FR.pdf",figure.15)
export.dais.plot("dais_graphs_FR/f16_FR.pdf",figure.16)
export.dais.plot("dais_graphs_FR/f17_FR.pdf",figure.17)
