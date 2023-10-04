library(DaisTheme)
library(ggplot2)
library(data.table)
library(ggpp)
library(scales)

#########################################################
#Load in graph spread sheets
graph.data <-  fread("Graphs_data/Graphs_spreadsheet.csv")

figure_1_data <- fread("Graphs_data/Figure_1.csv")
figure.1 <- plot.column.dais(figure_1_data, m, tech_job, group.by = GEO, order.bar="ascending", label=TRUE,
                           plot.fig.num = "Figure 1",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 1",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 1",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GEO, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())


figure_2_data <- fread("Graphs_data/Figure_2.csv")
figure.2 <- plot.column.dais(figure_2_data, m, tech_job, group.by = GEO, label=TRUE,
                           plot.fig.num = "Figure 2",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 2",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 2",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 2",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GEO, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())


figure_3_data <- fread("Graphs_data/Figure_3.csv")
figure.3 <- plot.column.dais(figure_3_data, m, tech_job, group.by = GENDER, label=TRUE,
                           plot.fig.num = "Figure 3",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 3",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 3",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 3",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GENDER, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_4_data <- fread("Graphs_data/Figure_4.csv")
figure.4 <- plot.column.dais(figure_4_data, m, tech_job, group.by = GENDER,  label=TRUE,
                           plot.fig.num = "Figure 4",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 4",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 4",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 4",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(GENDER, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())


figure_5_data <- fread("Graphs_data/Figure_5.csv")

figure.5 <- plot.column.dais(figure_5_data, m, tech_job, group.by = EDU, order.bar="ascending", label=TRUE,
                           plot.fig.num = "Figure 5",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 5",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 5",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 5",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           aes(x = reorder(EDU, m))+facet_grid(.~tech_job, switch = "x")+
                           theme(axis.text.x = element_blank())

figure_6_data <- fread("Graphs_data/Figure_6.csv")

figure.6 <-plot.column.dais(figure_6_data, m, tech_job, group.by = EDU, order.bar="ascending", label=TRUE,
                            plot.fig.num = "Figure 6",
                            plot.title= graph.data[graph.data$Figure_number=="Figure 6",Figure_title],
                            y.axis= graph.data[graph.data$Figure_number=="Figure 6",Y_Axis],
                            caption = graph.data[graph.data$Figure_number=="Figure 6",Caption],
                            label.unit = graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks],
                            label.adjust = 0.025)+
                            aes(x = reorder(EDU, m))+facet_grid(.~tech_job, switch = "x")+
                            theme(axis.text.x = element_blank())


figure_7_data <- fread("Graphs_data/Figure_7_tech.csv")
figure.7 <- plot.column.dais(figure_7_data, INC, Visible_Minority,order.bar = 'ascending', label=TRUE,
                             plot.fig.num = "Figure 7",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 7",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 7",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 7",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks],
                             label.adjust = 0.025)+
                             theme(axis.text.x = element_text(angle = 30, vjust=1))


figure_8_data <- fread("Graphs_data/Figure_8_tech.csv")
figure.8 <- plot.column.dais(figure_8_data,m, RACE, label=TRUE, order.bar="ascending", 
                           plot.fig.num = "Figure 8",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 8",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 8",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 8",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           theme(axis.text.x = element_text(angle = 60, vjust=1))


figure_9_data <- fread("Graphs_data/Figure_9.csv")
load("Graphs_data/Figure_9_bold.rdata")

figure.9 <- plot.column.dais(figure_9_data, estimate, term ,label=FALSE, group.by = tech, order.bar = "ascending",
                           plot.fig.num = "Figure 9",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 9",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 9",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 9",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks],
                           label.adjust = 0.025)+
                           theme(axis.text.x = element_blank())+
                           geom_text(label=Figure_9_bold[c(2,1)],
                                     parse=TRUE,
                                     position=position_dodgenudge(width=0.6,y=700), 
                                     vjust=0.6)

figure_10_data <- fread("Graphs_data/Figure_10.csv")
load("Graphs_data/Figure_10_bold.rdata")
load("Graphs_data/Figure_10_signs.rdata")

figure.10 <- plot.column.dais(figure_10_data, estimate, tech ,label=FALSE, group.by = GEO,
                              plot.fig.num = "Figure 10",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 10",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 10",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 10",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              scale_y_continuous(limits=c(min(figure_10_data$estimate)-200,0),labels=dollar_format())+
                              geom_text(label=Figure_10_bold,
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=-20),
                                        vjust=Figure_10_signs)+
                              theme(axis.text.x = element_text(angle = 0))

figure_11_data <- fread("Graphs_data/Figure_11.csv")
load("Graphs_data/Figure_11_bold.rdata")
load("Graphs_data/Figure_11_signs.rdata")

figure.11 <- plot.column.dais(figure_11_data, estimate, tech ,label=FALSE, group.by = term, 
                             plot.fig.num = "Figure 11",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 11",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 11",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 11",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks],
                             label.adjust = 0.025)+
                             aes(x = reorder(term, estimate))+facet_grid(.~tech, switch = "x")+
                             scale_y_continuous(limits=c(min(figure_11_data$estimate)-1000,max(figure_11_data$estimate)+800),
                                                labels=dollar_format())+
                             geom_text(label=Figure_11_bold,
                                      parse=TRUE,
                                      position=position_dodgenudge(width=0.6,y=700),
                                      vjust=Figure_11_signs)+
                             theme(axis.text.x = element_blank())

figure_12_data <- fread("Graphs_data/Figure_12.csv")
load("Graphs_data/Figure_12_bold.rdata")

figure.12 <- plot.column.dais(figure_12_data, estimate, tech ,label=FALSE, group.by = term, 
                              plot.fig.num = "Figure 12",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 12",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 12",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 12",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 12",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              aes(x = reorder(term, estimate))+facet_grid(.~tech, switch = "x")+
                              geom_text(label=Figure_12_bold,
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=2000))+
                              theme(axis.text.x = element_blank())

figure_13_data <- fread("Graphs_data/Figure_13_tech.csv")
load("Graphs_data/Figure_13_tech_bold.rdata")
load("Graphs_data/Figure_13_tech_signs.rdata")

figure.13 <- plot.column.dais(figure_13_data, estimate, term ,label=FALSE, order.bar = 'ascending',
                              plot.fig.num = "Figure 13",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 13",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 13",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 13",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 13",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(min(figure_13_data$estimate)-1000,max(figure_13_data$estimate)+1000),labels=dollar_format())+
  geom_text(label=Figure_13_tech_bold,
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=700),
            vjust=Figure_13_tech_signs)+
  theme(axis.text.x = element_text(angle=30, vjust=1))

figure_14_data <- fread("Graphs_data/Figure_14_tech.csv")
load("Graphs_data/Figure_14_tech_bold.rdata")
load("Graphs_data/Figure_14_tech_signs.rdata")

figure.14 <- plot.column.dais(figure_14_data, estimate, term ,label=FALSE, order.bar = 'ascending',
                              plot.fig.num = "Figure 14",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 14",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 14",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 14",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(min(figure_14_data$estimate)-1000,max(figure_14_data$estimate)+1000),labels=dollar_format())+
  geom_text(label=Figure_14_tech_bold,
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=800),
            vjust=Figure_14_tech_signs)+
  theme(axis.text.x = element_text(angle=30, vjust=1))

figure_15_data <- fread("Graphs_data/Figure_15_tech.csv")
load("Graphs_data/Figure_15_tech_bold.rdata")

figure.15 <- plot.column.dais(figure_15_data, estimate, term ,label=FALSE, order.bar = 'ascending',  group.by= GEO,
                              plot.fig.num = "Figure 15",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 15",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 15",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 15",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              scale_y_continuous(limits=c(0,max(figure_15_data$estimate)+800), labels=dollar_format(),expand= c(0,0))+
                              theme(axis.text.x = element_blank())+
                              geom_text(label=Figure_15_tech_bold,
                                        parse=TRUE,
                                        position=position_dodgenudge(width=0.6,y=200))

figure_16_data <- fread("Graphs_data/Figure_16_tech.csv")
load("Graphs_data/Figure_16_tech_bold.rdata")

figure.16 <- plot.column.dais(figure_16_data, estimate, term ,label=FALSE, order.bar = 'ascending',  group.by= GEO,
                              plot.fig.num = "Figure 16",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 16",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 16",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 16",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks],
                              label.adjust = 0.025)+
  scale_y_continuous(limits=c(0,max(figure_16_data$estimate)+800), labels=dollar_format(), expand= c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_text(label=Figure_16_tech_bold,
            parse=TRUE,
            position=position_dodgenudge(width=0.6,y=100))

figure_17_data <- fread("Graphs_data/Figure_17.csv")

figure.17 <- plot.column.dais(figure_17_data, Value, Country, group.by= Compensation ,label=TRUE, 
                              plot.fig.num = "Figure 17",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 17",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 17",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 17",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks],
                              label.adjust = 0.025)+
                              theme(axis.text.x = element_text(angle=0))

export.dais.plot("Dais_graphs/f1.pdf",figure.1)
export.dais.plot("Dais_graphs/f2.pdf",figure.2)
export.dais.plot("Dais_graphs/f3.pdf",figure.3)
export.dais.plot("Dais_graphs/f4.pdf",figure.4)
export.dais.plot("Dais_graphs/f5.pdf",figure.5)
export.dais.plot("Dais_graphs/f6.pdf",figure.6)
export.dais.plot("Dais_graphs/f7.pdf",figure.7, p.height = 6, p.width = 8.5)
export.dais.plot("Dais_graphs/f8.pdf",figure.8, p.height = 7, p.width = 8.5)
export.dais.plot("Dais_graphs/f9.pdf",figure.9)
export.dais.plot("Dais_graphs/f10.pdf",figure.10)
export.dais.plot("Dais_graphs/f11.pdf",figure.11)
export.dais.plot("Dais_graphs/f12.pdf",figure.12)
export.dais.plot("Dais_graphs/f13.pdf",figure.13)
export.dais.plot("Dais_graphs/f14.pdf",figure.14)
export.dais.plot("Dais_graphs/f15.pdf",figure.15)
export.dais.plot("Dais_graphs/f16.pdf",figure.16)
export.dais.plot("Dais_graphs/f17.pdf",figure.17)
  
