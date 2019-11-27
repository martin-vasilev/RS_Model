
rm(list= ls())

load("results/RS2.Rda")


# conditions:
RS$line_len= factor(ifelse(RS$cond==1| RS$cond==2, 1,2),labels = c("short line", "long line"))
RS$font_size= factor(ifelse(RS$cond==1| RS$cond==3, 1,2), labels = c("small font", "big font"))


### Line length effect:

library(reshape)
#mean land_pos in visual angle per condition 
LP1<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("LandStartVA", "M_landStartVA"), na.rm=TRUE)
mLP1<- cast(LP1, line_len ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

db<- data.frame(line_len= c(as.character(mLP1$line_len), as.character(mLP1$line_len)),
                Mean= c(mLP1$LandStartVA_M, mLP1$M_landStartVA_M),
                SD= c(mLP1$LandStartVA_SD, mLP1$M_landStartVA_SD),
                Type= c(rep('  Data   ', 2), rep('  Model', 2)))
db$SE<- db$SD/sqrt(64)
db$upper<- db$Mean + db$SE
db$lower<- db$Mean - db$SE


pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

library(ggplot2)

G1<- ggplot(db, aes(x= line_len, y= Mean, ymax= upper, ymin= lower, group= Type, color= Type, 
                    fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Line length", y= "Landing position (deg)", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 3)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.8, 0.8), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G1

ggsave(plot = G1, filename = 'plots/Line_length2.pdf', width = 9, height = 7)




### Font size effect:

library(reshape)
#mean land_pos in visual angle per condition 
LP2<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("LandStartVA", "M_landStartVA"), na.rm=TRUE)
mLP2<- cast(LP2, font_size ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

db2<- data.frame(font_size= c(as.character(mLP2$font_size), as.character(mLP2$font_size)),
                Mean= c(mLP2$LandStartVA_M, mLP2$M_landStartVA_M),
                SD= c(mLP2$LandStartVA_SD, mLP2$M_landStartVA_SD),
                Type= c(rep('  Data', 2), rep('  Model', 2)))
db2$SE<- db2$SD/sqrt(64)
db2$upper<- db2$Mean + db$SE
db2$lower<- db2$Mean - db$SE



G2<- ggplot(db2, aes(x= font_size, y= Mean, ymax= upper, ymin= lower, group= Type, color= Type, 
                    fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Font size", y= "", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 3)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.8, 0.8), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G2

ggsave(plot = G2, filename = 'plots/font_size2.pdf', width = 9, height = 7)



## Launch site effect:

db3<- RS[,c('font_size', 'line_len')]
db3<- rbind(db3, db3)
db3$launch<- c(RS$launchSiteVA, RS$launchSiteVA)
db3$Mean<- c(RS$LandStartVA, RS$M_landStartVA)
db3$Type<- c(rep("Data", nrow(RS)), rep("Model", nrow(RS)))
db3$launchRnd<- round(db3$launch)
a= which(db3$launchRnd>11)
db3$launchRnd<- as.character(db3$launchRnd)
db3$launchRnd[a]<- "12+"


#mean land_pos in visual angle per condition 
LS<- melt(db3, id=c('font_size', 'line_len', 'launchRnd', 'Type'), 
           measure=c("Mean"), na.rm=TRUE)
mLS<- cast(LS, launchRnd+Type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


mLS$launchRnd<- as.factor(mLS$launchRnd)
levels(mLS$launchRnd)
mLS$launchRnd<- factor(mLS$launchRnd, 
                       levels= c("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                 "11", "12+"))
levels(mLS$launchRnd)

mLS$SE<- mLS$Mean_SD/sqrt(64)
mLS$upper<- mLS$Mean_M + mLS$SE
mLS$lower<- mLS$Mean_M - mLS$SE

G3<- ggplot(mLS, aes(x= launchRnd, y= Mean_M, ymax= upper, ymin= lower, group= Type, color= Type, 
                     fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Launch site (in deg)", y= "", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 3)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.88, 0.88), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G3

ggsave(plot = G3, filename = 'plots/launch2.pdf', width = 9, height = 7)



# merge plots:
library(ggpubr)

figure <- ggarrange(G1, G2, G3, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

figure<- annotate_figure(figure,
                top = text_grob("Model saccade target: left margin", color = "black", face = "bold", size = 18))
                # bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
                #                    hjust = 1, x = 1, face = "italic", size = 10),
                # left = text_grob("Tooth length", color = "green", rot = 90),
                # right = "I'm done, thanks :-)!",
                # fig.lab = "Figure 1", fig.lab.face = "bold")

ggsave(filename = 'Plots/M2_merged.pdf', plot = figure, width = 18, height = 7)


#########################################################################################################################
#                                              UNDERSWEEEP
#########################################################################################################################

#rm(list= ls())

#load("results/RS.Rda")


# conditions:
#RS$line_len= factor(ifelse(RS$cond==1| RS$cond==2, 1,2),labels = c("short line", "long line"))
#RS$font_size= factor(ifelse(RS$cond==1| RS$cond==3, 1,2), labels = c("small font", "big font"))


### Line length effect:

library(reshape)
#mean land_pos in visual angle per condition 
UP1<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("undersweep_prob", "M_UND"), na.rm=TRUE)
mUP1<- cast(UP1, line_len ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

db<- data.frame(line_len= c(as.character(mUP1$line_len), as.character(mUP1$line_len)),
                Mean= c(mUP1$undersweep_prob_M, mUP1$M_UND_M),
                SD= c(mUP1$undersweep_prob_SD, mUP1$M_UND_SD),
                Type= c(rep('  Data   ', 2), rep('  Model', 2)))
db$SE<- db$SD/sqrt(64)
db$upper<- db$Mean + db$SE
db$lower<- db$Mean - db$SE


pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

library(ggplot2)

U1<- ggplot(db, aes(x= line_len, y= Mean, ymax= upper, ymin= lower, group= Type, color= Type, 
                    fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Line length", y= "Corrective saccade probability", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 1)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.8, 0.8), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G1

ggsave(plot = U1, filename = 'plots/USPLine_length2.pdf', width = 9, height = 7)




### Font size effect:

library(reshape)
#mean land_pos in visual angle per condition 
UP2<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("undersweep_prob", "M_UND"), na.rm=TRUE)
mUP2<- cast(UP2, font_size ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

db2<- data.frame(font_size= c(as.character(mUP2$font_size), as.character(mUP2$font_size)),
                 Mean= c(mUP2$undersweep_prob_M, mUP2$M_UND_M),
                 SD= c(mUP2$undersweep_prob_SD, mUP2$M_UND_SD),
                 Type= c(rep('  Data', 2), rep('  Model', 2)))
db2$SE<- db2$SD/sqrt(64)
db2$upper<- db2$Mean + db$SE
db2$lower<- db2$Mean - db$SE



U2<- ggplot(db2, aes(x= font_size, y= Mean, ymax= upper, ymin= lower, group= Type, color= Type, 
                     fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Font size", y= "", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 1)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.8, 0.8), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G2

ggsave(plot = U2, filename = 'plots/USPfont_size2.pdf', width = 9, height = 7)



## Launch site effect:

db3<- RS[,c('font_size', 'line_len')]
db3<- rbind(db3, db3)
db3$launch<- c(RS$launchSiteVA, RS$launchSiteVA)
db3$Mean<- c(RS$undersweep_prob, RS$M_UND)
db3$Type<- c(rep("Data", nrow(RS)), rep("Model", nrow(RS)))
db3$launchRnd<- round(db3$launch)
a= which(db3$launchRnd>11)
db3$launchRnd<- as.character(db3$launchRnd)
db3$launchRnd[a]<- "12+"


#mean land_pos in visual angle per condition 
LS<- melt(db3, id=c('font_size', 'line_len', 'launchRnd', 'Type'), 
          measure=c("Mean"), na.rm=TRUE)
mLS<- cast(LS, launchRnd+Type ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


mLS$launchRnd<- as.factor(mLS$launchRnd)
levels(mLS$launchRnd)
mLS$launchRnd<- factor(mLS$launchRnd, 
                       levels= c("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                 "11", "12+"))
levels(mLS$launchRnd)

mLS$SE<- mLS$Mean_SD/sqrt(64)
mLS$upper<- mLS$Mean_M + mLS$SE
mLS$lower<- mLS$Mean_M - mLS$SE

U3<- ggplot(mLS, aes(x= launchRnd, y= Mean_M, ymax= upper, ymin= lower, group= Type, color= Type, 
                     fill= Type, linetype= Type, shape= Type)) + theme_bw (22)+
  geom_line(size= 1.3)+ geom_point(size=4)+
  labs(x= "Launch site (in deg)", y= "", 
       color= "", shape= '', linetype= '', fill= '') +
  ggtitle("")+ ylim(0, 1)+
  geom_ribbon(alpha= 0.1, color= NA) + 
  theme(legend.position = c(0.88, 0.88), legend.title=element_blank(),
        legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2]))+
  scale_x_discrete(expand = c(0.1,0.1)); G3

ggsave(plot = U3, filename = 'plots/USPlaunch2.pdf', width = 9, height = 7)



# merge plots:
library(ggpubr)

figure <- ggarrange(U1, U2, U3, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

figure<- annotate_figure(figure,
                         top = text_grob("Model saccade target: 2nd character", color = "black", face = "bold", size = 18))
# bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
#                    hjust = 1, x = 1, face = "italic", size = 10),
# left = text_grob("Tooth length", color = "green", rot = 90),
# right = "I'm done, thanks :-)!",
# fig.lab = "Figure 1", fig.lab.face = "bold")

ggsave(filename = 'Plots/M2_USPmerged.pdf', plot = figure, width = 18, height = 7)


#################################
#         LP + USP              #
#################################

figure2 <- ggarrange(G1, G2, G3, U1, U2, U3, ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom", align = "hv")
figure2<- annotate_figure(figure2,
                         top = text_grob("Model saccade target: 2nd character", color = "black", face = "bold", size = 18))
ggsave(filename = 'Plots/M2_USPmerged.png', plot = figure2, width = 16, height = 12)
