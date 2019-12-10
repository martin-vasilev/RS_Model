
#####################
#  Font size study: #
#####################

rm(list= ls())
load("data/Font_size.Rda")
load("data/L2_word_pos_FS.Rda")
RS<- subset(RS, !is.na(RS$prevChar))
RS<- subset(RS, prevVA>0)

source('functions/Return_sweeper1.R')

FontSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
FontSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
FontSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP")

Empir<- RS
Empir$M_launchDistVA<- Empir$launchDistVA      
Empir$M_landStartVA<- Empir$LandStartVA       
Empir$M_landStartLet<- Empir$LandStartLet        
Empir$M_UND<- Empir$undersweep_prob              
Empir$M_next_LP<- Empir$next_land_pos            
Empir$M_next_sacc_len<- Empir$next_sacc_deg

Empir$M_next_LP[which(Empir$M_UND==0)]<- NA
mean(Empir$M_next_LP, na.rm=T)

FontSim0$Model<- "Left margin"
FontSim1.5$Model<- "1.5 char."
FontSimOVP$Model<- 'W1 OVP'
Empir$Model<- 'Empirical'

FS<- rbind(FontSim0, FontSim1.5, FontSimOVP, Empir)

# get means:
library(reshape)

DesFS<- melt(FS, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
               measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
mFS<- cast(DesFS, Model ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= round(sd(x),3) ))

write.csv(mFS, 'Summary/Font_size_descr.csv')

#################################################################################################################

rm(list= ls())
# Bold study (OZ)

 load("data/Bold_OZ.Rda")
 RS<- RSb; rm(RSb)
 RS<- subset(RS, Condition=="Normal")
 RS<- subset(RS, !is.na(RS$prevChar))
 RS<- subset(RS, prevVA>0)
 load("data/L2_word_pos_FS.Rda")
 
 source('functions/Return_sweeper1.R')
 
 OZSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
 OZSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
 #OZSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP")
 
 
 Empir<- RS
 Empir$M_launchDistVA<- Empir$launchDistVA      
 Empir$M_landStartVA<- Empir$LandStartVA       
 Empir$M_landStartLet<- Empir$LandStartLet        
 Empir$M_UND<- Empir$undersweep_prob              
 Empir$M_next_LP<- Empir$next_land_pos            
 Empir$M_next_sacc_len<- Empir$next_sacc_deg
 
 Empir$M_next_LP[which(Empir$M_UND==0)]<- NA
 mean(Empir$M_next_LP, na.rm=T)
 
 OZSim0$Model<- "Left margin"
 OZSim1.5$Model<- "1.5 char."
 #OZSimOVP$Model<- 'W1 OVP'
 Empir$Model<- 'Empirical'
 
 OZ<- rbind(OZSim0, OZSim1.5, Empir)# , OZSimOVP
 
 DesOZ<- melt(OZ, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
              measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 mOZ<- cast(DesOZ, Model ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= round(sd(x),3) ))
 
 write.csv(mOZ, 'Summary/OZ_descr.csv')
 
 

##########################################################################################################
rm(list= ls()) 
 
 # Comprehension data
 load("data/Comprehension_data.Rda")
 RS<- RSc; rm(RSc)
 RS<- subset(RS, !is.na(RS$prevChar))
 RS<- subset(RS, prevVA>0)
 load("data/L2_word_pos_FS.Rda")
 
 
 source('functions/Return_sweeper1.R')
 
 CompSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
 CompSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
 #CompSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP")
 
 
 Empir<- RS
 Empir$M_launchDistVA<- Empir$launchDistVA      
 Empir$M_landStartVA<- Empir$LandStartVA       
 Empir$M_landStartLet<- Empir$LandStartLet        
 Empir$M_UND<- Empir$undersweep_prob              
 Empir$M_next_LP<- Empir$next_land_pos            
 Empir$M_next_sacc_len<- Empir$next_sacc_deg
 
 Empir$M_next_LP[which(Empir$M_UND==0)]<- NA
 mean(Empir$M_next_LP, na.rm=T)
 
 CompSim0$Model<- "Left margin"
 CompSim1.5$Model<- "1.5 char."
 #CompSimOVP$Model<- 'W1 OVP'
 Empir$Model<- 'Empirical'
 
 Comp<- rbind(CompSim0, CompSim1.5, Empir)# , CompSimOVP
 
 DesComp<- melt(Comp, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
              measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 mComp<- cast(DesComp, Model ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= round(sd(x),3) ))
 
 write.csv(mComp, 'Summary/Comprehension_descr.csv')
 
