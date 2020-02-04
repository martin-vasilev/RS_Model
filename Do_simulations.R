

#####################
#  Font size study: #
#####################

rm(list= ls())
load("data/Font_size.Rda")
load("data/word_pos_FS.Rda")
RS<- subset(RS, !is.na(RS$prevChar))
RS<- subset(RS, prevVA>0)

source('functions/Return_sweeper1.R')

FontSim0<- Return_sweeper1(RS, word_pos, RS_target = 0, nsim= 10)
FontSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5, nsim= 10)
FontSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP", nsim= 10)

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

FS$Dataset= "Font size"
save(FS, file= 'results/raw/FS.Rda')
write.csv(FS, 'results/raw/FS.csv')


#################################################################################################################

rm(list= ls())
# Bold study (OZ)

 load("data/Bold_OZ.Rda")
 RS<- RSb; rm(RSb)
 RS<- subset(RS, Condition=="Normal")
 RS<- subset(RS, !is.na(RS$prevChar))
 RS<- subset(RS, prevVA>0)
 load("data/word_pos_OZ.Rda")
 
 source('functions/Return_sweeper1.R')
 
 OZSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
 OZSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
 OZSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP")
 
 
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
 OZSimOVP$Model<- 'W1 OVP'
 Empir$Model<- 'Empirical'
 
 OZ<- rbind(OZSim0, OZSim1.5, OZSimOVP, Empir)
 
 DesOZ<- melt(OZ, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
              measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 mOZ<- cast(DesOZ, Model ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= round(sd(x),3) ))
 
 write.csv(mOZ, 'Summary/OZ_descr.csv')
 
 OZ$Dataset= "OZ (Normal)"
 save(OZ, file= 'results/raw/OZ.Rda')
 write.csv(OZ, 'results/raw/OZ.csv') 

##########################################################################################################
rm(list= ls()) 
 
 # Comprehension data
 load("data/Comprehension_data.Rda")
 RS<- RSc; rm(RSc)
 RS<- subset(RS, !is.na(RS$prevChar))
 RS<- subset(RS, prevVA>0)
 load("data/word_pos_Comprehension.Rda")
 
 
 source('functions/Return_sweeper1.R')
 
 CompSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
 CompSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
 CompSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP")
 
 
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
 CompSimOVP$Model<- 'W1 OVP'
 Empir$Model<- 'Empirical'
 
 Comp<- rbind(CompSim0, CompSim1.5, CompSimOVP,  Empir)
 
 DesComp<- melt(Comp, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
              measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 mComp<- cast(DesComp, Model ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= round(sd(x),3) ))
 
 write.csv(mComp, 'Summary/Comprehension_descr.csv')

 
 Comp$Dataset= "Comprehension items"
 save(Comp, file= 'results/raw/Comp.Rda')
 write.csv(Comp, 'results/raw/Comp.csv')
 
 ######################################################
 rm(list= ls()) 
 
 # Abbreviation data data
 load("data/Abbrev_data.Rda")
 RS<- RSa; rm(RSa)
 RS<- subset(RS, !is.na(RS$prevChar))
 RS<- subset(RS, prevVA>0)
 load("data/word_pos_Abbrev.Rda")
 
 source('functions/Return_sweeper1.R')
 

 AbbrevSim0<- Return_sweeper1(RS, word_pos, RS_target = 0)
 AbbrevSim1.5<- Return_sweeper1(RS, word_pos, RS_target = 1.5)
 AbbrevSimOVP<- Return_sweeper1(RS, word_pos, RS_target = "OVP", diff_cond = TRUE)
 
 
 Empir<- RS
 Empir$M_launchDistVA<- Empir$launchDistVA      
 Empir$M_landStartVA<- Empir$LandStartVA       
 Empir$M_landStartLet<- Empir$LandStartLet        
 Empir$M_UND<- Empir$undersweep_prob              
 Empir$M_next_LP<- Empir$next_land_pos            
 Empir$M_next_sacc_len<- Empir$next_sacc_deg
 
 Empir$M_next_LP[which(Empir$M_UND==0)]<- NA
 mean(Empir$M_next_LP, na.rm=T)
 
 AbbrevSim0$Model<- "Left margin"
 AbbrevSim1.5$Model<- "1.5 char."
 AbbrevSimOVP$Model<- 'W1 OVP'
 Empir$Model<- 'Empirical'
 
 Abbrev<- rbind(AbbrevSim0, AbbrevSim1.5, AbbrevSimOVP, Empir)
 
 DesAbbrev<- melt(Abbrev, id=c('sub', 'item', 'cond', 'LandStartLet', 'Model'), 
                measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 mAbbrev<- cast(DesAbbrev, Model ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= round(sd(x),3) ))
 
 write.csv(mAbbrev, 'Summary/Abbreviation_descr.csv')
 
 Abbrev$Dataset= "Abbreviations (Silent)"
 save(Abbrev, file= 'results/raw/Abbrev.Rda')
 write.csv(Abbrev, 'results/raw/Abbrev.csv')
 
 
 # merge all results in a single data frame:
 
 rm(list= ls())

 load("results/raw/FS.Rda")
 load("results/raw/OZ.Rda")
 load("results/raw/Comp.Rda")
 load("results/raw/Abbrev.Rda")

 All<- rbind(FS, OZ, Comp, Abbrev) 
 
 
 # #####################
 # #       Provo:      #
 # #####################
 # 
 # rm(list= ls())
 # load("data/provo.Rda")
 # load("data/word_pos_FS.Rda")
 # #provo<- subset(provo, !is.na(provo$prevChar))
 # provo<- subset(provo, prevVA>0)
 # provo<- subset(provo, LandStartVA<15)
 # 
 # source('functions/Return_sweeper1.R')
 # 
 # ProvoSim0<- Return_sweeper1(provo, word_pos, RS_target = 0)
 # ProvoSim1.5<- Return_sweeper1(provo, word_pos, RS_target = 2.5)
 # ProvoSimOVP<- Return_sweeper1(provo, word_pos, RS_target = "OVP")
 # 
 # Empir<- provo
 # Empir$M_launchDistVA<- Empir$launchDistVA      
 # Empir$M_landStartVA<- Empir$LandStartVA       
 # Empir$M_landStartLet<- Empir$LandStartLet        
 # Empir$M_UND<- Empir$undersweep_prob              
 # Empir$M_next_LP<- Empir$next_land_pos            
 # Empir$M_next_sacc_len<- Empir$next_sacc_deg
 # 
 # Empir$M_next_LP[which(Empir$M_UND==0)]<- NA
 # mean(Empir$M_next_LP, na.rm=T)
 # 
 # ProvoSim0$Model<- "Left margin"
 # ProvoSim1.5$Model<- "1.5 char."
 # ProvoSimOVP$Model<- 'W1 OVP'
 # Empir$Model<- 'Empirical'
 # 
 # PR<- rbind(ProvoSim0, ProvoSim1.5)
 # 
 # # get means:
 # library(reshape)
 # 
 # DesPR<- melt(PR, id=c('sub', 'item', 'Model'), 
 #              measure=c("M_landStartVA", 'M_UND', 'M_launchDistVA', 'M_next_LP'), na.rm=TRUE)
 # mPR<- cast(DesPR, Model ~ variable
 #            ,function(x) c(M=signif(mean(x),3)
 #                           , SD= round(sd(x),3) ))
