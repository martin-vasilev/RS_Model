
# Abbreviations during paragraph reading:

rm(list= ls())

load("data/prep/Abbrev_data.Rda")
x_offset<- 500
#sent <- read.delim("D:/R/RS_Model/data/prep/sent.txt")

### Add some additional stuff:
nsubs<- unique(raw_fix$sub)

new<- NULL
for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_fix, sub== nsubs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){ # for each item..
    m<- subset(n, item== nitems[j])
    m$prevX<- NA
    m$prevY<- NA
    m$prevChar<- NA
    m$prev_max_char_line<- NA
    m$prev_fix_dur<- NA
    m$nextX<- NA
    
    for(k in 1:nrow(m)){ # for each fixation
      if(k>1){
          m$prevX[k]<- m$xPos[k-1]
          m$prevY[k]<- m$yPos[k-1]
          m$prevChar[k]<- m$char_line[k-1]
          m$prev_max_char_line[k]<- m$max_char_line[k-1]
          m$prev_fix_dur[k]<- m$fix_dur[k-1]
      }
      
      if(k< nrow(m)){ # next sacc
        m$nextX[k]<- m$xPos[k+1]
      }
      
    } # end of k
    
    new<- rbind(new, m)
    
  } # end of j
  cat(i); cat(' ')
} # end of i

raw_fix<- new; rm(new)



RSa<-subset(raw_fix, Rtn_sweep==1)
RSa$Condition<- ifelse(is.element(RSa$cond, c(1,3)), "Silent", "Aloud") # add condition type
RSa$VA<- 0.31998 # visual angle in experiment (equivalent to "small font" in the font size paper)
RSa$x_offset<- x_offset

RSa<- subset(RSa, Condition=='Silent')

# remove outliers:
out<- which(RSa$fix_dur<40 |RSa$fix_dur>1000)
RSa<- RSa[-out,]

# remove blinks:
RSa<- subset(RSa, prev_blink==0 & after_blink==0 & blink==0)


# remove useless columns:
RSa$hasText<- NULL
RSa$time_since_start<- NULL
RSa$outOfBnds<- NULL
RSa$blink<- NULL
RSa$prev_blink<- NULL
RSa$after_blink<- NULL
RSa$Rtn_sweep<- NULL

RSa$undersweep_prob<- ifelse(RSa$Rtn_sweep_type== "undersweep", 1,0)

#library(reshape)
#Des<- melt(RSa, id=c('sub', 'item', 'Condition'), 
#            measure=c("char_line", "undersweep_prob") , na.rm=TRUE)

#m<- cast(Des, Condition ~ variable
#          , function(x) c(M=signif(mean(x),3)
#                          , SD= sd(x) ))


# add landing position relative to line start (in letters):
RSa$LandStartLet<- RSa$char_line

# landing position relative to line start (in degrees per visual angle)

DPP<- 0.02461393513610085 # degree per pixel in the experiment

RSa$LandStartVA<- (RSa$xPos - RSa$x_offset)*DPP

# code (absolute) launch site distance in letters:
RSa$launchDistLet<- abs(RSa$char_line- RSa$prevChar)

# code (absolute) launch site distance in visual angle:
RSa$launchDistVA<- abs(RSa$xPos- RSa$prevX)*DPP


# recode saccade length:
RSa$sacc_len<- abs(RSa$char_line- RSa$prevChar)



### fix NA character landing positions (outside text):
a<- which(is.na(RSa$LandStartLet))
RSa$LandStartLet[a]<- ceiling(RSa$LandStartVA[a]/0.31998)

# Fix launch site distance
a<- which(is.na(RSa$launchDistLet))
RSa$launchDistLet[a]<- ceiling(RSa$launchDistVA[a]/0.31998)

# prevChar NAs:
a<- which(is.na(RSa$prevChar))
RSa$prevChar[a]<- ceiling((RSa$prevX[a]- RSa$x_offset[a])/13)


RSa$next_sacc<- abs(RSa$nextX - RSa$xPos)
RSa$next_sacc_deg<- NA
RSa$next_sacc_let<- NA
RSa$next_land_pos<- NA
RSa$next_land_let<- NA

for(i in 1:nrow(RSa)){
    RSa$next_sacc_deg[i]<- (RSa$next_sacc[i]/ 13)*0.31998
    RSa$next_sacc_let[i]<- ceiling(RSa$next_sacc[i]/ 13)
    RSa$next_land_pos[i]<- ((RSa$nextX[i] -RSa$x_offset[i])/13)*0.31998
    RSa$next_land_let[i]<- ceiling((RSa$nextX[i] -RSa$x_offset[i])/13)
}


RSa$prevVA<- (RSa$prevX- RSa$x_offset)*DPP


RSa$x_offset<- NULL

########

# RS$W1_start<- 1
# RS$W1_end<- NA
# 
# RS$W2_start<- NA
# RS$W2_end<- NA
# 
# RS$W3_start<- NA
# RS$W3_end<- NA
# 
# sent$Var6<- as.character(sent$Var6)
# 
# for(i in 1:nrow(RS)){
#   string<- sent$Var6[which(sent$Var1== RS$item[i])][1]
#   words<- unlist(strsplit(string, ' ')) 
#   
#   W1<- words[1]
#   W2<- words[2]
#   W3<- words[3]
#   
#   
#   # W1:
#   RS$W1_end[i]<- nchar(W1)
#   
#   # W2:
#   RS$W2_start[i]<- RS$W1_end[i]+1 +1 # +1 to add empty space 
#   RS$W2_end[i]<- RS$W2_start[i] + nchar(W2)-1
#   
#   # W3
#   RS$W3_start[i]<- RS$W2_end[i]+1 +1
#   RS$W3_end[i]<- RS$W3_start[i] + nchar(W3)-1
# }

save(RSa, file= 'data/Abbrev_data.Rda')
write.csv(RSa, 'data/Abbrev_data.csv')



####################################################################################################

# rm(list= ls())
# 
# sent <- read.delim("D:/R/RS_Model/data/prep/sent.txt")
# sent<- subset(sent, Var2==1)
# 
# word_pos<- list()
# sent$Var5<- as.character(sent$Var5)
# sent$Var6<- as.character(sent$Var6)
# 
# for(i in 1:nrow(sent)){
#   L2<- sent$Var6[i]
#   spaces<- unlist(gregexpr(' ', L2))
#   
#   word_pos[[toString(i)]]<- spaces
# }
# 
# save(word_pos, file= 'data/L2_word_pos_FS.Rda')
# 
