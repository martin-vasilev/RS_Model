
# Investigation of return-sweep fixations and word processing
# data from:   Slattery, T.J. & Parker, A.J. Psychon Bull Rev (2019) 26: 1948. 
#  https://doi.org/10.3758/s13423-019-01636-3 

# Please note that I only have access to 92 of the subjects tested in the USA (not the whole sample used in the paper)


# x-offset is 600;
# Pixel per letter (ppl) was 10; deg= 0.3125

rm(list= ls())


load("data/prep/Compr_data_raw.Rda")
x_offset<- 600

### Add some additional stuff:
nsubs<- unique(raw_data$sub)

new<- NULL
for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_data, sub== nsubs[i])
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

raw_data<- new; rm(new)



RSc<-subset(raw_data, Rtn_sweep==1)
RSc$VA<- 0.3125 # visual angle in experiment (equivalent to "small font" in the font size paper)

# remove outliers:
out<- which(RSc$fix_dur<80 |RSc$fix_dur>1000)
RSc<- RSc[-out,]

# remove blinks:
RSc<- subset(RSc, prev_blink==0 & after_blink==0 & blink==0)


# remove useless columns:
RSc$hasText<- NULL
RSc$time_since_start<- NULL
RSc$outOfBnds<- NULL
RSc$blink<- NULL
RSc$prev_blink<- NULL
RSc$after_blink<- NULL
RSc$Rtn_sweep<- NULL

RSc$undersweep_prob<- ifelse(RSc$Rtn_sweep_type== "undersweep", 1,0)

library(reshape)
Des<- melt(RSc, id=c('sub', 'item'), 
            measure=c("char_line", "undersweep_prob") , na.rm=TRUE)

m<- cast(Des, sub ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))



# add landing position relative to line start (in letters):
RSc$LandStartLet<- RSc$char_line

# landing position relative to line start (in degrees per visual angle)

DPP<- 0.03125 # degree per pixel in the experiment
RSc$x_offset<- x_offset

RSc$LandStartVA<- (RSc$xPos - RSc$x_offset)*DPP

# code (absolute) launch site distance in letters:
RSc$launchDistLet<- abs(RSc$char_line- RSc$prevChar)

# code (absolute) launch site distance in visual angle:
RSc$launchDistVA<- abs(RSc$xPos- RSc$prevX)*DPP


# recode saccade length:
RSc$sacc_len<- abs(RSc$char_line- RSc$prevChar)



### fix NA character landing positions (outside text):
a<- which(is.na(RSc$LandStartLet))
RSc$LandStartLet[a]<- ceiling(RSc$LandStartVA[a]/0.3125)

# Fix launch site distance
a<- which(is.na(RSc$launchDistLet))
RSc$launchDistLet[a]<- ceiling(RSc$launchDistVA[a]/0.3125)

# prevChar NAs:
a<- which(is.na(RSc$prevChar))
RSc$prevChar[a]<- ceiling((RSc$prevX[a]- RSc$x_offset[a])/10)


RSc$next_sacc<- abs(RSc$nextX - RSc$xPos)
RSc$next_sacc_deg<- NA
RSc$next_sacc_let<- NA
RSc$next_land_pos<- NA
RSc$next_land_let<- NA

for(i in 1:nrow(RSc)){
    RSc$next_sacc_deg[i]<- (RSc$next_sacc[i]/ 10)*0.3125
    RSc$next_sacc_let[i]<- ceiling(RSc$next_sacc[i]/ 10)
    RSc$next_land_pos[i]<- ((RSc$nextX[i] -RSc$x_offset[i])/10)*0.3125
    RSc$next_land_let[i]<- ceiling((RSc$nextX[i] -RSc$x_offset[i])/10)
}


RSc$prevVA<- (RSc$prevX- RSc$x_offset)*DPP


RSc$x_offset<- NULL

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

save(RSc, file= 'data/Comprehension_data.Rda')
write.csv(RSc, 'data/Comprehension_data.csv')



####################################################################################################

rm(list= ls())

sent <- readLines("data/prep/Comprehension_texts.txt")

word_pos<- NULL

for(i in 1:length(sent)){
  string<- sent[i]
  lines<- unlist(strsplit(string, '@'))
  
  for(j in 1:length(lines)){
    text= lines[j]
    words<- unlist(strsplit(text, " "))
    
    start_char= 1
    
    for(k in 1:length(words)){
      t<- data.frame(item=i, line= j, word= k, start= NA, end= NA, length= NA, OVP= NA, string= NA)
      
      t$length<- nchar(words[k])
      t$string<- words[k]
      t$start<- start_char
      t$end<- start_char + nchar(words[k])-1 
      range<- t$start:t$end
      t$OVP<- range[ceiling(length(range)/2)]
      
      start_char<- start_char + nchar(words[k])+1
      
      word_pos<- rbind(word_pos, t)
    }
    
    
  }
  
  
}


save(word_pos, file= 'data/word_pos_Comprehension.Rda')
