
# Effect of bolding on first word saccade targetting
# data from:   Slattery, T. J. & Vasilev, M. R. (2019). An eye-movement exploration into return-sweep
# targeting during reading. Attention, Perception, & Psychophysics, 81(5), 1197-1203. doi: 10.3758/s13414-019-01742-3 


# x-offset is 481; however, some lines were indented, so the x start was 541. Indented lines have 5 '#'s before text
# Pixel per letter (ppl) was 12

rm(list= ls())

#library(EMreading)
#raw_OZ<- preprocFromDA1(data_dir = "D:/Data/Oz/Fix33", ResX = 1920, ResY = 1080, tBlink = 200, padding = 5, 
#                        maxtrial = 60)
#save(raw_OZ, file= 'raw_OZ.Rda')


load("data/prep/Bold_data_raw.Rda")
x_offset<- 481
x_offset_indent<- 541
#sent <- read.delim("D:/R/RS_Model/data/prep/sent.txt")

### Add some additional stuff:
nsubs<- unique(raw_OZ$sub)

new<- NULL
for(i in 1:length(nsubs)){ # for each subject..
  n<- subset(raw_OZ, sub== nsubs[i])
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

raw_OZ<- new; rm(new)



RSb<-subset(raw_OZ, Rtn_sweep==1)
RSb$Condition<- ifelse(RSb$cond==1, "Normal", "Bold") # add condition type
RSb$VA<- 0.295 # visual angle in experiment (equivalent to "small font" in the font size paper)

# remove outliers:
out<- which(RSb$fix_dur<40 |RSb$fix_dur>1000)
RSb<- RSb[-out,]

# remove blinks:
RSb<- subset(RSb, prev_blink==0 & after_blink==0 & blink==0)


# remove useless columns:
RSb$hasText<- NULL
RSb$time_since_start<- NULL
RSb$outOfBnds<- NULL
RSb$blink<- NULL
RSb$prev_blink<- NULL
RSb$after_blink<- NULL
RSb$Rtn_sweep<- NULL

RSb$undersweep_prob<- ifelse(RSb$Rtn_sweep_type== "undersweep", 1,0)

library(reshape)
Des<- melt(RSb, id=c('sub', 'item', 'Condition'), 
            measure=c("char_line", "undersweep_prob") , na.rm=TRUE)

m<- cast(Des, Condition ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


# map x offset (used for calculating visual angle):
d_num<- c(2,3,5,7,9,10,11,13,15,16,17,18,20,21,22,23,25)
d<- paste('https://raw.githubusercontent.com/martin-vasilev/Oz/master/Experiment/DorothyText/Dorothy', 
          d_num, '.txt', sep='') 

t_num<- c(2,4,5,6,8,10,11,12,13,15,16,17,18,20,22,23,24,26)
t<- paste('https://raw.githubusercontent.com/martin-vasilev/Oz/master/Experiment/TiktokText/Tiktok', 
          t_num, '.txt', sep='') 

files<- c(d, t)

item<- NULL
line<- NULL
word<- NULL
curr_item<- NULL
indent<- NULL
isIndented<- NULL

for(i in 1:length(files)){ # for each text page..
  text<- readLines(files[i])
  
  for(j in 1:length(text)){ # for each line in text
    string<- unlist(strsplit(text[j], " "))
    #word_string<- gsub("#", "", string[1])
    word_string<- string[1]
    
    if(substr(word_string,1,1)=="#"){
      isIndented<- 1
    }else{
      isIndented<- 0
    }
    indent<- c(indent, isIndented)
    
    curr_item<- get_num(files[i])
    line<- c(line, j)
    word<- c(word, word_string)
    item<- c(item, curr_item)
  }
  
}


wb<- data.frame(item, line, word, indent)
wb$word<- as.character(wb$word)
wb$length<- nchar(wb$word)
wb$item[161:nrow(wb)]<-wb$item[161:nrow(wb)] +25


# map indent to data frame
RSb$x_offset<- NA

for(i in 1:nrow(RSb)){
  a<- which(wb$item== RSb$item[i] & wb$line== RSb$line[i])
  
  if(wb$indent[a]==1){
    RSb$x_offset[i]<- x_offset_indent
  }else{
    RSb$x_offset[i]<- x_offset
  }
}


# add landing position relative to line start (in letters):
RSb$LandStartLet<- RSb$char_line

# landing position relative to line start (in degrees per visual angle)

DPP<- 0.02461393513610085 # degree per pixel in the experiment

RSb$LandStartVA<- (RSb$xPos - RSb$x_offset)*DPP

# code (absolute) launch site distance in letters:
RSb$launchDistLet<- abs(RSb$char_line- RSb$prevChar)

# code (absolute) launch site distance in visual angle:
RSb$launchDistVA<- abs(RSb$xPos- RSb$prevX)*DPP


# recode saccade length:
RSb$sacc_len<- abs(RSb$char_line- RSb$prevChar)



### fix NA character landing positions (outside text):
a<- which(is.na(RSb$LandStartLet))
RSb$LandStartLet[a]<- ceiling(RSb$LandStartVA[a]/0.295)

# Fix launch site distance
a<- which(is.na(RSb$launchDistLet))
RSb$launchDistLet[a]<- ceiling(RSb$launchDistVA[a]/0.295)

# prevChar NAs:
a<- which(is.na(RSb$prevChar))
RSb$prevChar[a]<- ceiling((RSb$prevX[a]- RSb$x_offset[a])/12)


RSb$next_sacc<- abs(RSb$nextX - RSb$xPos)
RSb$next_sacc_deg<- NA
RSb$next_sacc_let<- NA
RSb$next_land_pos<- NA
RSb$next_land_let<- NA

for(i in 1:nrow(RSb)){
    RSb$next_sacc_deg[i]<- (RSb$next_sacc[i]/ 12)*0.295
    RSb$next_sacc_let[i]<- ceiling(RSb$next_sacc[i]/ 12)
    RSb$next_land_pos[i]<- ((RSb$nextX[i] -RSb$x_offset[i])/12)*0.295
    RSb$next_land_let[i]<- ceiling((RSb$nextX[i] -RSb$x_offset[i])/12)
}

RSb$x_offset<- NULL

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

save(RSb, file= 'data/Bold_OZ.Rda')
write.csv(RSb, 'data/Bold_OZ.csv')



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
