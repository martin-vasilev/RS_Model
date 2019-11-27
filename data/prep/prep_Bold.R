
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

RSb<-subset(raw_OZ, Rtn_sweep==1)
RSb$Condition<- ifelse(RSb$cond==1, "Normal", "Bold") # add condition type
RSb$VA<- 0.30 # visual angle in experiment

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
    RSb$x_offset[i]<- 517
  }else{
    RSb$x_offset[i]<- 481
  }
}


RS$next_sacc<- abs(RS$nextX - RS$xPos)
RS$next_sacc_deg<- NA
RS$next_sacc_let<- NA
RS$next_land_pos<- NA
RS$next_land_let<- NA

for(i in 1:nrow(RS)){
  if(is.element(RS$cond[i], c(1,3))){
    RS$next_sacc_deg[i]<- (RS$next_sacc[i]/ 12)*0.295
    RS$next_sacc_let[i]<- ceiling(RS$next_sacc[i]/ 12)
    RS$next_land_pos[i]<- ((RS$nextX[i] -200)/12)*0.295
    RS$next_land_let[i]<- ceiling((RS$nextX[i] -200)/12)
    
  }else{
    RS$next_sacc_deg[i]<- (RS$next_sacc[i]/ 16)*0.394
    RS$next_sacc_let[i]<- ceiling(RS$next_sacc[i]/ 16)
    RS$next_land_pos[i]<- ((RS$nextX[i] -200)/16)*0.394
    RS$next_land_let[i]<- ceiling((RS$nextX[i] -200)/16)
  }
}


# Visual angle per character for each obs:
RS$VA<- NA
RS$prevVA<- (RS$prevX- 200)*(0.29536722163321/12)

RS$VA[which(RS$cond==1 | RS$cond==3)]<- 0.295
RS$VA[which(RS$cond==2 | RS$cond==4)]<- 0.394

### fix NA character landing positions (outside text):

# small font:
a<- which(is.na(RS$LandStartLet) & is.element(RS$cond, c(1,3)))
RS$LandStartLet[a]<- ceiling(RS$LandStartVA[a]/0.295)
#RS$launchDistLet[a]<- ceiling(RS$launchDistVA[a]/0.295)

# big font:
b<- which(is.na(RS$LandStartLet) & is.element(RS$cond, c(2,4)))
RS$LandStartLet[b]<- ceiling(RS$LandStartVA[b]/0.394)
#RS$launchDistLet[b]<- ceiling(RS$launchDistVA[b]/0.394)

# Fix launch site distance

# small font:
a<- which(is.na(RS$launchDistLet) & is.element(RS$cond, c(1,3)))
RS$launchDistLet[a]<- ceiling(RS$launchDistVA[a]/0.295)

# big font:
b<- which(is.na(RS$launchDistLet) & is.element(RS$cond, c(2,4)))
RS$launchDistLet[b]<- ceiling(RS$launchDistVA[b]/0.394)


# prevChar NAs:
a<- which(is.na(RS$prevChar) & is.element(RS$cond, c(1,3)))
RS$prevChar[a]<- ceiling((RS$prevX[a]- 200)/12)

b<- which(is.na(RS$prevChar) & is.element(RS$cond, c(2,4)))
RS$prevChar[b]<- ceiling((RS$prevX[b]- 200)/16)



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

save(RS, file= 'data/Font_size.Rda')
write.csv(RS, 'data/Font_size.csv')



####################################################################################################

rm(list= ls())

sent <- read.delim("D:/R/RS_Model/data/prep/sent.txt")
sent<- subset(sent, Var2==1)

word_pos<- list()
sent$Var5<- as.character(sent$Var5)
sent$Var6<- as.character(sent$Var6)

for(i in 1:nrow(sent)){
  L2<- sent$Var6[i]
  spaces<- unlist(gregexpr(' ', L2))
  
  word_pos[[toString(i)]]<- spaces
}

save(word_pos, file= 'data/L2_word_pos_FS.Rda')

