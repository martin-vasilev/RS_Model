
rm(list= ls())

load("data/prep/Font_size.Rda")

sent <- read.delim("D:/R/RS_Model/data/prep/sent.txt")

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
