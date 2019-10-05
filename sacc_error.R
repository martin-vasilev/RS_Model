
#df= data.frame(x= c(10.5, 13.0, 15.5, 18.0, 20.5), y= c(9.65, 11.96, 14.28, 16.71, 19.05))

### SYSTEMATIC ERROR:
# from Gillen, Weiler, & Heath (2013), Exp. Brain Res., Table 1

df= data.frame(x= c(3.0, 5.5, 8.0, 10.5, 13.0, 15.5, 18.0, 20.5), y= c(2.87, 5.16, 7.46, 9.75, 12.09, 14.28, 16.71, 19.05))

df$und<- df$x- df$y

L1<- lm(und~x, df)

L1

### RANDOM ERROR:
# from Gillen, Weiler, & Heath (2013), Exp. Brain Res., Table 1
ds= data.frame(x= c(3.0, 5.5, 8.0, 10.5, 13.0, 15.5, 18.0, 20.5), y= c(0.39, 0.50, 0.86, 0.90, 1.17, 1.57, 1.51, 1.80))

L2<- lm(y~x, ds)
L2


# ### Kapoula & Robinson (1986)
# db= data.frame(x= c(5, 10, 15, 20), y= c(5.12, 9.41, 13.11, 17.21))
# db$und<- db$x- db$y
# summary(lm(und~x, db))

#1.22 +0.20

# undershoot probability:
#from Kapoula & Robinson (1985), Table 2
#df2<- data.frame(x= c(5, 10, 15, 20), y= c(39, 52, 55, 67))

#(lm(y~x, data= df2))


# ###
# df2<- data.frame(x= c(10.5, 13.0, 15.5, 18.0, 20.5), y= c(0.94, 1.02, 1.57, 1.51, 1.80))
# 
# L2<- lm(y~x, df2)
# L2
