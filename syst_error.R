
# Systematic error:

# from Gillen, Weiler, & Heath (2013), Exp. Brain Res., Table 1
df= data.frame(x= c(10.5, 13.0, 15.5, 18.0, 20.5), y= c(9.65, 11.96, 14.28, 16.71, 19.05))
df$und<- df$x- df$y

L1<- lm(und~x, df)

L1


# undershoot probability:
#from Kapoula & Robinson (1985), Table 2
df2<- data.frame(x= c(5, 10, 15, 20), y= c(39, 52, 55, 67))

(lm(y~x, data= df2))


# ###
# df2<- data.frame(x= c(10.5, 13.0, 15.5, 18.0, 20.5), y= c(0.94, 1.02, 1.57, 1.51, 1.80))
# 
# L2<- lm(y~x, df2)
# L2
