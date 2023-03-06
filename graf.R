# Gráficos de regressão
set.seed(123)
u <- rnorm(5,0,1)
x<-seq(2,10,2)
y<-2+5*x+u*10
mod<-lm(y~x)


##### Plot 1

png(file = "figuras/line1.png", 
    width = 480,
    height = 480,
    units =  "px")

par(mar = c(5.1,4.1,2.1,2.1)) # par(mar = c(5.1,4.1,4.1,2.1))

plot(x,y, pch = 19, col="darkblue", xaxt='n', yaxt='n', ann=FALSE)
abline(a=-1.105,b=5.840, col = "red", lwd = 2)

mtext("y", side=2, line=1, cex=1.5, las=1)
mtext("x", side=1, line=1, cex=1.5, las=1)

# 3 - Encerra com dev.off()
dev.off()



##### Plot 2

png(file = "figuras/line2.png", 
    width = 480,
    height = 480,
    units =  "px")

par(mar = c(5.1,4.1,2.1,2.1)) # par(mar = c(5.1,4.1,4.1,2.1))

plot(x,y, pch = 19, col="darkblue", xaxt='n', yaxt='n', ann=FALSE)
abline(a=-1.105,b=5.840, col = "red", lwd = 2)
segments(6,mod$fitted.values[3],6,y[3], lty = "dashed")
text(x = 6, y = y[3], expression(italic(~ y[i])), pos = 2,cex=1.3)
text(x = 6, y = (y[3]+mod$fitted.values[3])/2, expression(italic(e[i]==y[i]-hat(y)[i])), pos = 2, cex=1.3)
text(x = 6, y = mod$fitted.values[3], expression(italic(~ hat(y)[i])), pos = 2,cex=1.3)
text(x = 5.3, y = mod$fitted.values[2]-1,
     expression(italic(~ beta[1])),cex=1.3)
segments(4,mod$fitted.values[2],5,mod$fitted.values[2], lty = "dashed")
segments(5,mod$fitted.values[2],
         5,(mod$fitted.values[2]+mod$fitted.values[3])/2, lty = "dashed")
mtext(text = expression(italic(~ beta[0])), 
      side = 2, 
      line = .5,
      outer = FALSE, 
      at = mod$fitted.values[1]-2, 
      las = 2,
      cex=1.3)
mtext("y", side=2, line=1, cex=1.5, las=1)
mtext("x", side=1, line=1, cex=1.5, las=1)

# 3 - Encerra com dev.off()
dev.off()

text(x = 2, y = mod$fitted.values[1], expression(italic(~ beta[0])), pos = 3)

dados$predicted <- predict(mod)   # Save the predicted values
dados$residuals <- residuals(mod) # Save the residual values

dados <- data.frame(x,y)
devtools::install_github("NicolasH2/ggbrace")
library(ggbrace)
library(ggplot2)

dados %>%
ggplot() + geom_brace(aes(x=c(0,1), y=c(0,1)))



ggplot(dados, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_point(size =3) +
  geom_point(aes(y = predicted), shape = 1) +
  geom_segment(aes(xend = x, yend = predicted))+
  theme_classic()+
  stat_brace(rotate = 90)



library(grid)


geom_brace(aes(x=c(6,1), y=c(0,1)))
