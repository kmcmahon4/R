library(scatterplot3d)
data = read.csv("final_tablet.csv", header = TRUE, sep = ",")


lm <- lm(data$Price..USD. ~     data$Available.Seats + data$Days.until.flight, data = data)
lm

f <- data.frame(data$Days.until.flight,data$Available.Seats,data$Price..USD. )

# 3D scatter plot
s3d <- scatterplot3d(f, type = "h",color = "brown",
pch = 16, main = 'Visualization of Data with Regression Plane' ,angle =1200
,
xlab = 'Available Seats', ylab = "Days Until Flight", zlab = 'Price un USD') 

s3d$plane3d(lm)
# Add supplementary points


x <- data$Price..USD
df <- sort(x)
std <-sd(data$Price..USD)
mu <- 203.3
y <- dnorm(df, mu,std)
sd_to_fill <- 1

plot(df,y, lwd =2,type ='n',col="yellow",main = "Normal Distribution of Prices",axes = FALSE,xlab = "Price in USD", ylab ="Density",bg = 'lightblue', xlim = c(100,350), ylim = c(0,.008))
par(bg = "lightyellow")
lines(df,y)
lower <- mu - std
upper <- mu + std

boundsfilter <- df>=lower & df <= upper
df_in <- df[boundsfilter]
y_in <- y[boundsfilter]

dfpoly <- c(lower,df_in,upper)
ypoly <- c(0,y_in,0)

polygon(dfpoly,ypoly,col = 'lightblue')
round <- round(axis_bounds,digits =2)
sd_axis_bounds =6
axis_bounds <- seq(-sd_axis_bounds * std + mu, sd_axis_bounds * std + mu, by = std)
axis(side =1, at = round, pos =0)

legend(270, .008, legend=c("Mean = $203.30", "SD = 53.03"), #Legend
        cex=0.8,
       title="Legend", text.font=4, bg='lightyellow')

summary(lm)
lm
