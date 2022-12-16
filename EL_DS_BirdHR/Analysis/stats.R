merged <- read.csv("/Users/evanlewis/Desktop/merged.csv")

linear <- lm( Mass ~ akde.ml, data = merged)
print(linear)             
summary(linear)

lm(data.frame(scale(linear$model)))
