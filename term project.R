bones = bone_density
attach(bones)

## regression part 1

# scatterplots
par(mfrow=c(3,3))
plot(lumbar~age, main='Age vs. Lumbar Density')
plot(lumbar~height, main='Height vs. Lumbar Density')
plot(lumbar~weight, main='Weight vs. Lumbar Density')
plot(lumbar~smoke, main='Smoking Frequency vs. Lumbar Density')
plot(lumbar~tea, main='Tea Consumption vs. Lumbar Density')
plot(lumbar~coffee, main='Coffee Consumption vs. Lumbar Density')
plot(lumbar~alcohol, main='Alc. Consumption vs. Lumbar Density')
plot(lumbar~packyrs, main='Years Smoking vs. Lumbar Density')

par(mfrow=c(3,3))
plot(fshaft~age, main='Age vs. Femoral Shaft Density')
plot(fshaft~height, main='Height vs. Femoral Shaft Density')
plot(fshaft~weight, main='Weight vs. Femoral Shaft Density')
plot(fshaft~smoke, main='Smoking Frequency vs. Femoral Shaft Density')
plot(fshaft~tea, main='Tea Consumption vs. Femoral Shaft Density')
plot(fshaft~coffee, main='Coffee Consumption vs. Femoral Shaft Density')
plot(fshaft~alcohol, main='Alc. Consumption vs. Femoral Shaft Density')
plot(fshaft~packyrsmain='Years Smoking vs. Femoral Shaft Density')

par(mfrow=c(3,3))
plot(fneck~age, main='Age vs. Femoral Neck Density')
plot(fneck~height, main='Height vs. Femoral Neck Density')
plot(fneck~weight, main='Weight vs. Femoral Neck Density')
plot(fneck~smoke, main='Smoking Frequency vs. Femoral Neck Density')
plot(fneck~tea, main='Tea Consumption vs. Femoral Neck Density')
plot(fneck~coffee, main='Coffee Consumption vs. Femoral Neck Density')
plot(fneck~alcohol, main='Alc. Consumption vs. Femoral Neck Density')
plot(fneck~packyrs, main='Years Smoking vs. Femoral Neck Density')

# full models
summary(lm(lumbar~age+height+weight+smoke+tea+coffee+alcohol+packyrs))
summary(lm(fshaft~age+height+weight+smoke+tea+coffee+alcohol+packyrs))
summary(lm(fneck~age+height+weight+smoke+tea+coffee+alcohol+packyrs))

# backwards selection
# threshold: p <= 0.05
summary(lm(lumbar~age+height+weight+smoke+tea+coffee+alcohol+packyrs))
summary(lm(lumbar~age+height+weight+smoke+tea+alcohol+packyrs))
summary(lm(lumbar~age+weight+smoke+tea+alcohol+packyrs))
summary(lm(lumbar~age+smoke+tea+alcohol+packyrs))
summary(lm(lumbar~age+smoke+alcohol+packyrs))
summary(lm(lumbar~age+smoke+alcohol))
summary(lm(lumbar~age+alcohol))
summary(lm(lumbar~age))
lumbar.lm = lm(lumbar~age)

summary(lm(fshaft~age+height+weight+smoke+tea+coffee+alcohol+packyrs))
summary(lm(fshaft~age+height+weight+smoke+tea+alcohol+packyrs))
summary(lm(fshaft~age+height+weight+smoke+alcohol+packyrs))
summary(lm(fshaft~age+height+weight+smoke+alcohol))
summary(lm(fshaft~age+height+weight+alcohol))
summary(lm(fshaft~age+height+weight))
fshaft.lm = lm(fshaft~age+height+weight)
fshaft.lm.red = lm(fshaft~age+height)
anova(fshaft.lm.red, fshaft.lm)

summary(lm(fneck~age+height+weight+smoke+tea+coffee+alcohol+packyrs))
summary(lm(fneck~age+height+weight+smoke+tea+alcohol+packyrs))
summary(lm(fneck~age+height+weight+tea+alcohol+packyrs))
summary(lm(fneck~age+height+weight+tea+packyrs))
summary(lm(fneck~age+height+weight+packyrs))
summary(lm(fneck~age+height+weight))
summary(lm(fneck~age+height))
fneck.lm = lm(fneck~age+height)

# residual plots
par(mfrow=c(1,1))
plot(fitted(lumbar.lm),residuals(lumbar.lm),main="residual plot")
plot(fitted(fshaft.lm),residuals(fshaft.lm),main="residual plot")
plot(fitted(fneck.lm),residuals(fneck.lm),main="residual plot")

# Q-Q plots
qqnorm(residuals(lumbar.lm))
qqline(residuals(lumbar.lm))
qqnorm(residuals(fshaft.lm))
qqline(residuals(fshaft.lm))
qqnorm(residuals(fneck.lm))
qqline(residuals(fneck.lm))

## regression part 2 (twins)
all.twins = bones_twins_sorted
mono = bones_twins_sorted[1:21,]
di = bones_twins_sorted[22:41,]

# scatterplots
par(mfrow=c(3,3))
plot(lumbar_2~lumbar_1, all.twins, main='All Twins')
plot(fshaft_2~fshaft_1, all.twins,main='All Twins')
plot(fneck_2~fneck_1, all.twins,main='All Twins')
plot(lumbar_2~lumbar_1, mono,main='Monozygous Twins')
plot(fshaft_2~fshaft_1, mono,main='Monozygous Twins')
plot(fneck_2~fneck_1, mono,main='Monozygous Twins')
plot(lumbar_2~lumbar_1, di,main='Dizygous Twins')
plot(fshaft_2~fshaft_1, di,main='Dizygous Twins')
plot(fneck_2~fneck_1, di,main='Dizygous Twins')


# models
all.lumbar = lm(lumbar_2~lumbar_1, all.twins)
all.fshaft = lm(fshaft_2~fshaft_1, all.twins)
all.fneck = lm(fneck_2~fneck_1, all.twins)
mono.lumbar = lm(lumbar_2~lumbar_1, mono)
mono.fshaft = lm(log(fshaft_2)~fshaft_1, mono)
mono.fneck = lm(fneck_2~fneck_1, mono)
di.lumbar = lm(lumbar_2~lumbar_1, di)
di.fshaft = lm(log(fshaft_2)~fshaft_1, di)
di.fneck = lm(fneck_2~fneck_1, di)

summary(all.lumbar)
summary(all.fshaft)
summary(all.fneck)
summary(mono.lumbar)
summary(mono.fneck)
summary(mono.fshaft)
summary(di.lumbar)
summary(di.fneck)
summary(di.fshaft)

# residual plots
par(mfrow=c(3,3))
plot(fitted(all.lumbar),residuals(all.lumbar),main="residual plot")
plot(fitted(all.fshaft),residuals(all.fshaft),main="residual plot")
plot(fitted(all.fneck),residuals(all.fneck),main="residual plot")
plot(fitted(mono.lumbar),residuals(mono.lumbar),main="residual plot")
plot(fitted(mono.fshaft),residuals(mono.fshaft),main="residual plot")
plot(fitted(mono.fneck),residuals(mono.fneck),main="residual plot")
plot(fitted(di.lumbar),residuals(di.lumbar),main="residual plot")
plot(fitted(di.fshaft),residuals(di.fshaft),main="residual plot")
plot(fitted(di.fneck),residuals(di.fneck),main="residual plot")

# being monozygous is a better predictor of bone density
# than being dizygous, lower p-values for the slopes