library(tidyverse)

df = read.csv('data.csv', sep=';')
ref = read.csv('reference.csv', sep=';')
head(df)

mac_pops = df[!is.na(df$MAC),]$IBGE_POP
pops = df[!is.na(df$IBGE_POP),]$IBGE_POP

mean(mac_pops); median(mac_pops); min(mac_pops); max(mac_pops)
mean(pops); median(pops); min(pops); max(pops)
mean(mac_pops) / mean(pops) - 1

length(mac_pops) / length(pops)

y = c()
z = c()
for (i in 1:length(mac_pops)) {
  y = c(y, length(df[df$IBGE_POP>=sort(mac_pops)[i],]$CITY))
  z = c(z, length(df[(df$IBGE_POP>=sort(mac_pops)[i] & !is.na(df$MAC)),]$CITY))
}

comp_df = as.data.frame(cbind(
  x=1:length(mac_pops),
  y=y,
  z=z
))

comp_df$p = comp_df$z / comp_df$y

ggplot(aes(x,1-p), data=comp_df) + geom_line(color='blue') + ggtitle('% of larger cities that do not have a mcdonalds')

ref$DESCRIPTION
df$REGIAO_TUR
as.factor(df$CATEGORIA_TUR)

# area
# taxes
# number of companies
# elevation
# capital
# HDI
# tourism category
# hotels / hotel beds
df[is.na(df$MAC),]$MAC

na

df$mac_bin = 0
df[!is.na(df$MAC),]$mac_bin = 1

df$AREA = as.numeric(df$AREA)
df$TAXES = as.numeric(df$TAXES)
df$COMP_TOT = as.numeric(df$COMP_TOT)
df$ALT = as.numeric(df$ALT)
df$IDHM = as.numeric(df$IDHM)
df$HOTELS = as.numeric(df$HOTELS)

df$CAPITAL = as.factor(df$CAPITAL)
df$CATEGORIA_TUR = as.factor(df$CATEGORIA_TUR)


mod1 = glm(mac_bin ~ IBGE_POP + AREA + TAXES + COMP_TOT + ALT + CAPITAL + IDHM + CATEGORIA_TUR, family="binomial", data=df)
mod2 = glm(mac_bin ~ IBGE_POP + TAXES + COMP_TOT + ALT + CAPITAL + IDHM + CATEGORIA_TUR, family="binomial", data=df)
mod3 = glm(mac_bin ~ IBGE_POP + AREA + COMP_TOT + ALT + CAPITAL + IDHM + CATEGORIA_TUR, family="binomial", data=df)
mod4 = glm(mac_bin ~ IBGE_POP + AREA + TAXES + ALT + CAPITAL + IDHM + CATEGORIA_TUR, family="binomial", data=df)
summary(mod3)

as.data.frame(cbind(
  actual = df$mac_bin,
  fitted = round(mod1$fitted.values)
))

