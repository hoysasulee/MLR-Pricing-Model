
library(dplyr)
library(ggfortify)
library(ggplot2)
library(MASS)
library(cowplot)
library(plotly)
library("factoextra")
library("gg3D")
Pricing  <-read.csv("~/Desktop/current_pricing.csv")
Pricing$Type <- factor(Pricing$Type)
Pricing$Weight <- factor(Pricing$Weight)
Pricing$Type <- as.numeric(Pricing$Type)
Pricing$Weight <- as.numeric(Pricing$Weight)
Pricing2 <- read.csv("~/Desktop/△/STAT995/Data/current_pricing_v3.csv")
Pricing3 <- Pricing2[,c(1,3,2,4,5,6,7,8,9,10,11,12,13,14,15,16)]
Pricing3$Type <- factor(Pricing3$Type)
Pricing3$Weight <- factor(Pricing3$Weight)
Pricing3$Type <- as.numeric(Pricing3$Type)
Pricing3$Weight <- as.numeric(Pricing3$Weight)

#z-score
mean<-apply(Pricing[,3:16],2, mean)
mean
sd <- apply(Pricing[,3:16],2, sd)
sd
zscore <- as.data.frame(lapply(Pricing[,3:16], function(x) (x - mean(x))/ sd(x)))
zscore <- zscore[,colSums(is.na(zscore))<nrow(zscore)]
mean(zscore[,1])
sd(zscore[,1])

S<-cov(zscore)
S
write.csv(S, "~/Desktop/S.csv")
sum(diag(S))
s.eigen <- eigen(S)
s.eigen
for (s in s.eigen$values) {
  print(s / sum(s.eigen$values))
}
plot(s.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(s.eigen$values)

R<-cor(zscore)
R
write.csv(R, "~/Desktop/R.csv")

df_pca <- prcomp(zscore)
df_pca
write.csv(df_pca, "~/Desktop/pca.csv")
pcs <- df_pca$rotation
write.csv(pcs, "~/Desktop/pcs.csv")
df_pca_pre <- predict(df_pca)
df_pca_pre

fviz_pca_ind(df_pca, label="none", habillage=Pricing$Type,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")+ggtitle("Scatterplot and confidence ellipse of PC1 and PC2")+theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/pic1.png')

fviz_eig(df_pca)+ggtitle("Scree graph")+theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/pic2.png')
autoplot(df_pca)+
  theme_bw()
ggsave('~/Desktop/pic3.png')

Pricing1 <- Pricing[,c(2,4,5,6,8,9)]
ggplot(Pricing2,aes(Material.cost,Unit.quote.price,color=Weight))+
  geom_point()+
  facet_grid(~Type)+
  theme_bw()
ggsave('~/Desktop/material.png')
ggplot(Pricing2, aes(x=Material.cost, y=Unit.quote.price, z=Quantity, color=Weight)) + 
     theme_void() +
     axes_3D() +
     stat_3D()
ggsave('~/Desktop/material_3d.png')

plot_ly(data=Pricing2, x= ~Material.cost, y= ~Unit.quote.price, z= ~Quantity, type="scatter3d", mode="markers", color= ~Weight)

ggplot(Pricing2,aes(Material.cost,Unit.quote.price,color=Weight))+
  geom_point()+
  facet_grid(Weight~Type)+
  theme_bw()
ggsave('~/Desktop/material_all.png')
ggplot(Pricing2,aes(Quantity,Unit.quote.price,color=Weight))+
  facet_grid(~Type)+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE,color='red')
ggsave('~/Desktop/quantity.png')
ggplot(Pricing2,aes(Quantity,Unit.quote.price,color=Weight))+
  geom_point()+
  facet_grid(Weight~Type)+
  geom_smooth(method='loess')+
  theme_bw()
ggsave('~/Desktop/quantity_all.png')

ggplot(Pricing2,aes(Machine.price,Unit.quote.price,color=Type))+
  geom_point()+
  theme_bw()
ggsave('machine.png')

ggplot(Pricing2,aes(Machine.price,Unit.quote.price,color=Type))+
  geom_point()+
  facet_grid(Weight~Type)+
  geom_smooth(method='loess')+
  theme_bw()
ggsave('machine_all.png')

ggplot(Pricing2,aes(Discount,Unit.quote.price,color=Weight))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE,color='red')
ggsave('discount.png')

ggplot(Pricing2,aes(Discount,Unit.quote.price))+
  geom_point()+
  facet_grid(Weight~Type)+
  geom_smooth(method='loess')+
  theme_bw()
ggsave('discount_all.png')
lmTemp = lm(Discount~Unit.quote.price, data = Pricing2)

for(i in c(1,5,6)){
  Pricing1[,i] <- scale(Pricing1[,i])
}
#linear regression model
zscore$pc1 <- df_pca_pre[,1]
zscore$pc2 <- df_pca_pre[,2]
zscore$pc3 <- df_pca_pre[,3]
zscore$pc4 <- df_pca_pre[,4]
Model1<-lm(Unit.quote.price~.,data=zscore)
Model2 <- stepAIC(Model1,trace=F)
Model3 <- lm(Unit.quote.price~pc1+pc2+pc3+pc4, data = zscore)
# Linear regression finds out significant factors
summary(Model1)
summary(Model2)
summary(Model3)

M1 <- tab_model(
  Model1,
  dv.labels = c("OLS Model"),
  pred.labels = c("Intercept", "Type", "Weight", "Quantity","Total quote price",
                  "Material cost", "Discount", 
                  "Total Profit", "Unit profit"),
  show.se = TRUE,show.fstat = TRUE,
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)
stargazer(Model1, type = "html", title = "Results", align = TRUE, out='~/Desktop/△/data/R/model1.htm')
M2 <- tab_model(
  Model2,
  dv.labels = c("StepAIC-OLS Model"),
  pred.labels = c("Intercept", "Type", "Weight", "Quantity","Total quote price",
                  "Material cost", "Discount", 
                  "Total Profit", "Unit profit"),
  show.se = TRUE,show.fstat = TRUE,
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)
stargazer(Model2, type = "html", title = "Results", align = TRUE, out='~/Desktop/△/data/R/model2.htm')
M3 <- tab_model(
  Model3,
  dv.labels = c("PCR Model"),
  pred.labels = c("Intercept", "Type", "Weight", "Quantity","Total quote price",
                  "Material cost", "Discount", 
                  "Total Profit", "Unit profit"),
  show.se = TRUE,show.fstat = TRUE,
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)
stargazer(Model3, type = "html", title = "Results", align = TRUE, out='~/Desktop/△/data/R/model3.htm')
png('~/Desktop/pic8.png')
par(mfrow=c(2,2))
plot(Model2,which=1:4,label="none")
dev.off()
par(mfrow=c(1,1))
COEF<- data.frame(coefficent=coef(Model3)[-1])
# Rank factors by significance;
ggplot(COEF,aes(1:nrow(COEF),coefficent))+
  geom_bar(stat='identity')+
  theme_bw()+xlab("")+
  scale_x_continuous(breaks=1:nrow(COEF),label=rownames(COEF))+
  coord_flip()
ggsave('~/Desktop/pic9.png')
