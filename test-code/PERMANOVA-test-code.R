# NOT RUN {
data(wine)
X = wine[,4:21]
X=IniTransform(X)
D = DistContinuous (X)
perwine=PERMANOVA(D, wine$Group)
perwine


C = matrix(c(1, 1, -1, 1, 1, -1, 1, 1, 1, -1, -1, 1), nrow=3, byrow=TRUE)
rownames(C)=c("C1", "C2", "C3")
colnames(C)=levels(wine$Group)

effects=factor(c(1,2,3))
levels(effects)=c("Origin", "Year", "Interaction")
perwine2=PERMANOVA(D, wine$Group, C=C, Effects=effects, CoordPrinc = TRUE)
summary(perwine2)
# }