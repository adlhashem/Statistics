library(mlbench)
#install.packages('mlbench','rpart.plot','party') 
data("BreastCancer")

# split data into training (n=349) and test data (n=350) 
set.seed (68576897) 
indtrain=sample (1: NROW (BreastCancer), 349, replace=FALSE) 
trainbc=BreastCancer [indtrain,] 
testbc=BreastCancer [-indtrain,]


library (rpart) 
library (rpart.plot) 
rptree <- rpart (Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion  + Epith.c.size + Bare.nuclei + Bl.cromatin 
                 + Normal.nucleoli + Mitoses,  data=trainbc, method="class",  control = rpart.control (xval = 10, minsplit=10, minbucket = 3, cp = 0))

rpart.plot(rptree)



rptreepruned = prune(rptree, cp=rptree$cp[which.min(rptree$cp[, "xerror"]), "CP"])


rpart.plot(rptreepruned)

predrpart=predict(rptreepruned, newdata=testbc, type="class") 
mean(predrpart==testbc$Class)


library (party) 

cttree=ctree(Class~Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli 
             + Mitoses, data=trainbc)



predct=predict(cttree, newdata=testbc, type="response") 
mean (predct==testbc$Class)


