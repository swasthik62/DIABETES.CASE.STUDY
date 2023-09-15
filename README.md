# DIABETES.CASE.STUDY

Problem Statement:
This dataset is originally from the National Institute of Diabetes and Digestive and Kidney
Diseases. The objective of the dataset is to diagnostically predict whether a patient has diabetes

Solution: 
I collected the Dataset and started create to build the odel on this.

SETTING THE WORKING DIRECTORY AND IMPORTING THE CSV FILE
```
getwd()
setwd("C:/Users/SATHWIK/OneDrive/Documents/New folder")
dt_train=read.csv("diabetes.csv",stringsAsFactors = F)
```

USEd THE RECIPE FUNCTION TO CONVERT INT TO NUMERIC
```
library(tidymodels)
dp_pipe=recipe(Outcome~.,data=dt_train) %>%
  update_role(Pregnancies,Glucose,BloodPressure,SkinThickness,Insulin,Age ,new_role ="to_numeric") %>% 
  step_mutate_at(has_role("to_numeric"),fn=as.numeric) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)
train=bake(dp_pipe,new_data = NULL)
```

SPLIT THE TRAIN DATA INTO 80% AND 20% TO CHECK THE MODEL
```
set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
trn=train[s,]
tst=train[-s,]
```

USE THE LM FUNCTION TO CHECK IS THERE ANY VIF VALUES
```
library(car)
for_vif=lm(Outcome~.,data = trn)
sort(vif(for_vif),decreasing = T)[1:3]
summary(for_vif)
```
there is no VIF values found, Now i run the model on GLM

NOW RUN THE MODEL ON GLM
```
glm=glm(Outcome~.,data = trn)
val.score=predict(glm,newdata = trn, type = "response")
pROC::auc(pROC::roc(trn$Outcome,val.score)) #0.8432

val.score=predict(glm,newdata = tst, type = "response")
pROC::auc(pROC::roc(tst$Outcome,val.score)) #0.8167
```

WE ARE NOT GETTING THE GOOD AUC SCORE STILL I RUN THE MODEL ON ENTIRE DATASET
```
fit=stats::step(glm)
formula(glm)
glm_final = glm(Outcome~Pregnancies + Glucose + BloodPressure + SkinThickness + 
                  Insulin + BMI + DiabetesPedigreeFunction + Age,data = train)

train.score=predict(glm_final,newdata = train, type = "response")
pROC::auc(pROC::roc(train$Outcome,train.score)) #0.8393
```
The value is not convincing hence i more with other models.

NOW I IMPLEMETING THE DATA ON DTREE 
```
library(tree)
dtree= tree(Outcome~.,data=trn)

val.score=predict(dtree,newdata = trn,)
pROC::auc(pROC::roc(trn$Outcome,val.score)) #0.8729

val.score1=predict(dtree,newdata = tst)
pROC::auc(pROC::roc(tst$Outcome,val.score1)) #0.8086
```

NOW I RUN THE MODEL ON RANDOM FOREST
```
library(randomForest)
rrf = randomForest(Outcome~.,data=trn)
val.score=predict(rrf,newdata = trn, type="response")
pROC::auc(pROC::roc(trn$Outcome,val.score)) #1 #i got the good AUC score 

val.score1=predict(rrf,newdata = tst) 
pROC::auc(pROC::roc(tst$Outcome,val.score1)) #0.8276
```
I hot the convincing AUC score and now im runnig the model on entire TRAIN dataset.

I RUN THE ENTIRE MODEL ON RANDOM FOREST
```
rrf = randomForest(Outcome~.,data=train)
val.score=predict(rrf,newdata = train, type="response")
pROC::auc(pROC::roc(train$Outcome,val.score)) #1
```
Lets check which variables are important inorder to get the diabetes in patients
```
varImpPlot(rrf)
```
![image](https://github.com/swasthik62/DIABETES.CASE.STUDY/assets/125183564/986cbc07-8f8f-46ba-a340-d3265c97fba6)

when we plot the varImpPlot = the patient has Glucose, BMI, Age, Biabetese pigmentation are the major variable to cause the DIABETES

Lets plot the AUC Graph and see the grapghical represenation of AUC score.
```
library(pROC)
roc_obj <- roc(train$Outcome, test.score)
plot(roc_obj, main="ROC Curve for Logistic Regression", print.auc=TRUE)
```
![image](https://github.com/swasthik62/DIABETES.CASE.STUDY/assets/125183564/21a04c61-3dd9-4f1e-9e5e-5490628d9012)


**CONCLUSION**

**ON BASED ON THE DATA TRAINING WE FOUND THAT THE PATIENT HAS DIABETES**

##_------------------------------------------------------------------------------------------------------------------------------


RANDOM FOREST HYPERPARAMETRE TUNING(THIS STEP IS NOT REQUIRED FOR THIS MODEL AS WE GOT THE GOOD AUC SCORE ALREADY)
```
param = list(
  interaction.depth = c(1:7),
  n.trees = c(50, 100, 200, 500, 700),
  shrinkage = c(0.1, 0.01, 0.001),
  n.minobsinnode = c(1, 2, 5, 10)
)

subset_paras = function(full_list_para, n = 10) {
  all_comb = expand.grid(full_list_para)
  s = sample(1:nrow(all_comb), n)
  subset_para = all_comb[s, ]
  return(subset_para)
}

num_trials = 10

my_params=subset_paras(param,num_trials)

mycost_auc = function(y, yhat) {
  roccurve = pROC::roc(y, yhat)
  score = pROC::auc(roccurve)
  return(score)
}

library(cvTools)
myauc = 0
library(gbm)
for (i in 1:num_trials) {
  print(paste('starting iteration :', i))
  params = my_params[i, ]
  
  k = cvTuning(
    gbm,
    Outcome ~ . ,
    data = train,
    tuning = params,
    args = list(distribution = "bernoulli"),
    folds = cvFolds(nrow(train), K = 10, type = "random"),
    cost = mycost_auc,
    seed = 2,
    predictArgs = list(type = "response", n.trees = params$n.trees)
  )
  score.this = k$cv[, 2]
  
  if (score.this > myauc) {
    print(params)
    myauc = score.this
    print(myauc)
    best_params = params
  }
  
  print('DONE')
}

best_params=data.frame(interaction.depth=2,
                       n.trees=100,
                       shrinkage=0.01,
                       n.minobsinnode=2)

rg.gbm.final=randomForest(Outcome~.,
                          data =train,,
                          n.trees = best_params$n.trees,
                          n.minobsinnode = best_params$n.minobsinnode,
                          shrinkage = best_params$shrinkage,
                          interaction.depth = best_params$interaction.depth,
                          distribution = "bernoulli")

test.score=predict(rg.gbm.final,newdata=train,type='response',
                   n.trees = best_params$n.trees)
pROC::auc(pROC::roc(train$Outcome,test.score))
```


