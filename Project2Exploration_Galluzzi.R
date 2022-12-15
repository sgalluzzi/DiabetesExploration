library(faraway)
library(caret)

df <- diabetes

?diabetes
summary(df)
head(df)


df <- subset(df, select = -c(id,bp.2s,bp.2d))



df <- df                                    
df$glyhb[is.na(df$glyhb)] <- median(df$glyhb, na.rm = TRUE)  

df <- df                                    
df$chol[is.na(df$chol)] <- median(df$chol, na.rm = TRUE)  

df <- df                                    
df$hdl[is.na(df$hdl)] <- median(df$hdl, na.rm = TRUE)  

df <- df                                    
df$ratio[is.na(df$ratio)] <- median(df$ratio, na.rm = TRUE)  

df <- df                                    
df$height[is.na(df$height)] <- median(df$height, na.rm = TRUE)  

df <- df                                    
df$weight[is.na(df$weight)] <- median(df$weight, na.rm = TRUE)  

df <- df                                    
df$bp.1s[is.na(df$bp.1s)] <- median(df$bp.1s, na.rm = TRUE)  

df <- df                                    
df$bp.1d[is.na(df$bp.1d)] <- median(df$bp.1d, na.rm = TRUE)  

df <- df                                    
df$waist[is.na(df$waist)] <- median(df$waist, na.rm = TRUE)  

df <- df                                    
df$hip[is.na(df$hip)] <- median(df$hip, na.rm = TRUE)  

df <- df                                    
df$time.ppn[is.na(df$time.ppn)] <- median(df$time.ppn, na.rm = TRUE)  

df$frame <- df$frame %>% replace_na('medium')

head(df)

summary(df)

cleaned_df <- df
######################################################################3


install.packages('caret')
library(caret)

#####################################################################

df[sapply(df, is.factor)] <- data.matrix(df[sapply(df, is.factor)])




set.seed(6021)
sample<-sample.int(nrow(df), floor(.70*nrow(df)), replace = F)
train<-df[sample, ]
test<-df[-sample, ] 



set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(glyhb ~chol+stab.glu+age+time.ppn, data = df, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


#### 15 percent 
60/403



glyline <-lm(glyhb~., data=df)
summary(glyline)


gly_stab_line <-lm(glyhb~stab.glu, data=df)
summary(gly_stab_line)

result1 <-lm(glyhb~chol+stab.glu+age+time.ppn, data=df)
summary(result1)


df$yhat<-result1$fitted.values
df$res<-result1$residuals

ggplot(df, aes(x=yhat ,y= res))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="yhat", y="Residual", title="Residual Plot")

df$logyhat=log(df$yhat)

ggplot(df, aes(x=logyhat ,y= residual))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="logyhat", y="Residual", title="Residual Plot")




multi_check <- lm(glyhb~chol*ratio, data=df)
summary(multi_check)


########################
#### 70-30 split result

glyline <-lm(glyhb~., data=train)
summary(glyline)

#result<-lm(glyhb ~ age + chol + stab.glu + time.ppn , family="binomial", data=train)
#summary(result)

round(cor(df),3)


gly_option <- lm(glyhb~stab.glu)

acf(glyline)

df$diabetes <- df$glyhb > 7
df <- subset(df, select = -c(diabetes))

summary(df)

head(df)


df[sapply(df, is.factor)] <- data.matrix(df[sapply(df, is.factor)])





library(tidyverse)

ggplot(df, aes(x=glyhb))+
  geom_histogram(na.rm=TRUE, fill = "navyblue")+
  scale_x_continuous(n.breaks=20)+
  labs(x = "Glycosolated Hemoglobin", 
       title = "Glycosolated Hemoglobin Histogram")

ggplot(train, aes(x=glyhb))+
  geom_density(na.rm=TRUE, fill = "navyblue")+
  scale_x_continuous(n.breaks=20)+
  labs(x = "Glycosolated Hemoglobin", 
       title = "Glycosolated Hemoglobin Density Plot")

ggplot(train, aes(x= chol ,y= glyhb))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="chol", y="Glycosolated Hemoglobin", 
       title="Glycosolated Hemoglobin against Cholesterol")

ggplot(train, aes(x= age ,y= glyhb))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="age", y="Glycosolated Hemoglobin", 
       title="Glycosolated Hemoglobin against Cholesterol")

ggplot(train, aes(x= ratio ,y= glyhb))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="age", y="Glycosolated Hemoglobin", 
       title="Glycosolated Hemoglobin against Cholesterol")






cor.test(df$chol, df$glyhb, method=c("pearson"))

chol_gly <-lm(glyhb~chol, data=df)
summary(chol_gly)

df$yhat<-chol_gly$fitted.values
df$residual<-chol_gly$residuals

ggplot(df, aes(x=yhat ,y= residual))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="yhat", y="Residual", title="Residual Plot")

ggplot(df, aes(x=chol ,y= glyhb, color=gender))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="Cholesterol", color= "Gender", title="Cholesterol against Glycosolated Hemoglobin with Gender")

ggplot(train, aes(x=diabetes, fill = gender))+
  geom_bar()+
  labs(x = "Diabetes Risk", fill = "Gender",
       title= "Diabetes Risk by Gender")+
  theme(axis.text.x = element_text(angle = 45))
  

ggplot(df, aes(x=diabetes, y = stab.glu))+
  geom_boxplot()+
  labs(x = "Diabetes Risk",y = "Stabilized Glucose", 
       title= "Stabilzed Glucose against Diabetes Risk")


df <- subset(df, select = -glyhb)

result = lm(diabetes~., data = df)
summary(result)

res <- cor(df)
round(res, 2)


################################################
##### variable selection process

regnull <- lm(glyhb~1, data=df)
##model with all predictors
regfull <- lm(glyhb~., data=df)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

glyline <-lm(glyhb~., data=df)
summary(glyline)

glyline$residuals

anova.tab <- anova(glyline)
anova.tab

acf(glyline)



###################################



ggplot(df, aes(x=stab.glu ,y= glyhb))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Stabilized Glucose", y="Glycosylated Hemoglobin", 
        title="Glycosylated Hemoglobin against Stabilized Glucose Scatter Plot")

gly_stab_line <-lm(glyhb~stab.glu, data=df)
summary(gly_stab_line)


df$yhat<-gly_stab_line$fitted.values
df$residual<-gly_stab_line$residuals

ggplot(df, aes(x=yhat ,y= residual))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="yhat", y="Residual", title="Residual Plot")

qqnorm(df$glyhb, pch = 1, frame = FALSE)
qqnorm(df$yhat, pch = 1, frame = FALSE)
qqnorm(df$logyhat, pch = 1, frame = FALSE)

############################################################3










ggplot(df, aes(x=chol ,y= glyhb, color=location))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="Cholesterol", color= "Location", title="Cholesterol against Glycosolated Hemoglobin with Gender")






qqnorm(df$glyhb, pch = 1, frame = FALSE)







##################################################
df<-drop_na(chol, data = df)






pairs(df, lower.panel = NULL, main="Scatterplot of Quantitative Variables")

round(cor(df),3)



which.max(summary(glyline)$adjr2)
which.min(summary(glyline)$cp)
which.min(summary(glyline)$bic)














ggplot(train, aes(x=chol, y= diabetes))+
  geom_boxplot()+
  labs(x="Glycosolated Hemoglobin", y="Age",
       title="Age by Glycosolated Hemoglobin")
bp2<-ggplot(df, aes(x=glyhb, y=chol))+
  geom_boxplot()+
  labs(x="CHD", y="Systolic", title="Systolic BP by CHD")
bp3<-ggplot(df, aes(x=glyhb, y=weight))+
  geom_boxplot()+
  labs(x="CHD", y="Diastolic", title="Diastolic BP by CHD")
bp4<-ggplot(df, aes(x=glyhb, y=))+
  geom_boxplot()+
  labs(x="CHD", y="Cigarettes", title="Cigs by CHD")







ggplot(df, aes(x=glyhb ,y= weight))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="weight", 
       title="Cholesterol against Glycosolated Hemoglobin")



glyline <-lm(glyhb~., data=df)
summary(glyline)


acf(glyline)







df$logglyhb=log(df$glyhb)

ggplot(df, aes(x=logglyhb ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="Cholesterol", title="Cholesterol against Glycosolated Hemoglobin")




ggplot(df, aes(x=glyhb ,y= chol, color = gender))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="Cholesterol", title="Cholesterol against Glycosolated Hemoglobin", color = "Gender")








##################################################################################################


ggplot(df, aes(x=weight ,y=ratio))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="Ratio of Cholesterol to HDL", title="Ratio against Weight")


ggplot(df, aes(x=hdl ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="High Density Lipotene", y="Cholesterol", title="High Density Lipotene against Glycosolated Hemoglobin")

ggplot(df, aes(x=chol, color = gender))+
  geom_boxplot()+
  labs(x = "Cholesterol", title= "Cholesterol Box Plot by Gender", 
       color = "Gender")

ggplot(df, aes(x = weight, y = chol))
  
  
df1_complete <- na.omit(df)
  


ggplot(df, aes(x=weight ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="Cholesterol", title="Cholesterol against Weight Scatter Plot")

ggplot(df, aes(x=bmi, color = diabetes))+
  geom_histogram(na.rm=TRUE)+
  labs(x = "Body Mass Index", color = "Diabetes")

ggplot(df, aes(x= chol))+
  geom_histogram()+
  labs(x = "Cholesterol (mg/dL)", y = "Total Observations", title = "Cholesterol Histogram")

ggplot(df, aes(x = weight, y = ratio, color = gender))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)+
  labs(x = "Weight", y = "Cholesterol to HDL Ratio", color = "Gender", title = "Cholesterol to HDL Ratio against Weight between Genders")

df$logratio=log(df$ratio)


ggplot(df, aes(x = ratio, y = weight,))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)+
  labs(x = "Ratio between Cholesterol and HDL", y = "Weight", title = "Ratio against Weight")

ratio_weight <-lm(ratio~weight, data=df).
summary(ratio_weight)

df<-drop_na(chol,weight,glyhb, data = df)

df$yhat<-ratio_weight$fitted.values
df$residual<-ratio_weight$residuals

ggplot(df, aes(x=yhat ,y=residual))+
  geom_point(shape=5)+
  geom_hline(yintercept=0, color="red")+
  labs(x="yfit", y="Residual", title="Residual Plot of Weight to Cholesterol")










ggplot(df, aes(x = chol, color= gender))+
  geom_histogram()+
  labs(x = "Cholesterol", y = "Total", title = "Cholesterol Levels Histogram")



correlations <- cor(df)



df<-drop_na(chol,weight,glyhb, data = df)

df_line <- lm(chol~weight, data=df)
summary(df_line)

all_line <- lm(glyhb~., data= df)
summary(all_line)

acf(all_line)

complete.cases()
hdl_line <- lm(ratio~., data = df)
summary(hdl_line)

acf(hdl_line)

df$yhat<-df_line$fitted.values
df$residual<-df_line$residuals

ggplot(df, aes(x=yhat ,y=residual))+
  geom_point(shape=5)+
  geom_hline(yintercept=0, color="red")+
  labs(x="yfit", y="Residual", title="Residual Plot of Weight to Cholesterol")

ggplot(df, aes(x=))



gly <- cor.test(df$chol, df$glyhb, 
                method = "pearson")
gly

weight <-cor.test(df$chol, df$weight)
weight

weight<-cor.test(df$chol, df$weight)
weight

chol_line <- lm(chol~., data = df)
summary(chol_line)



df$bmi <- (df$weight / (df$height)^2) * 703

ggplot(df, aes(x=glyhb ,y= glyhb, color=location))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="ratio", y="bmi", title="bmi against hdl ratio")

choltobmi_line <- lm(ratio~bmi, data = df)
summary(choltobmi_line)

df <- subset(df, select = -id)

ggplot(df, aes(x = weight, y = glyhb, color = gender))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_x_continuous(n.breaks=10)+
  labs(x="Weight", y = "Glycosolated Hemoglobin",
       title = "Weight against Glycosolated Hemoglobin", color = "Gender")



weight (lb) / height (in)^2 * 703


anova(df_line)

summary(df$diabetes)


pairs(df, lower.panel = NULL)

59/329


ggplot(df, aes(x=weight ,y= hdl))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="HDL", title="High Density Lipotene against Weight")




ggplot(df, aes(x=age, y = weight))+
  geom_point(shape=5)+
  geom_smooth(method="lm", se=FALSE)+
  labs(x="Age", y= "Weight", title = "Weight Against Age")


hdl_line <- lm(hdl~., data = df)
summary(hdl_line)

acf(hdl_line)

regnull <- lm(hdl~1, data=df)
##model with all predictors
regfull <- lm(hdl~., data=df)
step(regfull, scope=list(lower=regfull, upper=regnull), direction="backward")

df$diabetes <- df$glyhb > 7

ggplot(df, aes(x=chol, color = diabetes))+
  geom_histogram()+
  labs(x="Diabetes", title = "Diabetes by Gender")

ggplot(df, aes(x=diabetes, y = hdl))+
  geom_boxplot(method="lm", se=FALSE)+
  labs(x="Diabetes", y= "HDL", title = "Choleasdferoasdf")


ggplot(df, aes(x=diabetes))+
  geom_bar()+
  labs(x="Diabetes", y= "Total", title = "Positive Diabetes Tests")

ggplot(df, aes(x=bmi, y = weight, color = gender))+
  geom_point(shape=5)+
  geom_smooth(method="lm", se=FALSE)+
  labs(x="Weight", y= "Cholesterol", title = "Cholesterol against Weight and Gender")

ggplot(df, aes(x=diabetes, y = chol))+
  geom_boxplot()+
  labs(x="Diabetes Risk", y= "Cholesterol", title = "Cholesterol against Diabetes Risk")



ggplot(df, aes(x=diabetes, fill = gender))+
  geom_bar()+
  labs(x = "Diabetes Risk", y = "Total Observations",
       title = "Diabetes Risk by Gender", fill = "Gender")
  
  
  
  
summary(df$hip)
summary(df$waist)
summary(df$age)
summary(df$gender)
summary(df$height)
mean(df$height)
summary(df$weight)
summary(df$frame)
