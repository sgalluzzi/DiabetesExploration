# DiabetesExploration
An exploration of diabetes predictors.
According to the CDC, over 34 million US adults have diabetes. As the seventh leading cause of death in the US, diabetes is a public health problem that has reached epidemic proportions. This disease can lead to other health problems including kidney failure, cardiovascular disease, and nerve damage. The most common test to determine if someone has diabetes is an A1C test. An A1C test is used to measure the glycosylated hemoglobin levels within the bloodstream. When sugar builds up in the bloodstream and attaches itself to the hemoglobin within the red blood cells, there is a resulting increase in glycosylated hemoglobin. According to the data, a glycosylated hemoglobin value greater than 7% is considered a positive diabetes test.  

The aim of our exploration is to gain an understanding of the predictors associated with diabetes. Identifying predictors of this condition can increase awareness, encourage preventative treatment, and lower the number of people at risk. To determine the most accurate predictors, we first visualized the data and then performed model selection techniques (stepwise regression and forward selection).

The dataset explored is the diabetes dataset from the faraway package in R. It contains 403 objects and 19 variables and was obtained by interviewing African Americans to “understand the prevalence of obesity, diabetes, and other cardiovascular risk factors in central Virginia.” We specifically chose this dataset for its large number of categorical and numerical variables and our interest in educating central Virginian communities about diabetes. One drawback that had to be accounted for was the small size of the data set. Some predictors within the data, such as cholesterol and age, shared some relationship with our response variable glycosylated hemoglobin. However, the predictor most correlated with glycosylated hemoglobin proved to be stabilized glucose. Adding additional predictors did little to increase the r-squared of our regressions. Therefore, more complex models were not preferred.

The visualizations that we explored included a histogram of glycosylated hemoglobin with a positive skew, and multiple scatter plots and box plots that further indicated the relationship between glycosylated hemoglobin and stabilized glucose. The linear regression between the two variables was y=2.2351+0.0312x with an R^2 of 0.560.   Approximately 14% of African Americans within our dataset are considered at risk of diabetes.
