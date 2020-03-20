# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("C:/Users/seacr/Desktop/Germany/2019SS/Thesis/Analysis")

#packages
library(caret) #train() function
library(caTools) #splitting datasets
library(haven) #reading dta file
library(dplyr) #data manipulation
library(MatchIt) #matching
library(gridExtra) #grid graphics
library(e1071) #svm
library(stats) #logit
library(ggplot2) #graphs
library(ggthemes) #graphs
library(cobalt) #love plot not necessary
library(ElemStatLearn) #ridge not necesary
library(randomForest) #RF
library(stringr) #data manipulation 
library(stringi) #data manipulation
library(xgboost) #xgboost
library(broom) #for result table
library(DMwR) #knn imputation
library(sandwich) #result analysis
library(lmtest) #result analysis
"=================================================================================="
"=============================CLEANING THE DATA===================================="
"=================================================================================="
#reading the dta file
og_data <- read_dta("test_west2018.dta") %>% as.data.frame()
dataset <- og_data %>% as.data.frame()
#rm(og_data)

# length of spell with full response: k
spell_length <- 8

#restricting to nursing and caring sector
dataset <- dataset %>% filter(w08_2>=86 & w08_2<90)

# beginning, end of spell
start_year <- 2007
end_year <- 2014
# treatment year, match year=treatment year - 2
treatment_year <- max(dataset$treatment_year, na.rm = T)
match_year <-  treatment_year-2
scope_year_end <- treatment_year+3
scope_year_start <- treatment_year-3
#summary(dataset$treatment_year)
#summary(dataset$match_year)

#sorting by crefonummer and year
dataset <- dataset %>% arrange(crefonummer, year)

#keeping only the obesrvations in the treatment year - 2 & treatment year + 2
dataset <- dataset %>% filter(year<=scope_year_end)
dataset <- dataset %>% filter(year>=scope_year_start)

#adding labels to the variables
attr(dataset$ln_sachanlage_pemp_nn1, "label") <- "log tangible assest"
attr(dataset$mw, "label") <- "treatment dummy variable"
attr(dataset$cap_int_nn1, "label") <- "total fixed asset / total balance sheet"
attr(dataset$inv_int_nn1, "label") <- "total fixed asset / total current asset"
attr(dataset$ek_quote_nn1, "label") <- "total equity / total balance sheet"
attr(dataset$ln_verbindlichkeiten_nn1, "label") <- "log total liability"
attr(dataset$ln_bilanzsumme_nn1, "label") <- "log total balance sheet"
attr(dataset$ln_alter, "label") <- "log firm age"

#renaming the column names for convenience
dataset <- dataset %>% rename(total_num_workers_year=anzahldermitarbeiterjahr)
dataset <- dataset %>% rename(total_asset_value_year=bilanzsummeeurjahr)
dataset <- dataset %>% rename(fixed_assets_year=anlagevermögeneurjahr)
dataset <- dataset %>% rename(intangible_assets_year=immvermögeneurjahr)
dataset <- dataset %>% rename(edp_software_year=edvsoftwareeurjahr)
dataset <- dataset %>% rename(fixed_asset_year=sachanlageneurjahr)
dataset <- dataset %>% rename(tech_equip_machine_year=techanlagenmascheurjahr)
dataset <- dataset %>% rename(office_equip_year=anlagenbgaeurjahr)
dataset <- dataset %>% rename(current_asset_year=umlaufvermögeneurjahr)
dataset <- dataset %>% rename(raw_materials_year=rhbeurjahr)
dataset <- dataset %>% rename(unfinished_goods_year=unfertigerzeugeurjahr)
dataset <- dataset %>% rename(finished_goods_year=fertigeerzeugeurjahr)
dataset <- dataset %>% rename(inventories_year=vorräteeurjahr)
dataset <- dataset %>% rename(material_costs_year=materialaufwandgkveurjahr)
dataset <- dataset %>% rename(salary_costs_year=personalaufwandgkveurjahr)
dataset <- dataset %>% rename(depreciation_costs_year=abschreibungengkveurjahr)
dataset <- dataset %>% rename(other_expenses_year=sonstigeaufwgkveurjahr)
dataset <- dataset %>% rename(revenues_year=umsatzerlöseeurjahr)
dataset <- dataset %>% rename(profit_year=gewinneurjahr)
dataset <- dataset %>% rename(balance_sheet_date=bilanzstichtagjahr)
dataset <- dataset %>% rename(industry_code=wz2008haupttätigkeitcode)
dataset <- dataset %>% rename(total_equity=eigenkapitaleurjahr)
dataset <- dataset %>% rename(total_liabilities=verbindlichkeiteneurjahr)
dataset <- dataset %>% rename(listed_on_stock_exchange=börsennotierung) #99.69% not listed
dataset <- dataset %>% rename(company_type=gesellschaftsform) #GMBH and etc.
dataset <- dataset %>% rename(solvency_status=solvenzstatus)

#transforming into log values
dataset <- dataset %>%
  mutate(ln_intangible_assets = log(intangible_assets_year))

dataset <- dataset %>%
  mutate(ln_software = log(edp_software_year))

dataset <- dataset %>%
  mutate(ln_tech_equip_machine = log(tech_equip_machine_year))

dataset <- dataset %>%
  mutate(ln_office_equip = log(office_equip_year))

dataset <- dataset %>%
  mutate(ln_raw_materials = log(raw_materials_year))

dataset <- dataset %>%
  mutate(ln_unfinished_goods = log(unfinished_goods_year))

dataset <- dataset %>%
  mutate(ln_finished_goods = log(finished_goods_year))

dataset <- dataset %>%
  mutate(ln_material_costs = log(material_costs_year))

dataset <- dataset %>%
  mutate(ln_depreciation_costs = log(depreciation_costs_year))

#constructing a ln_salary column
dataset <-  dataset %>% mutate(ln_salary = log(salary_costs_year))

dataset <- dataset %>%
  mutate(ln_other_expenses = log(other_expenses_year))

dataset <- dataset %>%
  mutate(ln_revenues = log(revenues_year))

dataset <- dataset %>%
  mutate(ln_profit = ifelse(is.na(profit_year), NA, 
                            ifelse(profit_year>=0,log(profit_year), -log(-profit_year))))
dataset <- dataset %>%
  mutate(ln_total_equity = log(total_equity))

#transforming -infinity values, which are generated as the original value before transformin into log values were zeros!, into 0
dataset <- dataset %>%
  mutate(ln_intangible_assets = ifelse(is.infinite(ln_intangible_assets),
                                       0, ln_intangible_assets))

dataset <- dataset %>%
  mutate(ln_software = ifelse(is.infinite(ln_software),
                              0, ln_software))

dataset <- dataset %>%
  mutate(ln_tech_equip_machine = ifelse(is.infinite(ln_tech_equip_machine),
                                        0, ln_tech_equip_machine))

dataset <- dataset %>%
  mutate(ln_office_equip = ifelse(is.infinite(ln_office_equip),
                                  0, ln_office_equip))

dataset <- dataset %>%
  mutate(ln_raw_materials = ifelse(is.infinite(ln_raw_materials),
                                   0, ln_raw_materials))

dataset <- dataset %>%
  mutate(ln_unfinished_goods = ifelse(is.infinite(ln_unfinished_goods),
                                      0, ln_unfinished_goods))

dataset <- dataset %>%
  mutate(ln_finished_goods = ifelse(is.infinite(ln_finished_goods),
                                    0, ln_finished_goods))

dataset <- dataset %>%
  mutate(ln_material_costs = ifelse(is.infinite(ln_material_costs),
                                    0, ln_material_costs))

dataset <- dataset %>%
  mutate(ln_depreciation_costs = ifelse(is.infinite(ln_depreciation_costs),
                                        0, ln_depreciation_costs))

dataset <- dataset %>% mutate(ln_salary = ifelse(is.infinite(ln_salary),
                                                 0, ln_salary))

dataset <- dataset %>%
  mutate(ln_other_expenses = ifelse(is.infinite(ln_other_expenses),
                                    0, ln_other_expenses))

dataset <- dataset %>%
  mutate(ln_revenues = ifelse(is.infinite(ln_revenues),
                              0, ln_revenues))

dataset <- dataset %>%
  mutate(ln_profit = ifelse(is.infinite(ln_profit),
                            0, ln_profit))
dataset <- dataset %>%
  mutate(ln_total_equity = ifelse(is.infinite(ln_total_equity),
                                  0, ln_total_equity))
#a vector of variables which I believe they are important
total_var <- c(
  "crefonummer",
  "year",
  "mw",
  "ln_emp_nn1", #ln_emp_nn1
  "ln_bilanzsumme_nn1", #ln_bilanzsumme_nn1
  "ln_anlagevermögen_nn1",      #ln_anlagevermögen_nn1
  "ln_intangible_assets", #log()
  "ln_software",      #log()
  "ln_sachanlagen_nn1",         #ln_sachanlagen_nn1
  "ln_tech_equip_machine",#log()
  "ln_office_equip",      #log()
  "ln_umlaufvermögen_nn1",     #ln_umlaufvermögen
  "ln_raw_materials",     #log()
  "ln_unfinished_goods",  #log()
  "ln_finished_goods",    #log()
  "ln_vor_nn1",       #ln_vor_nn1
  "ln_material_costs",    #log()
  "ln_salary",      #ln_salary
  "ln_depreciation_costs",#log()
  "ln_other_expenses",    #log()
  "ln_revenues",          #log()
  "ln_profit",            #log()
  "ln_total_equity",           
  "ln_verbindlichkeiten_nn1",      #ln_verbindlichkeiten
  "company_type",
  "solvency_status",
  "cap_int_nn1",
  "inv_int_nn1",
  "ek_quote_nn1",
  "ln_alter",
  "urban"
)


# Encoding the outcome variable as a factor variable
dataset$mw = factor(dataset$mw, levels = c(0, 1))
#dataset = dataset[, var]

#creating other datasets
og_data <- dataset #preserving the original dataset with all variables
dataset <- dataset %>% filter(year==match_year) #for doing the matching
result_data <- og_data #the result dataset with only the variables which would be used in matching
"##################################################################################"
"####################DATA MANIPULATION#############################################"
"##################################################################################"

#including only GmbH and GmbH & Co. KG
company_type_idx_a <- dataset$company_type == "GmbH & Co. KG"
company_type_idx_b <- dataset$company_type == "Gesellschaft mit beschränkter Haftung"
company_type_idx <- as.logical(company_type_idx_a + company_type_idx_b)
rm(company_type_idx_a, company_type_idx_b)

dataset <- dataset[company_type_idx,]

#checking whether all companies exist in all time periods!
for(y in sort(unique(og_data$year))){
  temp_og <- og_data %>% filter(year==y)
  check <- dataset$crefonummer %in% temp_og$crefonummer
  
  print(summary(check==T))
  rm(temp_og)
} #some companies do not exist in 2012 and 2013
temp_2013 <- og_data %>%  filter(year==2013)
company_idx_2013 <- temp_2013$crefonummer
for(y in sort(unique(og_data$year))){
  temp_og <- og_data %>% filter(year==y)
  check <- company_idx_2013 %in% temp_og$crefonummer
  
  print(summary(check==T))
  rm(temp_og)
}
rm(temp_2013, check)

#getting only companies which operates for the whole time period
company_idx_2013 <- dataset$crefonummer %in% company_idx_2013
dataset <- dataset[company_idx_2013,]

og_data <- og_data[og_data$crefonummer %in% dataset$crefonummer, ]
result_data <- result_data[result_data$crefonummer %in% dataset$crefonummer, ]
#summary(og_data$crefonummer %in% dataset$crefonummer)
"=================================================================================="
#writing temporary rds file for time efficiency
save.image(file = "cleaned_west_germany2018.RData")
load("cleaned_west_germany2018.RData")
"=================================================================================="

"##################################################################################"
"####################COVARIANCE BALANCE CHECKING FUCNTION##########################"
"##################################################################################"
matching_var <- function(candidates, num){
  match_comb <- combn(candidates, num) #creating all the possible combinations by choosing the number of "num" variables
  comb <- length(match_comb) / num #number of combinations
  print(comb)
  for(i in 1:comb){
    var <- append(match_comb[,i], c("mw", "ln_sachanlage_pemp_nn1")) #creating a vector of all variables
    
    temp_data <- dataset[, var]
    
    #Fitting Logistic Regression to the whole dataset
    classifier <- glm(formula = mw ~.,
                      family = binomial,
                      data = temp_data)
    
    #retreiving the propensity score
    prs_df <- data.frame(pr_score = predict(classifier, type = "response"),
                         mw = classifier$model$mw)
    #elminating missing values
    dataset_nomiss <- temp_data %>% 
      select(ln_sachanlage_pemp_nn1, mw, one_of(match_comb[,i])) %>%
      na.omit()
    
    #matching without replacements
    fmla <- paste("mw ~", paste(match_comb[,i], collapse = " + "))
    print(fmla)
    mod_match <- matchit(formula = as.formula(fmla),
                         method = "nearest", 
                         distance = prs.df$pr_score,
                         data = dataset_nomiss, 
                         replace = F)
    
    #assessing balance on the covariates by saving the love_plot image files
    plot_name <- paste0("love_plot_", num, "_", i,".jpg")
    love.plot(bal.tab(mod_match), threshold = .1)
    ggsave(plot_name, width = 5, height = 5)
  }
}
"##################################################################################"
"##################################################################################"
"##################################################################################"

"=================================================================================="
"=================Checking what are the good matching variables===================="
"=================================================================================="
for(i in 2:length(covar)){
  matching_var(covar,i)  
}


"=================================================================================="
"======================= Pre-check ================================================"
"=================================================================================="
#checking difference-in-means: outcome variable
dataset %>%
  group_by(mw) %>%
  summarise(n_firms = n(),
            mean_tangible_asset = mean(ln_sachanlage_pemp_nn1),
            std_error = sd(ln_sachanlage_pemp_nn1) / sqrt(n_firms))
t.test(dataset$ln_sachanlage_pemp_nn1 ~ dataset$mw)

#checking difference-in-means: pre-treatment covariates
dataset %>%
  group_by(mw) %>%
  select(one_of( covar ) ) %>%
  summarise_all(list(~mean(., na.rm = T)))

#Welch two sample t-test
lapply(covar, function(v) {
  t.test(dataset[[v]] ~ dataset$mw)
})

#data visualization
pl_cap <- dataset %>% ggplot(aes(x = cap_int_nn1)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_cap)

pl_liability <- dataset %>% ggplot(aes(x = ln_verbindlichkeiten_nn1)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_liability)

pl_balance <- dataset %>% ggplot(aes(x = ln_bilanzsumme_nn1)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_balance)

pl_age <- dataset %>% ggplot(aes(x = ln_alter)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_age)

pl_ek <- dataset %>% ggplot(aes(x = ek_quote_nn1)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_ek)

pl_salary <- dataset %>% ggplot(aes(x = ln_salary)) + 
  geom_histogram(aes(y = ..density.., color = mw, fill = mw)) + 
  geom_density(aes(y=..density..)) + 
  facet_grid(. ~ mw)
print(pl_salary)

"=================================================================================="
"======================= ML PREPERATION ==========================================="
"=================================================================================="
#loading pre-stored data
load("cleaned_west_germany2018.RData")

#checking each variable whether they have missing values more than 30% of total observation
bad <- list()
for(i in 1:length(names(dataset))){
  threshold <- round(length(dataset[,1])*0.3)
  idx <- match("TRUE", names(summary(is.na(dataset[, i])==T)))
  
  if(is.na(idx)==F){#if corresponding column has some missing values
    if(as.numeric(summary(is.na(dataset[,i])==T)[idx])>threshold){
      bad <- append(bad, names(dataset)[i])
    }
  }
}
bad <- bad %>% 
  unlist() %>% 
  as.character()

#defining covariates
covar = c("cap_int_nn1","ln_verbindlichkeiten_nn1", "ln_bilanzsumme_nn1", "ln_alter", "urban")

dataset <- dataset %>%  
  select(crefonummer, year, ln_sachanlage_pemp_nn1, mw, one_of(covar), w08_5) %>%
  na.omit() %>%
  as.data.frame()
#result_data <- result_data %>%  
#  select(crefonummer, year, ln_sachanlage_pemp_nn1, mw, one_of(covar), w08_5) %>%
#  na.omit() %>%
#  as.data.frame()

#checking whether all companies exist in all time periods!
for(y in sort(unique(og_data$year))){
  temp_og <- result_data %>% filter(year==y)
  check <- dataset$crefonummer %in% temp_og$crefonummer
  
  if(F %in% check){#removing companies which does not exist in corresponding year
    dataset <- dataset[check,]
  }
  print(summary(check==T))
  rm(temp_og)
}

#removing corresponding observations in matching dataset and result dataset
check <- result_data$crefonummer %in% dataset$crefonummer
result_data <- result_data[check, ] 
rm(company_idx_2013, check)

#a dataset for non-parametric methods
non_para_data <- og_data %>%
  select(total_var, ln_sachanlage_pemp_nn1, w08_5)
non_para_data <- non_para_data %>%
  mutate(crefonummer = factor(crefonummer))
bad <- bad[bad %in% names(non_para_data)]
non_para_data <- non_para_data %>%  
  select(-bad) #removing variables which has missing variables more than 30% of total observations in year 2008
non_para_data <- non_para_data %>%  
  select(-company_type, -solvency_status) #removing them due to an error message in RF "These variables have zero variance"

non_para_crefo <- non_para_data$crefonummer
non_para_data <- non_para_data %>%  
  select(-crefonummer, -w08_5) #temporaily removing crefonummer, removing  industry code as it is not important anymore

#non_para_data <- centralImputation(non_para_data) #median imputation
non_para_data <- knnImputation(non_para_data, k=1)
non_para_data <- cbind(crefonummer = non_para_crefo, non_para_data)

#only retrieving 2008 data
non_para_data <- non_para_data %>%  
  filter(year==2008)

#checking NA values
for(i in 1:length(names(non_para_data))){
  print(names(non_para_data)[i])
  print(summary(is.na(non_para_data[, i])==T))
}

#forcing non_para_data to have the same observations as dataset!
non_para_data <- non_para_data[non_para_data$crefonummer %in% dataset$crefonummer, ]

#mean_impute <- function(column){
#  idx <- column %in% NA
#  column[idx] <- mean(column, na.rm = T)
#  return(column)
#}
#for(i in 1:length(names(non_para_data))){
#  non_para_data[[i]] <- mean_impute(non_para_data[[i]])
#}

# Splitting the dataset into the Training set and Test set
set.seed(123)
#split = sample.split(dataset$mw, SplitRatio = 0.7)
#for dataset
#dataset = subset(dataset, split == TRUE) %>% 
#as.data.frame()
#dataset = subset(dataset, split == FALSE) %>% 
#as.data.frame()
##for non_para_dataset
#dataset_non_para = subset(non_para_data, split == TRUE) %>% 
#as.data.frame()
#dataset_non_para = subset(non_para_data, split == FALSE) %>% 
#as.data.frame()

#the equation
ml_formula <- paste("mw", "~", paste( covar, collapse = " + ") ) %>%
  formula()#the formula

#fold for k-cross validation
#folds = createFolds(dataset$mw, k = 10)
folds = createFolds(dataset$mw, k = 10)

#splitting the dataset into urban, rural datasets
#dataset_urban <- dataset %>% filter(urban==1) %>% as.data.frame()
#dataset_rural <- dataset %>% filter(urban==0) %>% as.data.frame()

#sorting all datasets by crefonummer
dataset <- dataset %>% arrange(crefonummer)
#dataset_rural <- dataset_rural %>% arrange(crefonummer)
#dataset_urban <- dataset_urban %>% arrange(crefonummer)

#creating indices indicating which firms are in the dataset for matching year & 2 yrs after treatment year depedning on the area
match_idx <- og_data$crefonummer %in% dataset$crefonummer 
FALSE %in% match_idx

#the index for mw variable in the dataset
mw_idx <- match("mw", names(dataset))

#checking
#0 %in% dataset$mw[match_idx]
#sum(match_idx==T)

"================================================================================================================="
"=========================== MATCHING ============================================================================"
"================================================================================================================="

"A fucntion to calculate propensity scores with different methods"
matching <- function(method_input="logit"){
  #vector of methods
  method <- method_input
  if(stri_cmp_eq(method, "svm")){
    #doing grid search to find the best parameter for ML
    #classifier = train(form = ml_formula, data = dataset, method = 'svmLinear2')
    #gridsearch <- classifier$bestTune
    
    classifier = svm(formula = ml_formula,
                     data = dataset,
                     type = 'C-classification',
                     kernel = 'linear',
                     probability = TRUE,
                     cost = 0.25)
    #retreiving the propensity score
    pred <- predict(classifier, dataset[-mw_idx], probability = T)
    prob_pred <- attr(pred, "probabilities")[, 1]
    
    #head(attr(pred, "probabilities"))
    
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = dataset$mw)
    # Applying k-Fold Cross Validation
    cv = lapply(folds, function(x) {
      training_fold = dataset[-x, ]
      test_fold = dataset[x, ]
      classifier = svm(ml_formula,
                       data = training_fold,
                       type = "C-classification",
                       kernel = 'linear',
                       probability = TRUE,
                       cost = 0.25)
      
      y_pred = predict(classifier, newdata = test_fold[-mw_idx])
      cm = table(test_fold[, mw_idx], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)
    })
    cv
    accuracy = mean(as.numeric(cv))
    accuracy
    print(var(as.numeric(cv)))
  }
  if(stri_cmp_eq(method, "ksvm")){
    #doing grid search to find the best parameter for ML
    #classifier = train(form = ml_formula, data = dataset, method = 'svmRadial')
    #gridsearch <- classifier$bestTune
    
    classifier = svm(formula = ml_formula,
                     data = dataset,
                     type = 'C-classification',
                     kernel = 'radial',
                     probability = TRUE,
                     gamma = 1.730628,
                     cost = 0.25)
    #retreiving the propensity score
    pred <- predict(classifier, dataset[-mw_idx], probability = T)
    prob_pred <- attr(pred, "probabilities")[, 1]
    
    #head(attr(pred, "probabilities"))
    
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = dataset$mw)
    #head(prob.df)
    
    # Applying k-Fold Cross Validation
    cv = lapply(folds, function(x) {
      training_fold = dataset[-x, ]
      test_fold = dataset[x, ]
      classifier = svm(ml_formula,
                       data = training_fold,
                       type = "C-classification",
                       kernel = 'radial',
                       probability = TRUE,
                       gamma = 1.730628,
                       cost = 0.25
      )
      y_pred = predict(classifier, newdata = test_fold[-mw_idx])
      cm = table(test_fold[, mw_idx], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)
    })
    cv
    accuracy = mean(as.numeric(cv))
    accuracy
    print(var(as.numeric(cv)))
  }
  if(stri_cmp_eq(method, "logit")){
    classifier <- glm(form = ml_formula,
                      family = binomial(),
                      data = dataset)
    #retreiving the propensity score
    prob_pred <- predict(classifier, dataset[-mw_idx], type = "response")
    pred <- ifelse(prob_pred > 0.5, 1, 0)
    
    head(prob_pred)
    
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = dataset$mw)
    # Applying k-Fold Cross Validation
    cv = lapply(folds, function(x) {
      training_fold = dataset[-x, ]
      test_fold = dataset[x, ]
      classifier = glm(form = ml_formula,
                       family = binomial(),
                       data = training_fold
      )
      prob_pred <- predict(classifier, test_fold[-mw_idx], type = "response")
      cm = table(test_fold[, mw_idx], prob_pred > 0.5)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)  
    })
    cv
    accuracy = mean(as.numeric(cv))
    accuracy
    print(var(as.numeric(cv)))
  }
  if(stri_cmp_eq(method, "rf")){
    temp_dataset <- dataset %>% select(-crefonummer, -year, -w08_5)
    mw_idx <- match("mw", names(temp_dataset))
    
    #doing grid search to find the best parameter for ML
    
    #classifier <- train(form = ml_formula, data = temp_dataset, method = 'rf', preProcess = "scale")
    #gridsearch <- classifier$bestTune
    
    classifier <- randomForest(ml_formula,
                               data = temp_dataset,
                               ntree = 100,
                               norm.votes = TRUE,
                               mtry = 2) #how many?
    prob_pred <- predict(classifier, dataset[-mw_idx], type = "prob")
    prob_pred <- prob_pred[,2]
    pred <- ifelse(prob_pred > 0.5, 1, 0)
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = dataset$mw)
    # Applying k-Fold Cross Validation
    
    cv = lapply(folds, function(x) {
      training_fold <- temp_dataset[-x, ]
      test_fold <- temp_dataset[x, ]
      classifier <- randomForest(ml_formula,
                                 data = training_fold,
                                 ntree = 100,
                                 norm.votes = TRUE,
                                 mtry = 2)
      
      prob_pred <- predict(classifier, test_fold[-mw_idx], type = "class")
      #prob_pred <- prob_pred[,2]
      #y_pred <- ifelse(prob_pred > 0.5, 1, 0)
      if(0 %in% prob_pred){
        cm = table(test_fold[, mw_idx], prob_pred == 1)
        accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
        return(accuracy)  
      }
      if(!(0 %in% prob_pred)){
        return(1)  
      }
    })
    cv
    accuracy = mean(as.numeric(cv), na.rm = T)
    accuracy
    print(var(as.numeric(cv), na.rm = T))
  }
  
  if(stri_cmp_eq(method, "xgboost")){
    temp_dataset <- dataset %>% select(-crefonummer, -year, -w08_5)
    mw_idx <- match("mw", names(temp_dataset))
    
    #classifier <- train(form = ml_formula, data = temp_dataset, method = 'xgbTree', preProcess = "scale")
    #gridsearch <- classifier$bestTune %>% as.list()
    opt_nrounds <- 100
    gridsearch <- list(max_depth = 1,
                       eta = 0.4,
                       gamma = 0,
                       colsample_bytree = 0.6,
                       min_child_weight=1,
                       subsample =1)
    
    classifier = xgboost(data = as.matrix(temp_dataset[-mw_idx]), label = as.vector(temp_dataset$mw), nrounds = opt_nrounds, params = gridsearch, objective = "binary:logistic")
    
    #retreiving the propensity score
    prob_pred <- predict(classifier, newdata = as.matrix(temp_dataset[-mw_idx]), outputmargin = F)
    pred <- ifelse(prob_pred > 0.5, 1, 0)
    
    
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = dataset$mw)
    # Applying k-Fold Cross Validation
    cv <- xgb.cv(data = as.matrix(temp_dataset[-mw_idx]), label = as.vector(temp_dataset$mw), nrounds = 10, 
                 objective = "binary:logistic", prediction = T, nfold = 10, folds = folds, metrics = 'error')
    cv_value <- cv$evaluation_log$test_error_mean
    
    cv_value
    accuracy = 1.0 - mean(cv_value)
    accuracy
    print(var(cv_value))
  }
  print(accuracy)
  print(classifier)
  #propensity scores of rural and urban
  #pr_score_rural <- prob_pred[dataset$urban %in% 0]
  #pr_score_urban <- prob_pred[dataset$urban %in% 1]
  
  #matching with nearest neighbor method(1) without replacement
  #rural
  #mod_match_rural <- matchit(ml_formula,
  #                           method = "nearest", data = dataset_rural, distance = pr_score_rural, replace = F)
  #dta_m_rural <- match.data(mod_match_rural)
  #urban
  #mod_match_urban <- matchit(ml_formula,
  #                           method = "nearest", data = dataset_urban, distance = pr_score_urban, replace = F)
  #dta_m_urban <- match.data(mod_match_urban)
  
  #scatter plots of propensity scores
  #plot(mod_match_urban, type = "jitter")
  #plot(mod_match_rural, type = "jitter")
  #histograms of propensity scores
  #plot(mod_match_urban, type = "hist")
  #plot(mod_match_rural, type = "hist")
  #mergin the two into one
  #dta_m <- data.frame(rbind(dta_m_rural, dta_m_urban))
  
  #matching with nearest neighbor method(1) without replacement
  mod_match <- matchit(ml_formula,
                       method = "nearest", data = dataset, distance = prob.df$pr_score, replace = F)
  dta_m <- match.data(mod_match)
  
  #histograms of propensity scores
  #plot(mod_match, type = "hist")
  #plots
  #plot(mod_match)
  #scatter plots of propensity scores
  #plot(mod_match, type = "jitter")
  
  #summary of mod_match
  sum_data <- summary(mod_match, standardize = T)
  #plot(sum_data)
  
  result <- list(sum_data, dta_m)
  return(result)
}

matching_non_para <- function(method_input="xgboost"){
  #methods <- c("rf", "xgboost")
  method <- method_input
  
  #temp data set for the function
  temp_dataset_non_para <- non_para_data %>% 
    select(-crefonummer, -ln_sachanlage_pemp_nn1, -year)
  
  #fold for k-cross validation
  non_para_folds = createFolds(non_para_data$mw, k = 10)
  
  #creating an idex variable for mw column
  non_mw_idx <- match("mw", names(temp_dataset_non_para))
  #formula
  non_para_formula <- paste("mw", "~", paste( names(temp_dataset_non_para)[2:length(names(temp_dataset_non_para))], collapse = " + ") ) %>%
    formula()#the formula
  
  if(stri_cmp_eq(method, "rf")){
    #removing features resulting in low Gini Impurity decrease(after feature selection)
    temp_dataset_non_para <- temp_dataset_non_para %>% 
      select(-urban)
    #formula
    non_para_formula <- paste("mw", "~", paste( names(temp_dataset_non_para)[2:length(names(temp_dataset_non_para))], collapse = " + ") ) %>%
      formula()#the formula
    
    #doing grid search to find the best parameter for ML
    #classifier = train(form = mw ~. , data = temp_dataset_non_para, method = 'rf', preProcess = "scale")
    #gridsearch <- classifier$bestTune
    
    classifier <- randomForest(mw ~.,
                               data = temp_dataset_non_para,
                               ntree = 100,
                               norm.votes = TRUE,
                               mtry = 7, #trained result
                               importance = T) #how many?
    
    #checking the feature importance
    importance(classifier)
    
    prob_pred <- predict(classifier, temp_dataset_non_para[-non_mw_idx], type = "prob")
    prob_pred <- prob_pred[,2]
    pred <- ifelse(prob_pred > 0.5, 1, 0)
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = temp_dataset_non_para$mw)
    
    cv = lapply(folds, function(x) {
      training_fold <- temp_dataset_non_para[-x, ]
      test_fold <- temp_dataset_non_para[x, ]
      classifier <- randomForest(mw ~. ,
                                 data = training_fold,
                                 ntree = 100,
                                 norm.votes = TRUE,
                                 mtry = 7)
      
      prob_pred <- predict(classifier, test_fold[-non_mw_idx], type = "class")
      #prob_pred <- prob_pred[,2]
      #y_pred <- ifelse(prob_pred > 0.5, 1, 0)
      if(0 %in% prob_pred){
        cm = table(test_fold[, non_mw_idx], prob_pred == 1)
        accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
        return(accuracy)  
      }
      if(!(0 %in% prob_pred)){
        return(1)  
      }
    })
    cv
    accuracy = mean(as.numeric(cv), na.rm = T)
    accuracy
    print(var(as.numeric(cv), na.rm = T))
    
  }
  
  if(stri_cmp_eq(method, "xgboost")){
    #removing features resulting in low Gini Impurity decrease(after feature selection)
    temp_dataset_non_para <- temp_dataset_non_para %>% 
      select(-urban)
    #formula
    non_para_formula <- paste("mw", "~", paste( names(temp_dataset_non_para)[2:length(names(temp_dataset_non_para))], collapse = " + ") ) %>%
      formula()#the formula
    
    #classifier <- train(form = mw ~., data = temp_dataset_non_para, method = 'xgbTree', preProcess = "scale")
    #gridsearch <- classifier$bestTune %>% as.list()
    opt_nrounds <- 50
    gridsearch <- list(max_depth = 3,
                       eta = 0.3,
                       gamma = 0,
                       colsample_bytree = 0.8,
                       min_child_weight=1,
                       subsample =0.75)
    
    mw_idx <- match("mw", names(temp_dataset_non_para))
    classifier = xgboost(data = as.matrix(temp_dataset_non_para[-mw_idx]), label = as.vector(temp_dataset_non_para$mw), nrounds = opt_nrounds, params = gridsearch, objective = "binary:logistic")
    
    feature_importance <- xgb.importance(model = classifier)
    xgb.plot.importance(feature_importance)
    
    #retreiving the propensity score
    prob_pred <- predict(classifier, newdata = as.matrix(temp_dataset_non_para[-non_mw_idx]), outputmargin = F)
    pred <- ifelse(prob_pred > 0.5, 1, 0)
    
    
    prob.df <- data.frame(pr_score = prob_pred,
                          mw = temp_dataset_non_para$mw)
    # Applying k-Fold Cross Validation
    cv <- xgb.cv(data = as.matrix(temp_dataset_non_para[-non_mw_idx]), label = as.vector(temp_dataset_non_para$mw), nrounds = 10, 
                 objective = "binary:logistic", prediction = T, nfold = 10, folds = non_para_folds, metrics = 'error')
    cv_value <- cv$evaluation_log$test_error_mean
    
    cv_value
    accuracy = 1.0 - mean(cv_value)
    accuracy
    print(var(cv_value))
  }
  
  
  #}
  print(accuracy)
  print(classifier)
  
  #matching with nearest neighbor method(1) without replacement
  mod_match <- matchit(non_para_formula, method = "nearest", data = non_para_data, distance = prob.df$pr_score, replace = F)
  dta_m <- match.data(mod_match)
  
  #plots
  #plot(mod_match)
  #scatter plots of propensity scores
  #plot(mod_match, type = "jitter")
  #histograms of propensity scores
  #plot(mod_match, type = "hist")
  
  #summary of mod_match
  sum_data <- summary(mod_match, standardize = T)
  #plot(sum_data)
  
  
  result <- list(sum_data, dta_m)
  return(result)
}
"=================================================================================="
#writing temporary rds file for time efficiency
save.image(file = "matching_split.RData")
load("matching_split.RData")
"=================================================================================="

"================================================================================================================="
"=========================== RESULT ============================================================================"
"================================================================================================================="
#result list
result_svm <- matching("svm")
result_ksvm <- matching("ksvm")
result_logit <- matching("logit")
result_rf <- matching("rf")
result_xgboost <- matching("xgboost")
result_rf_non_para <- matching_non_para("rf")
result_xgboost_non_para <- matching_non_para("xgboost")

#retrieving mathced data
dta_m_svm <- result_svm[[2]]
dta_m_ksvm <- result_ksvm[[2]]
dta_m_logit <- result_logit[[2]]
dta_m_rf <- result_rf[[2]]
dta_m_xgboost <- result_xgboost[[2]]
dta_m_rf_non_para <- result_rf_non_para[[2]]
dta_m_xgboost_non_para <- result_xgboost_non_para[[2]]

#retrieving summary statistics of matching
summary_svm <- result_svm[[1]]
summary_ksvm <- result_ksvm[[1]]
summary_logit <- result_logit[[1]]
summary_rf <- result_rf[[1]]
summary_xgboost <- result_xgboost[[1]]
summary_rf_non_para <- result_rf_non_para[[1]]
summary_xgboost_non_para <- result_xgboost_non_para[[1]]

#result data(2007-2013)
result_data_svm <- result_data[result_data$crefonummer %in% dta_m_svm$crefonummer, ]
result_data_ksvm <- result_data[result_data$crefonummer %in% dta_m_ksvm$crefonummer, ]
result_data_logit <- result_data[result_data$crefonummer %in% dta_m_logit$crefonummer, ]
result_data_rf <- result_data[result_data$crefonummer %in% dta_m_rf$crefonummer, ]
result_data_xgboost <- result_data[result_data$crefonummer %in% dta_m_xgboost$crefonummer, ]
result_data_rf_non_para <- result_data[result_data$crefonummer %in% dta_m_rf_non_para$crefonummer, ]
result_data_xgboost_non_para <- result_data[result_data$crefonummer %in% dta_m_xgboost_non_para$crefonummer, ]

"=================================================================================="
#writing temporary rds file for time efficiency
save.image(file = "result_split.RData")
load("result_split.RData")
"=================================================================================="
"================================================================================================================="
"=========================== RESULT ANALYSIS ============================================================================"
"================================================================================================================="
#creating an industry indicator: it has a value of one if an observation belongs to an industry which minimum wage was introduced.
treated_industry_code <- c(87100, 88101, 87300, 88102, 88100)

result_data_svm$industry_indicator <- 0
result_data_svm$industry_indicator[result_data_svm$w08_5 %in% treated_industry_code] <- 1

result_data_ksvm$industry_indicator <- 0
result_data_ksvm$industry_indicator[result_data_ksvm$w08_5 %in% treated_industry_code] <- 1

result_data_logit$industry_indicator <- 0
result_data_logit$industry_indicator[result_data_logit$w08_5 %in% treated_industry_code] <- 1

result_data_rf$industry_indicator <- 0
result_data_rf$industry_indicator[result_data_rf$w08_5 %in% treated_industry_code] <- 1

result_data_xgboost$industry_indicator <- 0
result_data_xgboost$industry_indicator[result_data_xgboost$w08_5 %in% treated_industry_code] <- 1

result_data_rf_non_para$industry_indicator <- 0
result_data_rf_non_para$industry_indicator[result_data_rf_non_para$w08_5 %in% treated_industry_code] <- 1

result_data_xgboost_non_para$industry_indicator <- 0
result_data_xgboost_non_para$industry_indicator[result_data_xgboost_non_para$w08_5 %in% treated_industry_code] <- 1

#changing crefonummer to a factor variable
result_data_svm <- result_data_svm %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_ksvm <- result_data_ksvm %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_logit <- result_data_logit %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_rf <- result_data_rf %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_xgboost <- result_data_xgboost %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_rf_non_para <- result_data_rf_non_para %>% 
  mutate(crefonummer = as.factor(crefonummer))
result_data_xgboost_non_para <- result_data_xgboost_non_para %>% 
  mutate(crefonummer = as.factor(crefonummer))

# estimate the regression
#capital-labor ratio = dependent variable

"==="
#SVM
result_data_svm_demeaned <- with(result_data_svm,
                                 data.frame(crefonummer = crefonummer,
                                            year = year,
                                            ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                            industry_indicator = industry_indicator 
                                 ))

analytical_result_demeaned_svm <- result_data_svm_demeaned %>% 
  group_by(year) %>%
  do(tidy(     coeftest( #robust standard errors    
    lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),  
    vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0")) 
  ) 
  )


"===="
#KSVM
result_data_ksvm_demeaned <- with(result_data_ksvm,
                                  data.frame(crefonummer = crefonummer,
                                             year = year,
                                             ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                             industry_indicator = industry_indicator 
                                  ))

analytical_result_demeaned_ksvm <- result_data_ksvm_demeaned %>% 
  group_by(year) %>%
  do(tidy(     coeftest( #robust standard errors  
    lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),   
    vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0")) 
  )  
  )

"====="
#Logit
result_data_logit_demeaned <- with(result_data_logit,
                                   data.frame(crefonummer = crefonummer,
                                              year = year,
                                              ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                              industry_indicator = industry_indicator 
                                   ))

analytical_result_demeaned_logit <- result_data_logit_demeaned %>% 
  group_by(year) %>%
  do(tidy(     coeftest( #robust standard errors  
    lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),  
    vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0")) 
  )  
  )



"======================="
#Random Forest(non_para)
result_data_rf_non_para_demeaned <- with(result_data_rf_non_para,
                                         data.frame(crefonummer = crefonummer,
                                                    year = year,
                                                    ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                                    industry_indicator = industry_indicator 
                                         ))

analytical_result_demeaned_rf_non_para <- result_data_rf_non_para_demeaned %>% 
  group_by(year) %>%
  do(tidy(     coeftest( #robust standard errors   
    lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),   
    vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0")) 
  )  
  )


"================="
#XGBOOST(non_para)
result_data_xgboost_non_para_demeaned <- with(result_data_xgboost_non_para,
                                              data.frame(crefonummer = crefonummer,
                                                         year = year,
                                                         ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                                         industry_indicator = industry_indicator 
                                              ))

analytical_result_demeaned_xgboost_non_para <- result_data_xgboost_non_para_demeaned %>% 
  group_by(year) %>%
  do(tidy(     coeftest( #robust standard errors   
    lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),    
    vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0")) 
  )  
  )



"============="
#RANDOM FOREST
result_data_rf_demeaned <- with(result_data_rf,
                                data.frame(crefonummer = crefonummer,
                                           year = year,
                                           ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                           industry_indicator = industry_indicator 
                                ))

analytical_result_demeaned_rf <- result_data_rf_demeaned %>% 
  group_by(year) %>%
  do(tidy( 
    coeftest( #robust standard errors  
      lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .),   
      vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0"))
  )   
  )



"======="
#XGBOOST
result_data_xgboost_demeaned <- with(result_data_xgboost,
                                     data.frame(crefonummer = crefonummer,
                                                year = year,
                                                ln_sachanlage_pemp_nn1 = ln_sachanlage_pemp_nn1 - ave(ln_sachanlage_pemp_nn1, crefonummer),
                                                industry_indicator = industry_indicator 
                                     ))

analytical_result_demeaned_xgboost <- result_data_xgboost_demeaned %>% 
  group_by(year) %>%
  do(tidy(
    coeftest( #robust standard errors 
      lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), 
      vcov = vcovHC(lm(ln_sachanlage_pemp_nn1 ~ industry_indicator, data = .), type = "HC0"))
  )   
  )


write.csv(analytical_result_demeaned_svm, "pooled_OLS_svm.csv")
write.csv(analytical_result_demeaned_ksvm, "pooled_OLS_ksvm.csv")
write.csv(analytical_result_demeaned_logit, "pooled_OLS_logit.csv")
write.csv(analytical_result_demeaned_rf, "pooled_OLS_rf.csv")
write.csv(analytical_result_demeaned_rf_non_para, "pooled_OLS_rf_non_para.csv")
write.csv(analytical_result_demeaned_xgboost, "pooled_OLS_xgboost.csv")
write.csv(analytical_result_demeaned_xgboost_non_para, "pooled_OLS_xgboost_non_para.csv")
"=================================================================================="
#writing temporary rds file for time efficiency
save.image(file = "analysis_split.RData")
load("analysis_split.RData")
"=================================================================================="