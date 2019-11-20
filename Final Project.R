## Bayesian Final Project ##
## Nick Weaver ##
## April 16, 2019 ##

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

### Data Set-up ###
setwd("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project")

#Function to read excel workbook files
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
#Find the data:
India <- read_excel_allsheets(filename = "India All Subjects.xlsx")
Ind <- India$`All data`

Ind$methylarginine <- as.numeric(Ind$methylarginine)

Pakistan <- read_excel_allsheets(filename = "Pakistan.xlsx")

IndWomen <- Pakistan$`Subjects (2)`[Pakistan$`Subjects (2)`$'SiteLocation' == "India",]

Cluster <- c()
for (i in 1:length(Ind$`Subject ID`)){
  dummy <- Ind$`Subject ID`[i]
  for (j in 1:length(IndWomen$SubjectID)){
    if(dummy == IndWomen$SubjectID[j]){
      Cluster[i] <- IndWomen$Cluster[j]
    }
  }
}

Ind <- cbind(Ind, Cluster)

PakWomen <- Pakistan$'Subjects (2)'[Pakistan$`Subjects (2)`$'SiteLocation' == "Pakistan",]

Arm <- c()
for (i in 1:length(Pakistan$Sheet1$`Subject ID`)){
  dummy <- Pakistan$Sheet1$`Subject ID`[i]
  for (j in 1: length(PakWomen$SubjectID)){
    if (dummy == PakWomen$SubjectID[j]){
      Arm[i] <- PakWomen$Arm[j]
    }
  }
}

Cluster <- c()
for (i in 1:length(Pakistan$Sheet1$`Subject ID`)){
  dummy <- Pakistan$Sheet1$`Subject ID`[i]
  for (j in 1: length(PakWomen$SubjectID)){
    if (dummy == PakWomen$SubjectID[j]){
      Cluster[i] <- PakWomen$Cluster[j]
    }
  }
}

Pak <- cbind(Pakistan$Sheet1, Arm, Cluster)

#Make all metabolites numeric:
Pak$Methylarginine <- as.numeric(Pak$Methylarginine)
Pak$Glutamine <- as.numeric(Pak$Glutamine)

#Load the 2 datasets from excel files:
Guat <- read.csv("Guatemala.csv", header = TRUE, na.strings = c("NA", "LOQ"))
DRC <- read.csv("DRC.csv", header = TRUE, na.strings = c("NA", "LOQ"))

#### Alter these data sets with the following:
### - Only Batch A 
### - Only Time point at 34 Weeks
### - Column indicating country

#First select only observations from Batch A
Ind <- Ind[Ind$Batch == "A",]
Pak <- Pak[Pak$Batch == "A",]
Guat <- Guat[Guat$Batch == "A",]
DRC <- DRC[DRC$Batch == "A",]

#Now select only observations from the 34 Weeks TimePoint:
Ind <- Ind[Ind$Time == "34Weeks",]
Pak <- Pak[Pak$`Time Point` == "34Weeks",]
Guat <- Guat[Guat$Time == "34Weeks",]
DRC <- DRC[DRC$Time == "34Weeks",]

#Now select only arms 1 and 2
Ind <- Ind[Ind$Arm != 3,]
Pak <- Pak[Pak$Arm != 3,]
Guat <- Guat[Guat$Arm != 3,]
DRC <- DRC[DRC$Arm != 3,]

#Finally, the region identifier:
d <- rep("DRC", times = length(DRC$Subject.ID))
g <- rep("Guatemala", time = length(Guat$Subject.ID))
indy <- rep("India", times = length(Ind$`Subject ID`))
p <- rep("Pakistan", time = length(Pak$`Subject ID`))

#### Now we will merge everything together!

#First, select the desired columns, in correct order, from each region:
aa <- data.frame(DRC$Subject.ID,  d, DRC$Cluster, DRC$Batch, DRC$Arm, DRC$Time, DRC$Mat.Ht, DRC$Mat.Age, DRC$BMI.Enr,
                 DRC$Parity, DRC$Inf.Length, DRC$Inf.Gender, DRC$ADMA, DRC$Alanine, DRC$Arginine, DRC$Betaine,
                 DRC$Choline, DRC$Citrulline, DRC$Creatine, DRC$Creatinine, DRC$Glutamine, DRC$Histidine,
                 DRC$Homoarginine, DRC$Lysine, DRC$Methionine, DRC$Methylarginine, DRC$Ornithine, DRC$Phenylalanine,
                 DRC$Proline, DRC$SDMA, DRC$Serine, DRC$Taurine, DRC$Threonine, DRC$Total.Cys,
                 DRC$Tryptophan, DRC$Tyrosine, DRC$Valine, DRC$Isoleucine)
bb <- data.frame(Guat$Subject.ID,  g, Guat$Cluster, Guat$Batch, Guat$Arm, Guat$Time, Guat$Mat.Ht, Guat$Mat.Age, Guat$BMI,
                 Guat$Parity, Guat$Inf.Length, Guat$Inf.Gender, Guat$ADMA, Guat$Alanine, Guat$Arginine, Guat$Betaine,
                 Guat$Choline, Guat$Citrulline, Guat$Creatine, Guat$Creatinine, Guat$Glutamine, Guat$Histidine,
                 Guat$Homoarginine, Guat$Lysine, Guat$Methionine, Guat$Methylarginine, Guat$Ornithine, Guat$Phenylalanine,
                 Guat$Proline, Guat$SDMA, Guat$Serine, Guat$Taurine, Guat$Threonine, Guat$Total.Cys,
                 Guat$Tryptophan, Guat$Tyrosine, Guat$Valine, Guat$Isoleucine)
cc <- data.frame(Ind$`Subject ID`, indy, Ind$Cluster  , Ind$Batch, Ind$Arm, Ind$Time, Ind$`Mat Ht`, Ind$`Mat Age`, Ind$BMI,
                 Ind$Parity, Ind$`Inf Length`, Ind$`Inf Gender`, Ind$ADMA, Ind$Alanine, Ind$Arginine, Ind$Betaine,
                 Ind$Choline, Ind$Citrulline, Ind$Creatine, Ind$Creatinine, Ind$Glutamine, Ind$Histidine,
                 Ind$Homoarginine, Ind$Lysine, Ind$Methionine, Ind$methylarginine, Ind$Ornithine, Ind$Phenylalanine,
                 Ind$Proline, Ind$SDMA, Ind$Serine, Ind$Taurine, Ind$Threonine, Ind$`Total Cys`,
                 Ind$Tryptophan, Ind$Tyrosine, Ind$Valine, Ind$Isoleucine)
dd <- data.frame(Pak$`Subject ID`, p, Pak$Cluster , Pak$Batch, Pak$Arm, Pak$`Time Point`, Pak$`Mat Ht`, Pak$`Mat Age`, Pak$BMI,
                 Pak$Parity, Pak$`Inf Length`, Pak$`Inf Gender`, Pak$ADMA, Pak$Alanine, Pak$Arginine, Pak$Betaine,
                 Pak$Choline, Pak$Citrulline, Pak$Creatine, Pak$Creatinine, Pak$Glutamine, Pak$Histidine,
                 Pak$Homoarginine, Pak$Lysine, Pak$Methionine, Pak$Methylarginine, Pak$Ornithine, Pak$Phenylalanine,
                 Pak$Proline, Pak$SDMA, Pak$Serine, Pak$Taurine, Pak$Threonine, Pak$Cyseine,
                 Pak$Tryptophan, Pak$Tyrosine, Pak$Valine, Pak$Isoleucine)

#Now, give names to the columns:
nams <- colnames(Guat[,12:38])
nam <- c("Subject", "Site", "Cluster", "Batch", "Arm", "Time", "Maternal Height", "Maternal Age", "Maternal BMI",
         "Parity", "Infant Length", "Infant Gender", nams)
colnames(aa) <- nam
colnames(bb) <- nam
colnames(cc) <- nam
colnames(dd) <- nam

#Finally, combine into one large data.frame by using rbind:
dframe <- rbind(aa, bb, cc, dd)

#Replace any strings within metabolite columns with NA values
for(i in 13:38){
  dframe[,i] <- as.numeric(dframe[,i])
}

check <- c()
for(i in 12:37){
  check <- cbind(check, is.numeric(dframe[,i]))
}

dframe[dframe == "NA"] <- NA
dframe[dframe == "LOQ"] <- NA

# I may want to remove Oultiers, but I am not sure what the best approach for this would be. Thus, I have not done it yet!

#### Data Prep Done ####


#### Analysis Time ####

# A scatterplot of the data

for (i in 13:38) {
  png(paste(nam[i],'.scatterplot.png', sep=''), width = 900, height = 750, res = 150)
  
  plot(dframe[,i] ~ dframe$`Maternal BMI`, main = nam[i],
      xlab = "BMI",
      ylab = "Natural Log of Metabolite Concentration",
      col = ifelse(dframe$Arm == 1, 1, 4),
      pch = ifelse(dframe$Arm == 1, 1, 16))
  legend("topright", legend = c("Arm 1", "Arm 2"),
        col = c(1, 4), cex = 1, pch = c(1,16))
  
  dev.off()
}

PlotMetabolite <- function(dataset,metabolite){
  dataset$Arm <- factor(dataset$Arm,
                         levels = c('1','2'),ordered = TRUE)
  ggplot(dataset,aes_string(x='Arm',y=metabolite)) + geom_boxplot(aes(col = Site)) + 
    geom_point(aes(col=Site),position = position_jitterdodge())+ 
    labs(title=metabolite,y='Metabolite Concentration') +  scale_color_brewer(name="Site",palette='Dark2') + theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line("gray"))
  ggsave(filename = paste(metabolite,'.boxplot.png',sep=''))
}

for (i in 1:length(nams)){
  PlotMetabolite(dataset = dframe,metabolite = nams[i]) #Loop through these as well
}

## Really not seeing much evidence of a difference between these two arms...

#### Create the model Design ####

#We need an indicator variable for Cluster (Note: this handles the issue for separate regions as well) 51 total Clusters!
# - s2 will be a dummy variable representing 201
# - s3 will be a dummy variable representing 202
# - s4 will be a dummy variable representing 203
# - s5 will be a dummy variable representing 204
# - s6 will be a dummy variable representing 205
# - s7 will be a dummy variable representing 206
# - s8 will be a dummy variable representing 207
# - s9 will be a dummy variable representing 208
# - s10 will be a dummy variable representing 209
# - s11 will be a dummy variable representing 210
# - s12 will be a dummy variable representing 211
# - s13 will be a dummy variable representing 212
# - s14 will be a dummy variable representing 611
# - s15 will be a dummy variable representing 612
# - s16 will be a dummy variable representing 613
# - s17 will be a dummy variable representing 614
# - s18 will be a dummy variable representing 615
# - s19 will be a dummy variable representing 616
# - s20 will be a dummy variable representing 617
# - s21 will be a dummy variable representing 618
# - s22 will be a dummy variable representing 619
# - s23 will be a dummy variable representing 620
# - s24 will be a dummy variable representing 624
# - s25 will be a dummy variable representing 625
# - s26 will be a dummy variable representing 626
# - s27 will be a dummy variable representing 627
# - s28 will be a dummy variable representing 640
# - s29 will be a dummy variable representing 641
# - s30 will be a dummy variable representing 642
# - s31 will be a dummy variable representing 811
# - s32 will be a dummy variable representing 813
# - s33 will be a dummy variable representing 814
# - s34 will be a dummy variable representing 815
# - s35 will be a dummy variable representing 817
# - s36 will be a dummy variable representing 819
# - s37 will be a dummy variable representing 820
# - s38 will be a dummy variable representing 824
# - s39 will be a dummy variable representing 826
# - s40 will be a dummy variable representing 911
# - s41 will be a dummy variable representing 912
# - s42 will be a dummy variable representing 913
# - s43 will be a dummy variable representing 915
# - s44 will be a dummy variable representing 916
# - s45 will be a dummy variable representing 917
# - s46 will be a dummy variable representing 918
# - s47 will be a dummy variable representing 919
# - s48 will be a dummy variable representing 921
# - s49 will be a dummy variable representing 922
# - s50 will be a dummy variable representing 926
# - s51 will be a dummy variable representing 931

# - If all are zero, then it represents 932!

s2 <- (dframe$Cluster == "201")*1
s3 <- (dframe$Cluster == "202")*1
s4 <- (dframe$Cluster == "203")*1
s5 <- (dframe$Cluster == "204")*1
s6 <- (dframe$Cluster == "205")*1
s7 <- (dframe$Cluster == "206")*1
s8 <- (dframe$Cluster == "207")*1
s9 <- (dframe$Cluster == "208")*1
s10 <- (dframe$Cluster == "209")*1
s11 <- (dframe$Cluster == "210")*1
s12 <- (dframe$Cluster == "211")*1
s13 <- (dframe$Cluster == "212")*1
s14 <- (dframe$Cluster == "611")*1
s15 <- (dframe$Cluster == "612")*1
s16 <- (dframe$Cluster == "613")*1
s17 <- (dframe$Cluster == "614")*1
s18 <- (dframe$Cluster == "615")*1
s19 <- (dframe$Cluster == "616")*1
s20 <- (dframe$Cluster == "617")*1
s21 <- (dframe$Cluster == "618")*1
s22 <- (dframe$Cluster == "619")*1
s23 <- (dframe$Cluster == "620")*1
s24 <- (dframe$Cluster == "624")*1
s25 <- (dframe$Cluster == "625")*1
s26 <- (dframe$Cluster == "626")*1
s27 <- (dframe$Cluster == "627")*1
s28 <- (dframe$Cluster == "640")*1
s29 <- (dframe$Cluster == "641")*1
s30 <- (dframe$Cluster == "642")*1
s31 <- (dframe$Cluster == "811")*1
s32 <- (dframe$Cluster == "813")*1
s33 <- (dframe$Cluster == "814")*1
s34 <- (dframe$Cluster == "815")*1
s35 <- (dframe$Cluster == "817")*1
s36 <- (dframe$Cluster == "819")*1
s37 <- (dframe$Cluster == "820")*1
s38 <- (dframe$Cluster == "824")*1
s39 <- (dframe$Cluster == "826")*1
s40 <- (dframe$Cluster == "911")*1
s41 <- (dframe$Cluster == "912")*1
s42 <- (dframe$Cluster == "913")*1
s43 <- (dframe$Cluster == "915")*1
s44 <- (dframe$Cluster == "916")*1
s45 <- (dframe$Cluster == "917")*1
s46 <- (dframe$Cluster == "918")*1
s47 <- (dframe$Cluster == "919")*1
s48 <- (dframe$Cluster == "921")*1
s49 <- (dframe$Cluster == "922")*1
s50 <- (dframe$Cluster == "926")*1
s51 <- (dframe$Cluster == "931")*1

#We need an indicator variable for Arm
# - d2 will be a dummy variable representing Arm 1 (0 will be Arm 2)

d2 <- (dframe$Arm == 1)*1

#Now we create the model in rstan

pl_mod4 = "
data {
int<lower=1> n;  // number of observations
vector[n] y;     // Response Variable (metabolite concentration)
vector[n] d2;    // Indicator for Arm 1
vector[n] s2;    // Incidator 
vector[n] s3;    // Indicator 
vector[n] s4;    // Indicator
vector[n] s5;    // Indicator
vector[n] s6;    // Indicator
vector[n] s7;    // Indicator
vector[n] s8;    // Indicator
vector[n] s9;    // Indicator
vector[n] s10;   // Indicator
vector[n] s11;   // Indicator
vector[n] s12;   // Indicator
vector[n] s13;   // Indicator
vector[n] s14;   // Indicator
vector[n] s15;   // Indicator
vector[n] s16;   // Indicator
vector[n] s17;   // Indicator
vector[n] s18;   // Indicator
vector[n] s19;   // Indicator
vector[n] s20;   // Indicator
vector[n] s21;   // Indicator
vector[n] s22;   // Indicator
vector[n] s23;   // Indicator
vector[n] s24;   // Indicator
vector[n] s25;   // Indicator
vector[n] s26;   // Indicator
vector[n] s27;   // Indicator
vector[n] s28;   // Indicator
vector[n] s29;   // Indicator
vector[n] s30;   // Indicator
vector[n] s31;   // Indicator
vector[n] s32;   // Indicator
vector[n] s33;   // Indicator
vector[n] s34;   // Indicator
vector[n] s35;   // Indicator
vector[n] s36;   // Indicator
vector[n] s37;   // Indicator
vector[n] s38;   // Indicator
vector[n] s39;   // Indicator
vector[n] s40;   // Indicator
vector[n] s41;   // Indicator
vector[n] s42;   // Indicator
vector[n] s43;   // Indicator
vector[n] s44;   // Indicator
vector[n] s45;   // Indicator
vector[n] s46;   // Indicator
vector[n] s47;   // Indicator
vector[n] s48;   // Indicator
vector[n] s49;   // Indicator
vector[n] s50;   // Indicator
vector[n] s51;   // Indicator
real<lower=0> v; // sample variance of y
}
parameters {
real<lower=0> sigmasq;
real beta0;
real beta1;
real beta2;
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real beta8;
real beta9;
real beta10;
real beta11;
real beta12;
real beta13;
real beta14;
real beta15;
real beta16;
real beta17;
real beta18;
real beta19;
real beta20;
real beta21;
real beta22;
real beta23;
real beta24;
real beta25;
real beta26;
real beta27;
real beta28;
real beta29;
real beta30;
real beta31;
real beta32;
real beta33;
real beta34;
real beta35;
real beta36;
real beta37;
real beta38;
real beta39;
real beta40;
real beta41;
real beta42;
real beta43;
real beta44;
real beta45;
real beta46;
real beta47;
real beta48;
real beta49;
real beta50;
real beta51;
}
transformed parameters {
vector[n] mu;           // mean of observations
for(i in 1:n) {
  mu[i] = beta0 + beta1*d2[i] + beta2*s2[i] + beta3*s3[i] + beta4*s4[i] + beta5*s5[i] + beta6*s6[i] + beta7*s7[i] + beta8*s8[i] + beta9*s9[i] + beta10*s10[i] + beta11*s11[i] + beta12*s12[i] + beta13*s13[i] + beta14*s14[i] + beta15*s15[i] + beta16*s16[i] + beta17*s17[i] + beta18*s18[i] + beta19*s19[i] + beta20*s20[i] + beta21*s21[i] + beta22*s22[i] + beta23*s23[i] + beta24*s24[i] + beta25*s25[i] + beta26*s26[i] + beta27*s27[i] + beta28*s28[i] + beta29*s29[i] + beta30*s30[i] + beta31*s31[i] + beta32*s32[i] + beta33*s33[i] + beta34*s34[i] + beta35*s35[i] + beta36*s36[i] + beta37*s37[i] + beta38*s38[i] + beta38*s38[i] + beta39*s39[i] + beta40*s40[i] + beta41*s41[i] + beta42*s42[i] + beta43*s43[i] + beta44*s44[i] + beta45*s45[i] + beta46*s46[i] + beta47*s47[i] + beta48*s48[i] + beta49*s49[i] + beta50*s50[i] + beta51*s51[i];
  }
}
model {
// prior distributions
sigmasq ~ inv_gamma(.01, .01);
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
beta2 ~ normal(0, 100);
beta3 ~ normal(0, 100);
beta4 ~ normal(0, 100);
beta5 ~ normal(0, 100);
beta6 ~ normal(0, 100);
beta7 ~ normal(0, 100);
beta8 ~ normal(0, 100);
beta9 ~ normal(0, 100);
beta10 ~ normal(0, 100);
beta11 ~ normal(0, 100);
beta12 ~ normal(0, 100);
beta13 ~ normal(0, 100);
beta14 ~ normal(0, 100);
beta15 ~ normal(0, 100);
beta16 ~ normal(0, 100);
beta17 ~ normal(0, 100);
beta18 ~ normal(0, 100);
beta19 ~ normal(0, 100);
beta20 ~ normal(0, 100);
beta21 ~ normal(0, 100);
beta22 ~ normal(0, 100);
beta23 ~ normal(0, 100);
beta24 ~ normal(0, 100);
beta25 ~ normal(0, 100);
beta26 ~ normal(0, 100);
beta27 ~ normal(0, 100);
beta28 ~ normal(0, 100);
beta29 ~ normal(0, 100);
beta30 ~ normal(0, 100);
beta31 ~ normal(0, 100);
beta32 ~ normal(0, 100);
beta33 ~ normal(0, 100);
beta34 ~ normal(0, 100);
beta35 ~ normal(0, 100);
beta36 ~ normal(0, 100);
beta37 ~ normal(0, 100);
beta38 ~ normal(0, 100);
beta39 ~ normal(0, 100);
beta40 ~ normal(0, 100);
beta41 ~ normal(0, 100);
beta42 ~ normal(0, 100);
beta43 ~ normal(0, 100);
beta44 ~ normal(0, 100);
beta45 ~ normal(0, 100);
beta46 ~ normal(0, 100);
beta47 ~ normal(0, 100);
beta48 ~ normal(0, 100);
beta49 ~ normal(0, 100);
beta50 ~ normal(0, 100);
beta51 ~ normal(0, 100);
// data distribution
for(i in 1:n) y[i] ~ normal(mu[i], sqrt(sigmasq));
}
generated quantities {
real Rbsq;              // goodness-of-fit
real log_lik[n];      // log likelihood of each observation
Rbsq = 1 - sigmasq/v;
for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(sigmasq));
}
"
# Specify the data in R, using a list
# format compatible with STAN:

#It won't run if NA's are present in the data. So remove any row that contains an NA (we will want to repeat this for
# each metabolite that we run!):
dframe <- dframe[complete.cases(dframe),]

dat4 <- list(n = length(dframe$Glutamine), y = dframe$Glutamine, d2 = d2, s2 = s2, s3 = s3, s4 = s4,
             s5 = s5, s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10, s11 = s11, s12 = s12, s13 = s13,
             s14 = s14, s15 = s15, s16 = s16, s17 = s17, s18 = s18, s19 = s19, s20 = s20, s21 = s21,
             s22 = s22, s23 = s23, s24 = s24, s25 = s25, s26 = s26, s27 = s27, s28 = s28, s29 = s29,
             s30 = s30, s31 = s31, s32 = s32, s33 = s33, s34 = s34, s35 = s35, s36 = s36, s37 = s37,
             s38 = s38, s39 = s39, s40 = s40, s41 = s41, s42 = s42, s43 = s43, s44 = s44, s45 = s45,
             s46 = s46, s47 = s47, s48 = s48, s49 = s49, s50 = s50, s51 = s51, v = var(dframe$Glutamine))

# draw samples from the model
fit_4 <- stan(model_code = pl_mod4, data = dat4, iter = 100000,
              control = list(adapt_delta = 0.99),
              seed = 43)


#### Parts 2 & 3 ####
## Analyze Model 1
# check convergence with gelman-rubin statistics
summary(fit_4)$summary[,"Rhat"]
#Looks Fine!

# check convergence with trace plots
stan_trace(fit_4, c("beta0", "beta1","sigmasq"))
#All appear to converge!


# summary of fitted values
summary(fit_4)$summary[c("beta0", "beta1","sigmasq"),]

# posterior means
summary(fit_4)$summary[c("beta0", "beta1","sigmasq"),"mean"]

# 95% central posterior intervals
summary(fit_4)$summary[c("beta0", "beta1","sigmasq"), c("2.5%", "97.5%")]

# plot of densities
stan_dens(fit_4, par = c("beta0", "beta1","sigmasq"),
          separate_chains = TRUE)

# distribution of Rb^2
stan_dens(fit_4, "Rbsq") + xlim(c(0.5, 1))

### Looks like our parameter of interest (beta1) does not really have an impact upon Glutamine in week 34 ###

### Modle Check ###

# store nyrep samples of yrep
# from from posterior predictive distribution
nyrep = 10000
yrep = matrix(0, nrow = nyrep, ncol = length(dframe$Glutamine))

# rename for convenience
y = dframe$Glutamine
# sample  observations from posterior predictive distribution
iters <- extract(fit_4)
mu <- iters$mu
for (i in seq_len(nyrep)) {
  for (j in 1:196) {
    yrep[i, j] = rnorm(1, mean = mu[i,j], sd = sqrt(iters$sigmasq[i]))
  }  
}

# compare density of y and yrep
library(bayesplot)
ppc_dens_overlay(y, yrep[1:20 , ])

# minimum of replicated sample
mins = apply(yrep, 1, min)
# estimated p-value
(sum(mins <= min(y)) + 1)/(length(mins) + 1)

# histogram comparing T(y) and T(yrep)
ppc_stat(y, yrep, stat = "min")

# look at asymmetry of distribution
# by comparing order statistics to
# samples of posterior mean
d_sim = d_obs = numeric(nrow(yrep))
sort_y = sort(y)

for (i in 1:nrow(yrep)) {
  thetai = mu[i]
  sort_yrep = sort(yrep[i, ])
  d_sim[i] = abs(sort_yrep[74] - mu[i]) - 
    abs(sort_yrep[7] - mu[i])
  d_obs[i] = abs(sort_y[74] - mu[i]) - 
    abs(sort_y[7] - mu[i])
}
# estimated posterior predictive p-value
(sum(d_sim >= d_obs) + 1)/(length(d_sim) + 1)

# compare observed and simulated discrepancy measures
plot(d_sim ~ d_obs, xlab = "observed discrepancy", ylab = "simulated discrepancy")
abline(0, 1)

#### Model is no good. We need to make sure that we do not predict negative values 
#### for a concentration. Several ways to fix this: Model response as positive continuous?
#### normalize the values <- I think this way makes sense because we already expect linear
#### relationship!

#Log Transform of response to allow response to be on the real line, not just positive values
dframe[,nams] <- log(dframe[,nams])
### Note, this transformation does not seem to change the linear relationship, so I think we are good!

#Rerun the Model with the updated data:
dat5 <- list(n = length(dframe$Glutamine), y = dframe$Glutamine, d2 = d2, s2 = s2, s3 = s3, s4 = s4,
             s5 = s5, s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10, s11 = s11, s12 = s12, s13 = s13,
             s14 = s14, s15 = s15, s16 = s16, s17 = s17, s18 = s18, s19 = s19, s20 = s20, s21 = s21,
             s22 = s22, s23 = s23, s24 = s24, s25 = s25, s26 = s26, s27 = s27, s28 = s28, s29 = s29,
             s30 = s30, s31 = s31, s32 = s32, s33 = s33, s34 = s34, s35 = s35, s36 = s36, s37 = s37,
             s38 = s38, s39 = s39, s40 = s40, s41 = s41, s42 = s42, s43 = s43, s44 = s44, s45 = s45,
             s46 = s46, s47 = s47, s48 = s48, s49 = s49, s50 = s50, s51 = s51, v = var(dframe$Glutamine))

# draw samples from the model
fit_5 <- stan(model_code = pl_mod4, data = dat5, iter = 100000,
              control = list(adapt_delta = 0.99),
              seed = 43)

#### Parts 2 & 3 ####
## Analyze Model 1
# check convergence with gelman-rubin statistics
summary(fit_5)$summary[,"Rhat"]
#Looks Fine!

# check convergence with trace plots
stan_trace(fit_5, c("beta0", "beta1","sigmasq"))
#All appear to converge!


# summary of fitted values
summary(fit_5)$summary[c("beta0", "beta1","sigmasq"),]

# posterior means
summary(fit_5)$summary[c("beta0", "beta1","sigmasq"),"mean"]

# 95% central posterior intervals
summary(fit_5)$summary[c("beta0", "beta1","sigmasq"), c("2.5%", "97.5%")]

# plot of densities
stan_dens(fit_5, par = c("beta0", "beta1","sigmasq"),
          separate_chains = TRUE)

# distribution of Rb^2
stan_dens(fit_5, "Rbsq") + xlim(c(0.5, 1))

### Looks like our parameter of interest (beta1) does not really have an impact upon Glutamine in week 34 ###

### Modle Check ###

# store nyrep samples of yrep
# from from posterior predictive distribution
nyrep = 10000
logyrep = matrix(0, nrow = nyrep, ncol = length(dframe$Glutamine))

# rename for convenience
logy = dframe$Glutamine
# sample  observations from posterior predictive distribution
logiters <- extract(fit_5)
logmu <- logiters$mu
for (i in seq_len(nyrep)) {
  for (j in 1:196) {
    logyrep[i, j] = rnorm(1, mean = logmu[i,j], sd = sqrt(logiters$sigmasq[i]))
  }  
}

# compare density of y and yrep
library(bayesplot)
ppc_dens_overlay(logy, logyrep[1:20, ])

# minimum of replicated sample
logmins = apply(logyrep, 1, min)
# estimated p-value
(sum(logmins <= min(logy)) + 1)/(length(logmins) + 1)

# histogram comparing T(y) and T(yrep)
ppc_stat(logy, logyrep, stat = "min")

# look at asymmetry of distribution
# by comparing order statistics to
# samples of posterior mean
logd_sim = d_logobs = numeric(nrow(logyrep))
sort_logy = sort(logy)

for (i in 1:nrow(logyrep)) {
  thetai = logmu[i]
  sort_logyrep = sort(logyrep[i, ])
  logd_sim[i] = abs(sort_logyrep[74] - logmu[i]) - 
    abs(sort_logyrep[7] - logmu[i])
  d_logobs[i] = abs(sort_logy[74] - logmu[i]) - 
    abs(sort_logy[7] - logmu[i])
}
# estimated posterior predictive p-value
(sum(logd_sim >= d_logobs) + 1)/(length(logd_sim) + 1)

# compare observed and simulated discrepancy measures
plot(logd_sim ~ d_logobs, xlab = "observed discrepancy", ylab = "simulated discrepancy")
abline(0, 1)


#### New Model Fit ####
## what if we include site as well? I'm sure the clusters within a site are related in a
## way that is not being identified in our model!

## Need to add the following parameters:

# t1 -- dummy variable for DRC
# t2 -- dummy variable for pakistan
# t3 -- dummy variable for India

# If all are zero, then we are in Guatemala!

t1 <- (dframe$Site == "DRC")*1
t2 <- (dframe$Site == "Pakistan")*1
t3 <- (dframe$Site == "India")*1

#Update the previous model fit:

pl_mod5 = "
data {
int<lower=1> n;  // number of observations
vector[n] y;     // Response Variable (metabolite concentration)
vector[n] d2;    // Indicator for Arm 1
vector[n] t1;    // Indicator for Site
vector[n] t2;    // Indicator
vector[n] t3;    // Indicator
vector[n] s2;    // Incidator for Cluster 
vector[n] s3;    // Indicator 
vector[n] s4;    // Indicator
vector[n] s5;    // Indicator
vector[n] s6;    // Indicator
vector[n] s7;    // Indicator
vector[n] s8;    // Indicator
vector[n] s9;    // Indicator
vector[n] s10;   // Indicator
vector[n] s11;   // Indicator
vector[n] s12;   // Indicator
vector[n] s13;   // Indicator
vector[n] s14;   // Indicator
vector[n] s15;   // Indicator
vector[n] s16;   // Indicator
vector[n] s17;   // Indicator
vector[n] s18;   // Indicator
vector[n] s19;   // Indicator
vector[n] s20;   // Indicator
vector[n] s21;   // Indicator
vector[n] s22;   // Indicator
vector[n] s23;   // Indicator
vector[n] s24;   // Indicator
vector[n] s25;   // Indicator
vector[n] s26;   // Indicator
vector[n] s27;   // Indicator
vector[n] s28;   // Indicator
vector[n] s29;   // Indicator
vector[n] s30;   // Indicator
vector[n] s31;   // Indicator
vector[n] s32;   // Indicator
vector[n] s33;   // Indicator
vector[n] s34;   // Indicator
vector[n] s35;   // Indicator
vector[n] s36;   // Indicator
vector[n] s37;   // Indicator
vector[n] s38;   // Indicator
vector[n] s39;   // Indicator
vector[n] s40;   // Indicator
vector[n] s41;   // Indicator
vector[n] s42;   // Indicator
vector[n] s43;   // Indicator
vector[n] s44;   // Indicator
vector[n] s45;   // Indicator
vector[n] s46;   // Indicator
vector[n] s47;   // Indicator
vector[n] s48;   // Indicator
vector[n] s49;   // Indicator
vector[n] s50;   // Indicator
vector[n] s51;   // Indicator
real<lower=0> v; // sample variance of y
}
parameters {
real<lower=0> sigmasq;
real alpha1;
real alpha2;
real alpha3;
real beta0;
real beta1;
real beta2;
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real beta8;
real beta9;
real beta10;
real beta11;
real beta12;
real beta13;
real beta14;
real beta15;
real beta16;
real beta17;
real beta18;
real beta19;
real beta20;
real beta21;
real beta22;
real beta23;
real beta24;
real beta25;
real beta26;
real beta27;
real beta28;
real beta29;
real beta30;
real beta31;
real beta32;
real beta33;
real beta34;
real beta35;
real beta36;
real beta37;
real beta38;
real beta39;
real beta40;
real beta41;
real beta42;
real beta43;
real beta44;
real beta45;
real beta46;
real beta47;
real beta48;
real beta49;
real beta50;
real beta51;
}
transformed parameters {
vector[n] mu;           // mean of observations
for(i in 1:n) {
mu[i] = beta0 + beta1*d2[i] + alpha1*t1[i] + alpha2*t2[i] + alpha3*t3[i] + beta2*s2[i] + beta3*s3[i] + beta4*s4[i] + beta5*s5[i] + beta6*s6[i] + beta7*s7[i] + beta8*s8[i] + beta9*s9[i] + beta10*s10[i] + beta11*s11[i] + beta12*s12[i] + beta13*s13[i] + beta14*s14[i] + beta15*s15[i] + beta16*s16[i] + beta17*s17[i] + beta18*s18[i] + beta19*s19[i] + beta20*s20[i] + beta21*s21[i] + beta22*s22[i] + beta23*s23[i] + beta24*s24[i] + beta25*s25[i] + beta26*s26[i] + beta27*s27[i] + beta28*s28[i] + beta29*s29[i] + beta30*s30[i] + beta31*s31[i] + beta32*s32[i] + beta33*s33[i] + beta34*s34[i] + beta35*s35[i] + beta36*s36[i] + beta37*s37[i] + beta38*s38[i] + beta38*s38[i] + beta39*s39[i] + beta40*s40[i] + beta41*s41[i] + beta42*s42[i] + beta43*s43[i] + beta44*s44[i] + beta45*s45[i] + beta46*s46[i] + beta47*s47[i] + beta48*s48[i] + beta49*s49[i] + beta50*s50[i] + beta51*s51[i];
}
}
model {
// prior distributions
sigmasq ~ inv_gamma(.01, .01);
alpha1 ~ normal(0, 100);
alpha2 ~ normal(0, 100);
alpha3 ~ normal(0, 100);
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
beta2 ~ normal(0, 100);
beta3 ~ normal(0, 100);
beta4 ~ normal(0, 100);
beta5 ~ normal(0, 100);
beta6 ~ normal(0, 100);
beta7 ~ normal(0, 100);
beta8 ~ normal(0, 100);
beta9 ~ normal(0, 100);
beta10 ~ normal(0, 100);
beta11 ~ normal(0, 100);
beta12 ~ normal(0, 100);
beta13 ~ normal(0, 100);
beta14 ~ normal(0, 100);
beta15 ~ normal(0, 100);
beta16 ~ normal(0, 100);
beta17 ~ normal(0, 100);
beta18 ~ normal(0, 100);
beta19 ~ normal(0, 100);
beta20 ~ normal(0, 100);
beta21 ~ normal(0, 100);
beta22 ~ normal(0, 100);
beta23 ~ normal(0, 100);
beta24 ~ normal(0, 100);
beta25 ~ normal(0, 100);
beta26 ~ normal(0, 100);
beta27 ~ normal(0, 100);
beta28 ~ normal(0, 100);
beta29 ~ normal(0, 100);
beta30 ~ normal(0, 100);
beta31 ~ normal(0, 100);
beta32 ~ normal(0, 100);
beta33 ~ normal(0, 100);
beta34 ~ normal(0, 100);
beta35 ~ normal(0, 100);
beta36 ~ normal(0, 100);
beta37 ~ normal(0, 100);
beta38 ~ normal(0, 100);
beta39 ~ normal(0, 100);
beta40 ~ normal(0, 100);
beta41 ~ normal(0, 100);
beta42 ~ normal(0, 100);
beta43 ~ normal(0, 100);
beta44 ~ normal(0, 100);
beta45 ~ normal(0, 100);
beta46 ~ normal(0, 100);
beta47 ~ normal(0, 100);
beta48 ~ normal(0, 100);
beta49 ~ normal(0, 100);
beta50 ~ normal(0, 100);
beta51 ~ normal(0, 100);
// data distribution
for(i in 1:n) y[i] ~ normal(mu[i], sqrt(sigmasq));
}
generated quantities {
real Rbsq;              // goodness-of-fit
real log_lik[n];      // log likelihood of each observation
Rbsq = 1 - sigmasq/v;
for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(sigmasq));
}
"
# Specify the data in R, using a list
# format compatible with STAN:

#It won't run if NA's are present in the data. So remove any row that contains an NA (we will want to repeat this for
# each metabolite that we run!):
dframe <- dframe[complete.cases(dframe),]

dat6 <- list(n = length(dframe$Glutamine), y = dframe$Glutamine, d2 = d2, t1 = t1, t2 = t2, t3 = t3, s2 = s2, s3 = s3, s4 = s4,
             s5 = s5, s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10, s11 = s11, s12 = s12, s13 = s13,
             s14 = s14, s15 = s15, s16 = s16, s17 = s17, s18 = s18, s19 = s19, s20 = s20, s21 = s21,
             s22 = s22, s23 = s23, s24 = s24, s25 = s25, s26 = s26, s27 = s27, s28 = s28, s29 = s29,
             s30 = s30, s31 = s31, s32 = s32, s33 = s33, s34 = s34, s35 = s35, s36 = s36, s37 = s37,
             s38 = s38, s39 = s39, s40 = s40, s41 = s41, s42 = s42, s43 = s43, s44 = s44, s45 = s45,
             s46 = s46, s47 = s47, s48 = s48, s49 = s49, s50 = s50, s51 = s51, v = var(dframe$Glutamine))

# draw samples from the model
fit_6 <- stan(model_code = pl_mod5, data = dat6, iter = 100000,
              control = list(adapt_delta = 0.99),
              seed = 43)


#### Parts 2 & 3 ####
## Analyze Model 1
# check convergence with gelman-rubin statistics
summary(fit_6)$summary[,"Rhat"]
#Looks Fine! Although not as close to 1 as I may like...

# check convergence with trace plots
stan_trace(fit_6, c("beta0", "beta1","sigmasq"))
#Not Good!!!! Look at that beta0 plot!!!


# summary of fitted values
summary(fit_6)$summary[c("beta0", "beta1","sigmasq"),]

# Means and 95% central posterior intervals
summary(fit_6)$summary[c("beta0", "beta1","sigmasq"), c("mean", "2.5%", "97.5%")]

# plot of densities
stan_dens(fit_6, par = c("beta0", "beta1","sigmasq"),
          separate_chains = TRUE)
#Looks like our chains did not converge to the same distribution!!! Especially for beta0!!

# distribution of Rb^2
stan_dens(fit_6, "Rbsq") + xlim(c(0.5, 1))

### Looks like our parameter of interest (beta1) does not really have an impact upon Glutamine in week 34 ###

### Modle 2 Check ###

# store nyrep samples of yrep
# from from posterior predictive distribution
nyrep = 10000
logyrep_mod2 = matrix(0, nrow = nyrep, ncol = length(dframe$Glutamine))

# rename for convenience
logy_mod2 = dframe$Glutamine
# sample  observations from posterior predictive distribution
logiters_mod2 <- extract(fit_6)
logmu_mod2 <- logiters_mod2$mu
for (i in seq_len(nyrep)) {
  for (j in 1:196) {
    logyrep_mod2[i, j] = rnorm(1, mean = logmu_mod2[i,j], sd = sqrt(logiters_mod2$sigmasq[i]))
  }  
}

# compare density of y and yrep
library(bayesplot)
ppc_dens_overlay(logy_mod2, logyrep_mod2[1:20, ])

# minimum of replicated sample
logmins_mod2 = apply(logyrep_mod2, 1, min)
# estimated p-value
(sum(logmins_mod2 <= min(logy_mod2)) + 1)/(length(logmins_mod2) + 1)

# histogram comparing T(y) and T(yrep)
ppc_stat(logy_mod2, logyrep_mod2, stat = "min")

# look at asymmetry of distribution
# by comparing order statistics to
# samples of posterior mean
logd_sim_mod2 = d_logobs_mod2 = numeric(nrow(logyrep_mod2))
sort_logy_mod2 = sort(logy_mod2)

for (i in 1:nrow(logyrep_mod2)) {
  thetai = logmu_mod2[i]
  sort_logyrep_mod2 = sort(logyrep_mod2[i, ])
  logd_sim_mod2[i] = abs(sort_logyrep_mod2[74] - logmu_mod2[i]) - 
    abs(sort_logyrep_mod2[7] - logmu_mod2[i])
  d_logobs_mod2[i] = abs(sort_logy_mod2[74] - logmu_mod2[i]) - 
    abs(sort_logy_mod2[7] - logmu_mod2[i])
}
# estimated posterior predictive p-value
(sum(logd_sim_mod2 >= d_logobs_mod2) + 1)/(length(logd_sim_mod2) + 1)

# compare observed and simulated discrepancy measures
plot(logd_sim_mod2 ~ d_logobs_mod2, xlab = "observed discrepancy", ylab = "simulated discrepancy")
abline(0, 1)

# compute looic for the 2 models
library(loo)

# extract log likelihoods
ll_pl = extract_log_lik(fit_5, merge_chains = FALSE)
ll_sl = extract_log_lik(fit_6, merge_chains = FALSE)


# compute relative efficiency of log likelihoods
# r_eff_pl = exp(relative_eff(ll_pl))
r_eff_pl = exp(relative_eff(ll_pl))
r_eff_sl = exp(relative_eff(ll_sl))

# compute looic for each model
(looic_pl <- loo(ll_pl,
                 r_eff = r_eff_pl))
(looic_sl <- loo(ll_sl,
                 r_eff = r_eff_sl))

compare(looic_pl, looic_sl)

## The first model is better!!! But I think the setup of the second one is more accurate...

########## ONLY GUATEMALA ############

## There are way too many clusters to account for when combining all of the regions. I think
## it may be better to think about each site as being it's own population, so let us just look
## at the Guatemalan women

## Log transform
Guat[,nams] <- log(Guat[,nams])

## Dummy variable for Arm:
d2 <- (Guat$Arm == 1)*1

#Now we create the model in rstan
s2 <- (Guat$Cluster == "611")*1
s3 <- (Guat$Cluster == "612")*1
s4 <- (Guat$Cluster == "613")*1
s5 <- (Guat$Cluster == "614")*1
s6 <- (Guat$Cluster == "615")*1
s7 <- (Guat$Cluster == "616")*1
s8 <- (Guat$Cluster == "617")*1
s9 <- (Guat$Cluster == "618")*1
s10 <- (Guat$Cluster == "619")*1
s11 <- (Guat$Cluster == "620")*1
s12 <- (Guat$Cluster == "624")*1
s13 <- (Guat$Cluster == "625")*1
s14 <- (Guat$Cluster == "626")*1
s15 <- (Guat$Cluster == "627")*1
s16 <- (Guat$Cluster == "640")*1
s17 <- (Guat$Cluster == "641")*1

#cluster 642 is the cluster when all s values are 0

pl_modG1 = "
data {
int<lower=1> n;  // number of observations
vector[n] y;     // Response Variable (metabolite concentration)
vector[n] d2;    // Indicator for Arm 1
vector[n] s2;    // Incidator for Cluster
vector[n] s3;    // Indicator 
vector[n] s4;    // Indicator
vector[n] s5;    // Indicator
vector[n] s6;    // Indicator
vector[n] s7;    // Indicator
vector[n] s8;    // Indicator
vector[n] s9;    // Indicator
vector[n] s10;   // Indicator
vector[n] s11;   // Indicator
vector[n] s12;   // Indicator
vector[n] s13;   // Indicator
vector[n] s14;   // Indicator
vector[n] s15;   // Indicator
vector[n] s16;   // Indicator
vector[n] s17;   // Indicator
real<lower=0> v; // sample variance of y
}
parameters {
real<lower=0> sigmasq;
real beta0;
real beta1;
real beta2;
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real beta8;
real beta9;
real beta10;
real beta11;
real beta12;
real beta13;
real beta14;
real beta15;
real beta16;
real beta17;
}
transformed parameters {
vector[n] mu;           // mean of observations
for(i in 1:n) {
mu[i] = beta0 + beta1*d2[i] + beta2*s2[i] + beta3*s3[i] + beta4*s4[i] + beta5*s5[i] + beta6*s6[i] + beta7*s7[i] + beta8*s8[i] + beta9*s9[i] + beta10*s10[i] + beta11*s11[i] + beta12*s12[i] + beta13*s13[i] + beta14*s14[i] + beta15*s15[i] + beta16*s16[i] + beta17*s17[i];
}
}
model {
// prior distributions
sigmasq ~ inv_gamma(.01, .01);
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
beta2 ~ normal(0, 100);
beta3 ~ normal(0, 100);
beta4 ~ normal(0, 100);
beta5 ~ normal(0, 100);
beta6 ~ normal(0, 100);
beta7 ~ normal(0, 100);
beta8 ~ normal(0, 100);
beta9 ~ normal(0, 100);
beta10 ~ normal(0, 100);
beta11 ~ normal(0, 100);
beta12 ~ normal(0, 100);
beta13 ~ normal(0, 100);
beta14 ~ normal(0, 100);
beta15 ~ normal(0, 100);
beta16 ~ normal(0, 100);
beta17 ~ normal(0, 100);
// data distribution
for(i in 1:n) y[i] ~ normal(mu[i], sqrt(sigmasq));
}
generated quantities {
real Rbsq;              // goodness-of-fit
real log_lik[n];      // log likelihood of each observation
Rbsq = 1 - sigmasq/v;
for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(sigmasq));
}
"
# Specify the data in R, using a list
# format compatible with STAN:

#It won't run if NA's are present in the data. So remove any row that contains an NA (we will want to repeat this for
# each metabolite that we run!):
Guat <- Guat[complete.cases(Guat),]

for (i in 1: length(nams)){
  #Set the directory to save all of the files/images to!
  setwd(paste("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project\\Guatemala Only\\", nams[i], sep =''))
  
  #Run the model for the specific metabolite:
  n = length(Guat[,nams[i]])
  y = Guat[, nams[i]]
  v = var(y)
  
  datG1 <- list(n = n, y = y, d2 = d2, s2 = s2, s3 = s3, s4 = s4,
             s5 = s5, s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10, s11 = s11, s12 = s12, s13 = s13,
             s14 = s14, s15 = s15, s16 = s16, s17 = s17, v = v)

  # draw samples from the model
  fit_G1 <- stan(model_code = pl_modG1, data = datG1, iter = 100000,
              control = list(adapt_delta = 0.99),
              seed = 43)
  
  ## Analyze Model 
  # check convergence with gelman-rubin statistics
  write.csv(as.table(summary(fit_G1)$summary[,"Rhat"]), file = paste("Gelman-Rubin Convergence for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  # check convergence with trace plots
  png(filename = paste(nams[i], " Trace Plots Model 1.png", sep = ''), width = 1500, height = 433, res = 150)
  print(stan_trace(fit_G1, c("beta0", "beta1","sigmasq")))
  dev.off()
  
  # Means and 95% central posterior intervals
  write.csv(as.table(summary(fit_G1)$summary[c("beta0", "beta1","sigmasq"), c("mean", "2.5%", "97.5%")]),
            file = paste("Summary Values for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  # plot of densities
  png(filename = paste(nams[i], " Density Plots Model 1.png", sep = ''), width = 1000, height = 700, res = 150)
  print(stan_dens(fit_G1, par = c("beta0", "beta1","sigmasq"),
            separate_chains = TRUE))
  dev.off()
  
  # distribution of Rb^2
  png(filename = paste(nams[i], " Rbsq plot Model 1.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(stan_dens(fit_G1, "Rbsq") + xlim(c(0.5, 1)))
  dev.off()
  
  ### Modle 1 Check ###
  
  # store nyrep samples of yrep
  # from from posterior predictive distribution
  nyrep = 10000
  logyrep_mod2 = matrix(0, nrow = nyrep, ncol = n)
  
  # rename for convenience
  logy_mod2 = y
  # sample  observations from posterior predictive distribution
  logiters_mod2 <- extract(fit_G1)
  logmu_mod2 <- logiters_mod2$mu
  for (j in seq_len(nyrep)) {
    for (k in 1:n) {
      logyrep_mod2[j, k] = rnorm(1, mean = logmu_mod2[j,k], sd = sqrt(logiters_mod2$sigmasq[j]))
    }  
  }
  
  # compare density of y and yrep
  library(bayesplot)
  png(filename = paste(nams[i], " Yrep Density Overlay.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_dens_overlay(logy_mod2, logyrep_mod2[1:20, ]))
  dev.off()
  
  # minimum of replicated sample
  logmins_mod2 = apply(logyrep_mod2, 1, min)
  # estimated p-value
  (sum(logmins_mod2 <= min(logy_mod2)) + 1)/(length(logmins_mod2) + 1)
  
  # histogram comparing T(y) and T(yrep)
  png(filename = paste(nams[i], " Yrep Min plot.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_stat(logy_mod2, logyrep_mod2, stat = "min"))
  dev.off()
  
  # look at asymmetry of distribution
  # by comparing order statistics to
  # samples of posterior mean
  logd_sim_mod2 = d_logobs_mod2 = numeric(nrow(logyrep_mod2))
  sort_logy_mod2 = sort(logy_mod2)
  
  for (j in 1:nrow(logyrep_mod2)) {
    thetai = logmu_mod2[j]
    sort_logyrep_mod2 = sort(logyrep_mod2[j, ])
    logd_sim_mod2[j] = abs(sort_logyrep_mod2[74] - logmu_mod2[j]) - 
      abs(sort_logyrep_mod2[7] - logmu_mod2[j])
    d_logobs_mod2[j] = abs(sort_logy_mod2[74] - logmu_mod2[j]) - 
      abs(sort_logy_mod2[7] - logmu_mod2[j])
  }
  # estimated posterior predictive p-value
  (sum(logd_sim_mod2 >= d_logobs_mod2) + 1)/(length(logd_sim_mod2) + 1)
  
  # compare observed and simulated discrepancy measures
  png(filename = paste(nams[i], " Yrep Center plot.png", sep = ''), width = 1000, height = 700, res = 150)
  print(plot(logd_sim_mod2 ~ d_logobs_mod2, xlab = "observed discrepancy", ylab = "simulated discrepancy"))
  abline(0, 1)
  dev.off()
  
}


#Do it for Pakistan too!
## There are way too many clusters to account for when combining all of the regions. I think
## it may be better to think about each site as being it's own population, so let us just look
## at the Guatemalan women

## Log transform
nams <- colnames(Pak[,11:36])
Pak[,nams] <- log(Pak[,nams])

#It won't run if NA's are present in the data. So remove any row that contains an NA (we will want to repeat this for
# each metabolite that we run!):
Pak <- Pak[complete.cases(Pak),]
## Dummy variable for Arm:
d2 <- (Pak$Arm == 1)*1

#Now we create the model in rstan
s2 <- (Pak$Cluster == "911")*1
s3 <- (Pak$Cluster == "912")*1
s4 <- (Pak$Cluster == "913")*1
s5 <- (Pak$Cluster == "915")*1
s6 <- (Pak$Cluster == "916")*1
s7 <- (Pak$Cluster == "917")*1
s8 <- (Pak$Cluster == "918")*1
s9 <- (Pak$Cluster == "919")*1
s10 <- (Pak$Cluster == "921")*1
s11 <- (Pak$Cluster == "922")*1
s12 <- (Pak$Cluster == "926")*1
s13 <- (Pak$Cluster == "931")*1

#cluster 932 is the cluster when all s values are 0

pl_modP1 = "
data {
int<lower=1> n;  // number of observations
vector[n] y;     // Response Variable (metabolite concentration)
vector[n] d2;    // Indicator for Arm 1
vector[n] s2;    // Incidator for Cluster
vector[n] s3;    // Indicator 
vector[n] s4;    // Indicator
vector[n] s5;    // Indicator
vector[n] s6;    // Indicator
vector[n] s7;    // Indicator
vector[n] s8;    // Indicator
vector[n] s9;    // Indicator
vector[n] s10;   // Indicator
vector[n] s11;   // Indicator
vector[n] s12;   // Indicator
vector[n] s13;   // Indicator
real<lower=0> v; // sample variance of y
}
parameters {
real<lower=0> sigmasq;
real beta0;
real beta1;
real beta2;
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real beta8;
real beta9;
real beta10;
real beta11;
real beta12;
real beta13;
}
transformed parameters {
vector[n] mu;           // mean of observations
for(i in 1:n) {
mu[i] = beta0 + beta1*d2[i] + beta2*s2[i] + beta3*s3[i] + beta4*s4[i] + beta5*s5[i] + beta6*s6[i] + beta7*s7[i] + beta8*s8[i] + beta9*s9[i] + beta10*s10[i] + beta11*s11[i] + beta12*s12[i] + beta13*s13[i];
}
}
model {
// prior distributions
sigmasq ~ inv_gamma(.01, .01);
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
beta2 ~ normal(0, 100);
beta3 ~ normal(0, 100);
beta4 ~ normal(0, 100);
beta5 ~ normal(0, 100);
beta6 ~ normal(0, 100);
beta7 ~ normal(0, 100);
beta8 ~ normal(0, 100);
beta9 ~ normal(0, 100);
beta10 ~ normal(0, 100);
beta11 ~ normal(0, 100);
beta12 ~ normal(0, 100);
beta13 ~ normal(0, 100);
// data distribution
for(i in 1:n) y[i] ~ normal(mu[i], sqrt(sigmasq));
}
generated quantities {
real Rbsq;              // goodness-of-fit
real log_lik[n];      // log likelihood of each observation
Rbsq = 1 - sigmasq/v;
for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(sigmasq));
}
"
# Specify the data in R, using a list
# format compatible with STAN:

loic_results <- matrix(0, nrow = length(nams), ncol = 2)
loic_results[,1] <- nams
library(loo)
for (i in 1: length(nams)){
  #Set the directory to save all of the files/images to!
  setwd(paste("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project\\May 13\\", nams[i], sep =''))
  
  #Run the model for the specific metabolite:
  n = length(Pak[,nams[i]])
  y = Pak[, nams[i]]
  v = var(y)
  
  datG1 <- list(n = n, y = y, d2 = d2, s2 = s2, s3 = s3, s4 = s4,
                s5 = s5, s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10, s11 = s11, s12 = s12, s13 = s13,
                v = v)
  
  # draw samples from the model
  fit_G1 <- stan(model_code = pl_modP1, data = datG1, iter = 100000,
                 control = list(adapt_delta = 0.99),
                 seed = 43)
  
  ## Analyze Model 
  # check convergence with gelman-rubin statistics
  write.csv(as.table(summary(fit_G1)$summary[,"Rhat"]), file = paste("Gelman-Rubin Convergence for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  # check convergence with trace plots
  png(filename = paste(nams[i], " Trace Plots Model 1.png", sep = ''), width = 1500, height = 433, res = 150)
  print(stan_trace(fit_G1, c("beta0", "beta1","sigmasq")))
  dev.off()
  
  # Means and 95% central posterior intervals
  write.csv(as.table(summary(fit_G1)$summary[c("beta0", "beta1","sigmasq"), c("mean", "2.5%", "97.5%")]),
            file = paste("Summary Values for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  # plot of densities
  png(filename = paste(nams[i], " Density Plots Model 1.png", sep = ''), width = 1000, height = 700, res = 150)
  print(stan_dens(fit_G1, par = c("beta0", "beta1","sigmasq"),
                  separate_chains = TRUE))
  dev.off()
  
  # distribution of Rb^2
  png(filename = paste(nams[i], " Rbsq plot Model 1.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(stan_dens(fit_G1, "Rbsq") + xlim(c(-1, 1)))
  dev.off()
  
  ### Modle 1 Check ###
  
  # store nyrep samples of yrep
  # from from posterior predictive distribution
  nyrep = 10000
  logyrep_mod = matrix(0, nrow = nyrep, ncol = n)
  
  # rename for convenience
  logy_mod = y
  # sample  observations from posterior predictive distribution
  logiters_mod <- rstan::extract(fit_G1)
  logmu_mod <- logiters_mod$mu
  for (j in seq_len(nyrep)) {
    for (k in 1:n) {
      logyrep_mod[j, k] = rnorm(1, mean = logmu_mod[j,k], sd = sqrt(logiters_mod$sigmasq[j]))
    }  
  }
  
  # compare density of y and yrep
  library(bayesplot)
  png(filename = paste(nams[i], " Yrep Density Overlay.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_dens_overlay(logy_mod, logyrep_mod[1:20, ]))
  dev.off()
  
  # minimum of replicated sample
  logmins_mod = apply(logyrep_mod, 1, min)
  # estimated p-value
  (sum(logmins_mod <= min(logy_mod)) + 1)/(length(logmins_mod) + 1)
  
  # histogram comparing T(y) and T(yrep)
  png(filename = paste(nams[i], " Yrep Min plot.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_stat(logy_mod, logyrep_mod, stat = "min"))
  dev.off()
  
  # look at asymmetry of distribution
  # by comparing order statistics to
  # samples of posterior mean
  logd_sim_mod = d_logobs_mod = numeric(nrow(logyrep_mod))
  sort_logy_mod = sort(logy_mod)
  minval <- max(which(sort_logy_mod <= quantile(sort_logy_mod, .10)))
  maxval <- min(which(sort_logy_mod >= quantile(sort_logy_mod, .90)))
  for (j in 1:nrow(logyrep_mod)) {
    thetai = logmu_mod[j]
    sort_logyrep_mod = sort(logyrep_mod[j, ])
    logd_sim_mod[j] = abs(sort_logyrep_mod[maxval] - logmu_mod[j]) - 
      abs(sort_logyrep_mod[minval] - logmu_mod[j])
    d_logobs_mod[j] = abs(sort_logy_mod[maxval] - logmu_mod[j]) - 
      abs(sort_logy_mod[minval] - logmu_mod[j])
  }
  # estimated posterior predictive p-value
  (sum(logd_sim_mod >= d_logobs_mod) + 1)/(length(logd_sim_mod) + 1)
  
  # compare observed and simulated discrepancy measures
  png(filename = paste(nams[i], " Yrep Center plot.png", sep = ''), width = 1000, height = 700, res = 150)
  print(plot(logd_sim_mod ~ d_logobs_mod, xlab = "observed discrepancy", ylab = "simulated discrepancy"))
  abline(0, 1)
  dev.off()
  
  # Look at bands for yrep sampling:
  png(filename =  paste(nams[i], " Yrep bands.png", sep =''), width = 1000, height = 700, res = 150)
  print(ppc_intervals(logy_mod, logyrep_mod))
  dev.off()
  
  # extract log likelihoods
  ll_pl1 = extract_log_lik(fit_G1, merge_chains = FALSE)
  
  # compute relative efficiency of log likelihoods
  # r_eff_pl = exp(relative_eff(ll_pl))
  r_eff_pl1 = exp(relative_eff(ll_pl1))

  # compute looic for each model
  loic_results[i,2] <- loo(ll_pl1,
                   r_eff = r_eff_pl1)$estimates[3,1]
  
}

#### Do a Hierarchical model for Pakistan!

## We begin with a model that says each cluster will have a random effect on the mean:
dat1 <- cbind(Pak$Cluster, Pak$Arm ,Pak$ADMA)
clusts <- levels(as.factor(Pak$Cluster))
for(j in 1: length(clusts)){  
  for(i in 1:length(Pak$ADMA)){
    if(clusts[j] == dat1[i,1]){
      dat1[i,1] <- j
    }
    if(dat1[i,2] == 2){
      dat1[i,2] <- 0
    }
  }
}

clustermod = "
data {
  int<lower=1> n;  // number of observations
  int<lower=1> m;  // number of clusters
  int<lower=1> clusterid[n]; // cluster identifier
  vector[n] y;  // vector of log metabolite values
  real<lower=0> v; // sample variance
  int<lower=0, upper = 1> d2[n];   // dummy for arm status
}
parameters {
  real<lower=0> sigmasq_a; // variance of between-cluster random effects
  real<lower=0> sigmasq;   // variance of within-cluster random effects
  vector[m] a;  // random effect for mean
  real beta0;   // intercept term
  real beta1;    // dummy for arm
}
transformed parameters {
  vector[n] mu;           // mean of observations
  for(i in 1:n) {
    mu[i] = beta0 + beta1*d2[i];
  }
}
model {
  // prior distributions
  beta0 ~ normal(0, sqrt(1000));
  beta1 ~ normal(0, sqrt(1000));
  sigmasq_a ~ inv_gamma(0.001, 0.001);
  sigmasq ~ inv_gamma(0.001, 0.001);

  // distribution of random effects for mean
  for(i in 1:m) {
    a[i] ~ normal(0, sqrt(sigmasq_a));
  }
  // data distribution
  for(i in 1:n) {
    y[i] ~ normal(mu[i] + a[clusterid[i]], sqrt(sigmasq));
  }
}
generated quantities {
  real<lower=0> tvar; // total variance
  real<lower=-1, upper=1> cor; //correlation
  real<lower=0> sigma; 
  real<lower=0> sigma_a;
  real Rbsq;              // goodness-of-fitt
  real log_lik[n];      // log likelihood of each observation
  tvar = sigmasq + sigmasq_a;
  cor = sigmasq_a/tvar;
  sigma = sqrt(sigmasq);
  sigma_a = sqrt(sigmasq_a);

  Rbsq = 1 - tvar/v;
  for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | (mu[i] + a[clusterid[i]]), sqrt(sigmasq));
}
"
loic_results <- cbind(loic_results, rep(0, times = length(loic_results[,1])))

for (i in 1:length(nams)){
  #Set the directory to save all of the files/images to!
  setwd(paste("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project\\May 13\\", nams[i], sep =''))
  
  dat1[,3] <- Pak[,nams[i]]

  # create the data list
  blood_data = list(n = length(dat1[,1]), m = length(clusts), clusterid = dat1[,1], y = dat1[,3], v = var(dat1[,3]),
                    d2 = dat1[,2])

  # draw samples from the model
  clusters_samples = stan(model_code = clustermod, data = blood_data, 
                      iter = 100000, seed = 23,  control = list(adapt_delta = 0.99,max_treedepth = 15))

  ## Analyze Model 
  # check convergence with gelman-rubin statistics
  write.csv(as.table(summary(clusters_samples)$summary[,"Rhat"]), file = paste("Gelman-Rubin Convergence for Model 2 for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)

  # check convergence with trace plots
  png(filename = paste(nams[i], " Trace Plots Model 2.png", sep = ''), width = 1500, height = 433, res = 150)
  print(stan_trace(clusters_samples, c("beta0", "beta1","sigmasq", "sigmasq_a")))
  dev.off()

  # Means and 95% central posterior intervals
  write.csv(as.table(summary(clusters_samples)$summary[c("beta0", "beta1","sigmasq", "sigmasq_a"), c("mean", "2.5%", "97.5%")]),
          file = paste("Summary Values for Model 2 for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)

  # plot of densities
  png(filename = paste(nams[i], " Density Plots Model 2.png", sep = ''), width = 1000, height = 700, res = 150)
  print(stan_dens(clusters_samples, par = c("beta0", "beta1","sigmasq", "sigmasq_a"),
                separate_chains = TRUE))
  dev.off()

  # distribution of Rb^2
  png(filename = paste(nams[i], " Rbsq plot Model 2.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(stan_dens(clusters_samples, "Rbsq") + xlim(c(0, 1)))
  dev.off()

  ### Modle 2 Check ###

  # store nyrep samples of yrep
  # from from posterior predictive distribution
  nyrep = 10000
  logyrep_mod1 = matrix(0, nrow = nyrep, ncol = length(dat1[,3]))

  # rename for convenience
  logy_mod1 = dat1[,3]
  # sample  observations from posterior predictive distribution
  logiters_mod1 <- rstan::extract(clusters_samples)
  loga_mod1 <- logiters_mod1$a
  logmu_mod1 <- logiters_mod1$mu
  for (j in seq_len(nyrep)) {
    for (k in 1:length(dat1[,3])) {
      logyrep_mod1[j, k] = rnorm(1, mean = (logmu_mod1[j,k] + loga_mod1[j, dat1[k,1]]), sd = sqrt(logiters_mod1$sigmasq[j]))
    }  
  }

  # compare density of y and yrep
  library(bayesplot)
  png(filename = paste(nams[i], " Yrep Density Overlay Model 2.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_dens_overlay(logy_mod1, logyrep_mod1[1:20, ]))
  dev.off()

  # minimum of replicated sample
  logmins_mod2 = apply(logyrep_mod1, 1, min)
  # estimated p-value
  (sum(logmins_mod1 <= min(logy_mod1)) + 1)/(length(logmins_mod1) + 1)

  # histogram comparing T(y) and T(yrep)
  png(filename = paste(nams[i], " Yrep Min plot Model 2.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_stat(logy_mod1, logyrep_mod1, stat = "min"))
  dev.off()

  # look at asymmetry of distribution
  # by comparing order statistics to
  # samples of posterior mean
  logd_sim_mod1 = d_logobs_mod1 = numeric(nrow(logyrep_mod2))
  sort_logy_mod1 = sort(logy_mod1)
  minval <- max(which(sort_logy_mod1 <= quantile(sort_logy_mod1, .10)))
  maxval <- min(which(sort_logy_mod1 >= quantile(sort_logy_mod1, .90)))
  for (j in 1:nrow(logyrep_mod1)) {
    thetai = logmu_mod1[j]
    sort_logyrep_mod1 = sort(logyrep_mod1[j, ])
    logd_sim_mod1[j] = abs(sort_logyrep_mod1[maxval] - logmu_mod1[j]) - 
    abs(sort_logyrep_mod1[minval] - logmu_mod1[j])
    d_logobs_mod1[j] = abs(sort_logy_mod1[maxval] - logmu_mod1[j]) - 
      abs(sort_logy_mod1[minval] - logmu_mod1[j])
  }
  # estimated posterior predictive p-value
  (sum(logd_sim_mod1 >= d_logobs_mod1) + 1)/(length(logd_sim_mod1) + 1)

  # compare observed and simulated discrepancy measures
  png(filename = paste(nams[i], " Yrep Center plot Model 2.png", sep = ''), width = 1000, height = 700, res = 150)
  print(plot(logd_sim_mod1 ~ d_logobs_mod1, xlab = "observed discrepancy", ylab = "simulated discrepancy"))
  abline(0, 1)
  dev.off()

  # Look at bands for yrep sampling:
  png(filename =  paste(nams[i], " Yrep bands Model 2.png", sep =''), width = 1000, height = 700, res = 150)
  print(ppc_intervals(logy_mod1, logyrep_mod1))
  dev.off()
  
  # extract log likelihoods
  ll_pl1 = extract_log_lik(clusters_samples, merge_chains = FALSE)

  # compute relative efficiency of log likelihoods
  # r_eff_pl = exp(relative_eff(ll_pl))
  r_eff_pl1 = exp(relative_eff(ll_pl1))

  # compute looic for the model
  loic_results[i,3] <- loo(ll_pl1,
                         r_eff = r_eff_pl1)$estimates[3,1]
}


clustermod2 = "
data {
int<lower=1> n;  // number of observations
int<lower=1> m;  // number of clusters
int<lower=1> clusterid[n]; // cluster identifier
vector[n] y;  // vector of log metabolite values
real<lower=0> v; // sample variance
int<lower=0, upper = 1> d2[n];   // dummy for arm status
}
parameters {
real<lower=0> sigmasq_a; // variance of between-cluster random effects
real<lower=0> sigmasq_b; // variance of individual-woman random effect
real<lower=0> sigmasq;   // variance of within-cluster random effects
vector[m] a;  // random effect for mean
vector[n] b;  // random effect for woman for mean
real beta0;   // intercept term
real beta1;    // dummy for arm
}
transformed parameters {
vector[n] mu;           // mean of observations
for(i in 1:n) {
mu[i] = beta0 + beta1*d2[i];
}
}
model {
// prior distributions
beta0 ~ normal(0, sqrt(1000));
beta1 ~ normal(0, sqrt(1000));
sigmasq_a ~ inv_gamma(0.001, 0.001);
sigmasq_b ~ inv_gamma(0.001, 0.001);
sigmasq ~ inv_gamma(0.001, 0.001);

// distribution of random effects for mean
for(i in 1:m) {
a[i] ~ normal(0, sqrt(sigmasq_a));
}
for(i in 1:n){
b[i] ~ normal(0, sqrt(sigmasq_b));
}
// data distribution
for(i in 1:n) {
y[i] ~ normal(mu[i] + a[clusterid[i]] + b[i], sqrt(sigmasq));
}
}
generated quantities {
real<lower=0> tvar; // total variance
real<lower=-1, upper=1> cor; //correlation
real<lower=0> sigma; 
real<lower=0> sigma_a;
real<lower=0> sigma_b;
real Rbsq;              // goodness-of-fitt
real log_lik[n];      // log likelihood of each observation
tvar = sigmasq + sigmasq_a + sigmasq_b;
cor = sigmasq_a/tvar;
sigma = sqrt(sigmasq);
sigma_a = sqrt(sigmasq_a);
sigma_b = sqrt(sigmasq_b);

Rbsq = 1 - tvar/v;
for (i in 1:n) log_lik[i] = normal_lpdf(y[i] | (mu[i] + a[clusterid[i]] + b[i]), sqrt(sigmasq));
}
"
loic_results <- cbind(loic_results, rep(0, times = length(loic_results[,1])))

for (i in 1:length(nams)){
  #Set the directory to save all of the files/images to!
  i <- 4
  setwd(paste("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project\\May 13\\", nams[i], sep =''))
  
  dat1[,3] <- Pak[,nams[i]]
  
  # create the data list
  blood_data = list(n = length(dat1[,1]), m = length(clusts), clusterid = dat1[,1], y = dat1[,3], v = var(dat1[,3]),
                    d2 = dat1[,2])
  
  # draw samples from the model
  clusters2_samples = stan(model_code = clustermod2, data = blood_data, 
                          iter = 100000, seed = 23,  control = list(adapt_delta = 0.99,max_treedepth = 15))
  
  ## Analyze Model 
  # check convergence with gelman-rubin statistics
  write.csv(as.table(summary(clusters2_samples)$summary[,"Rhat"]), file = paste("Gelman-Rubin Convergence for Model 3 for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  # check convergence with trace plots
  png(filename = paste(nams[i], " Trace Plots Model 3.png", sep = ''), width = 1500, height = 433, res = 150)
  print(stan_trace(clusters2_samples, c("beta0", "beta1","sigmasq", "sigmasq_a", "sigmasq_b")))
  dev.off()
  
  # Means and 95% central posterior intervals
  write.csv(as.table(summary(clusters2_samples)$summary[c("beta0", "beta1","sigmasq", "sigmasq_a", "sigmasq_b"), c("mean", "2.5%", "97.5%")]),
            file = paste("Summary Values for Model 3 for ", nams[i], ".csv", sep = ''), quote = FALSE, row.names = FALSE)
  
  png(filename = paste(nams[i], " Autocorrelation Plot Model 3.png", sep = ''), width = 800, height = 600, res = 150)
  print(stan_ac(clusters2_samples, "beta1", include = TRUE, unconstrain = FALSE,
                        inc_warmup = FALSE, nrow = NULL, ncol = NULL, 
                        separate_chains = FALSE, lags = 25, partial = FALSE))
  dev.off()
  
  # plot of densities
  png(filename = paste(nams[i], " Density Plots Model 3.png", sep = ''), width = 1000, height = 700, res = 150)
  print(stan_dens(clusters2_samples, par = c("beta0", "beta1","sigmasq", "sigmasq_a", "sigmasq_b"),
                  separate_chains = TRUE))
  dev.off()
  
  # distribution of Rb^2
  png(filename = paste(nams[i], " Rbsq plot Model 3.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(stan_dens(clusters2_samples, "Rbsq") + xlim(c(-1.5, 1)))
  dev.off()
  
  ### Modle 2 Check ###
  
  # store nyrep samples of yrep
  # from from posterior predictive distribution
  nyrep = 10000
  logyrep_mod2 = matrix(0, nrow = nyrep, ncol = length(dat1[,3]))
  
  # rename for convenience
  logy_mod2 = dat1[,3]
  # sample  observations from posterior predictive distribution
  logiters_mod2 <- rstan::extract(clusters2_samples)
  loga_mod2 <- logiters_mod2$a
  logb_mod2 <- logiters_mod2$b
  logmu_mod2 <- logiters_mod2$mu
  for (j in seq_len(nyrep)) {
    for (k in 1:length(dat1[,3])) {
      logyrep_mod2[j, k] = rnorm(1, mean = (logmu_mod2[j,k] + loga_mod2[j, dat1[k,1]] + logb_mod2[j,k]), sd = sqrt(logiters_mod2$sigmasq[j]))
    }  
  }
  
  # compare density of y and yrep
  color_scheme_set()
  par(bg = "blue")
  library(bayesplot)
  png(filename = paste(nams[i], " Yrep Density Overlay Model 3.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_dens_overlay(logy_mod2, logyrep_mod2[1:200, ]))
  dev.off()
  
  # minimum of replicated sample
  logmins_mod2 = apply(logyrep_mod2, 1, min)
  # estimated p-value
  (sum(logmins_mod2 <= min(logy_mod2)) + 1)/(length(logmins_mod2) + 1)
  
  # histogram comparing T(y) and T(yrep)
  png(filename = paste(nams[i], " Yrep Min plot Model 3.png", sep = ''), width = 1000, height = 1000, res = 150)
  print(ppc_stat(logy_mod2, logyrep_mod2, stat = "min"))
  dev.off()
  
  # look at asymmetry of distribution
  # by comparing order statistics to
  # samples of posterior mean
  logd_sim_mod2 = d_logobs_mod2 = numeric(nrow(logyrep_mod2))
  sort_logy_mod2 = sort(logy_mod2)
  minval <- max(which(sort_logy_mod2 <= quantile(sort_logy_mod2, .10)))
  maxval <- min(which(sort_logy_mod2 >= quantile(sort_logy_mod2, .90)))
  for (j in 1:nrow(logyrep_mod2)) {
    thetai = logmu_mod2[j]
    sort_logyrep_mod2 = sort(logyrep_mod2[j, ])
    logd_sim_mod2[j] = abs(sort_logyrep_mod2[maxval] - logmu_mod2[j]) - 
      abs(sort_logyrep_mod2[minval] - logmu_mod2[j])
    d_logobs_mod2[j] = abs(sort_logy_mod2[maxval] - logmu_mod2[j]) - 
      abs(sort_logy_mod2[minval] - logmu_mod2[j])
  }
  # estimated posterior predictive p-value
  (sum(logd_sim_mod2 >= d_logobs_mod2) + 1)/(length(logd_sim_mod2) + 1)
  
  # compare observed and simulated discrepancy measures
  png(filename = paste(nams[i], " Yrep Center plot Model 3.png", sep = ''), width = 1000, height = 700, res = 150)
  print(plot(logd_sim_mod2 ~ d_logobs_mod2, xlab = "observed discrepancy", ylab = "simulated discrepancy"))
  abline(0, 1)
  dev.off()
  
  # Look at bands for yrep sampling:
  png(filename =  paste(nams[i], " Yrep bands Model 3.png", sep =''), width = 1000, height = 700, res = 150)
  print(ppc_intervals(logy_mod2, logyrep_mod2))
  dev.off()
  
  # extract log likelihoods
  ll_pl1 = extract_log_lik(clusters2_samples, merge_chains = FALSE)
  
  # compute relative efficiency of log likelihoods
  # r_eff_pl = exp(relative_eff(ll_pl))
  r_eff_pl1 = exp(relative_eff(ll_pl1))
  
  # compute looic for the model
  loic_results[i,4] <- loo(ll_pl1,
                           r_eff = r_eff_pl1)$estimates[3,1]
}

setwd("C:\\Users\\weavenic\\Dropbox\\Bayesian Statistics\\Final Project\\May 13")
write.csv(loic_results, file = "Model Comparison.csv", row.names = FALSE, quote = FALSE)
