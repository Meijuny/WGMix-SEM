library(dplyr)
library(lavaan)
library(haven)
library(maps)
library(ggplot2)

################################################################################################
####################### PISA 2018 Data Management ##############################################
################################################################################################

##read the data in:
pisa2018<-read_sav("./Empirical Illustration/PISA2018.SAV")

##select the items we need:
pisa2018<-pisa2018 %>%
  dplyr::select(CNT, ##country
                ST004D01T, ##gender
                AGE, #age
                ST034Q01TA, ST034Q02TA, ST034Q03TA, ST034Q04TA, ST034Q05TA, ST034Q06TA, ##sense of belonging
                ST100Q01TA, ST100Q02TA, ST100Q03TA, ST100Q04TA, ##teacher support
                ST205Q01HA, ST205Q02HA, ST205Q03HA, ST205Q04HA, ##perceived competitiveness
                ST206Q01HA, ST206Q02HA, ST206Q03HA, ST206Q04HA, ##perceived cooperativity
                ST038Q03NA, ST038Q04NA, ST038Q05NA, ST038Q06NA, ST038Q07NA, ST038Q08NA ##bully exposure
  ) %>%
  rename(belong1=ST034Q01TA, belong2=ST034Q02TA, belong3=ST034Q03TA, belong4=ST034Q04TA, belong5=ST034Q05TA, belong6=ST034Q06TA, ##sense of belonging
         support1=ST100Q01TA, support2=ST100Q02TA, support3=ST100Q03TA, support4=ST100Q04TA, ##teacher support
         compete1=ST205Q01HA, compete2=ST205Q02HA, compete3=ST205Q03HA, compete4=ST205Q04HA, ##perceived competitiveness
         cooperate1=ST206Q01HA, cooperate2=ST206Q02HA, cooperate3=ST206Q03HA, cooperate4=ST206Q04HA, ##perceived cooperativity
         bully1=ST038Q03NA, bully2=ST038Q04NA, bully3=ST038Q05NA, bully4=ST038Q06NA, bully5=ST038Q07NA, bully6=ST038Q08NA ##bully exposure
  )

##make female dummy
pisa2018$female<-ifelse(pisa2018$ST004D01T==2,0,1)

##make age as numeric
pisa2018$AGE<-as.numeric(pisa2018$AGE)

##inspect the missingness for the items we want to study
#
##sense of belonging
Belong_NA<-pisa2018 %>%
  group_by(CNT) %>%
  summarise(belong1NA=sum(is.na(belong1))/n(),
            belong2NA=sum(is.na(belong2))/n(),
            belong3NA=sum(is.na(belong3))/n(),
            belong4NA=sum(is.na(belong4))/n(),
            belong5NA=sum(is.na(belong5))/n(),
            belong6NA=sum(is.na(belong6))/n()
  ) %>% filter((belong1NA=1)|(belong2NA==1)|(belong3NA==1)|
                 (belong4NA==1)|(belong5NA==1)|(belong6NA==1))

##ISR (Israel), LBN(Lebanon), MKD(North Macedonia) complete missing

##first take out the above three countries
pisa2018<-pisa2018 %>%
  filter((CNT!="ISR")&(CNT!="LBN")&(CNT!="MKD"))

#
##teacher support
TeacherSup_NA<-pisa2018 %>%
  group_by(CNT) %>%
  summarise(support1NA=sum(is.na(support1))/n(),
            support2NA=sum(is.na(support2))/n(),
            support3NA=sum(is.na(support3))/n(),
            support4NA=sum(is.na(support4))/n()
  ) %>% filter((support1NA==1)|(support2NA==1)|(support3NA==1)|
                 (support4NA==1))

##CAN [Canada] complete missing

##take out Canada
pisa2018<-pisa2018 %>%
  filter(CNT!="CAN")

#
##perceived competitiveness
Compete_NA<-pisa2018 %>%
  group_by(CNT) %>%
  summarise(compete1NA=sum(is.na(compete1))/n(),
            compete2NA=sum(is.na(compete2))/n(),
            compete3NA=sum(is.na(compete3))/n(),
            compete4NA=sum(is.na(compete4))/n()
  ) %>% filter((compete1NA==1)|(compete2NA==1)|(compete3NA==1)|
                 (compete4NA==1))
#
##perceived cooperativity
Cooperate_NA<-pisa2018 %>%
  group_by(CNT) %>%
  summarise(cooperate1NA=sum(is.na(cooperate1))/n(),
            cooperate2NA=sum(is.na(cooperate2))/n(),
            cooperate3NA=sum(is.na(cooperate3))/n(),
            cooperate4NA=sum(is.na(cooperate4))/n()
  ) %>% filter((cooperate1NA==1)|(cooperate2NA==1)|(cooperate3NA==1)|
                 (cooperate4NA==1))
#
##bully exposure
Bully_NA<-pisa2018 %>%
  group_by(CNT) %>%
  summarise(bully1NA=sum(is.na(bully1))/n(),
            bully2NA=sum(is.na(bully2))/n(),
            bully3NA=sum(is.na(bully3))/n(),
            bully4NA=sum(is.na(bully4))/n(),
            bully5NA=sum(is.na(bully5))/n(),
            bully6NA=sum(is.na(bully6))/n()
  ) %>% filter((bully1NA==1)|(bully2NA==1)|(bully3NA==1)|
                 (bully4NA==1)|(bully5NA==1)|(bully6NA==1))


##Sense of Belonging
#reverse belong2, belong3, belong5 so that higher value indicates higher level of belonging
pisa2018$belong2<-5-pisa2018$belong2
pisa2018$belong3<-5-pisa2018$belong3
pisa2018$belong5<-5-pisa2018$belong5

##teacher support
#reverse all support items so that higher value indicates stronger support
pisa2018$support1<-5-pisa2018$support1
pisa2018$support2<-5-pisa2018$support2
pisa2018$support3<-5-pisa2018$support3
pisa2018$support4<-5-pisa2018$support4

rm(list=c("Belong_NA","Bully_NA","Compete_NA","Cooperate_NA","TeacherSup_NA"))

##turn all the variables except CNT to numeric
pisa2018 <- pisa2018 %>%
  mutate(across(-c(CNT, female, ST004D01T), as.numeric))

# Center to the group mean
pisa2018 <- pisa2018 %>%
  group_by(CNT) %>%
  mutate(across(where(is.numeric) & -c(female, ST004D01T, AGE), ~. - mean(., na.rm = T))) %>%
  ungroup()


##focus only on the European and Asian countries:
european_countries <- c(
  "ALB",  # Albania
  "AUT",  # Austria
  "BEL",  # Belgium
  "BGR",  # Bulgaria
  "BIH",  # Bosnia and Herzegovina
  "BLR",  # Belarus
  "CHE",  # Switzerland
  "CZE",  # Czech Republic
  "DEU",  # Germany
  "DNK",  # Denmark
  "ESP",  # Spain
  "EST",  # Estonia
  "FIN",  # Finland
  "FRA",  # France
  "GBR",  # United Kingdom
  "GEO",  # Georgia
  "GRC",  # Greece
  "HRV",  # Croatia
  "HUN",  # Hungary
  "IRL",  # Ireland
  "ISL",  # Iceland
  "ITA",  # Italy
  "KSV",  # Kosovo
  "LTU",  # Lithuania
  "LUX",  # Luxembourg
  "LVA",  # Latvia
  "MDA",  # Moldova
  "MLT",  # Malta
  "MNE",  # Montenegro
  "NLD",  # Netherlands
  "NOR",  # Norway
  "POL",  # Poland
  "PRT",  # Portugal
  "ROU",  # Romania
  "RUS",  # Russia
  "SRB",  # Serbia
  "SVK",  # Slovakia
  "SVN",  # Slovenia
  "SWE",  # Sweden
  "UKR"   # Ukraine
)

asian_countries <- c(
  "BRN",  # Brunei
  "HKG",  # Hong Kong
  "IDN",  # Indonesia
  "JPN",  # Japan
  "KOR",  # South Korea
  "MAC",  # Macao
  "MYS",  # Malaysia
  "PHL",  # Philippines
  "QCI",  # B-S-J-Z China (PISA adjudicated region)
  "SGP",  # Singapore
  "TAP",  # Chinese Taipei (Taiwan)
  "THA",  # Thailand
  "VNM"   # Vietnam
)

pisa2018 <- pisa2018 %>%
  filter(CNT %in% c(european_countries, asian_countries))

##Take out countries with problematic measurement model (the configural invariant cannot be reached)
pisa2018<-pisa2018 %>%
  filter(!CNT %in% c("ALB","BGR","GEO","IDN","KSV","MNE","THA"))




################################################################################################
####################### PISA 2018 Measurement Model ############################################
################################################################################################

####-------------------------------------------------------------------------------------------
##Sense of Belonging
#
#start with the configural invariance (after taking out the problematic countries)
SenseOfBelong.MM.config1<-'
SenseOfBelong=~belong1+belong2+belong3+belong4+belong5+belong6
'

SenseOfBelong.fit.config1<-cfa(model = SenseOfBelong.MM.config1,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_config1.txt")
summary(SenseOfBelong.fit.config1, fit.measures=T, standardized=T)
sink()

SenseOfBelong.MI.config1<-modificationindices(SenseOfBelong.fit.config1, standardized = T)
SenseOfBelong.MI.config1<-SenseOfBelong.MI.config1 %>%
  mutate(Parameters=paste(lhs, op, rhs))
SenseOfBelong.MI.config1.summary<-SenseOfBelong.MI.config1  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())

##based on the modification indices, allow the residual covariances between belong2 and belong5 (belong2 ~~ belong5)
SenseOfBelong.MM.config2<-'
SenseOfBelong=~belong1+belong2+belong3+belong4+belong5+belong6

belong2 ~~ belong5
'

SenseOfBelong.fit.config2<-cfa(model = SenseOfBelong.MM.config2,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_config2.txt")
summary(SenseOfBelong.fit.config2, fit.measures=T, standardized=T)
sink()

SenseOfBelong.MI.config2<-modificationindices(SenseOfBelong.fit.config2, standardized = T)
SenseOfBelong.MI.config2<-SenseOfBelong.MI.config2 %>%
  mutate(Parameters=paste(lhs, op, rhs))
SenseOfBelong.MI.config2.summary<-SenseOfBelong.MI.config2  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())


##based on modification indices, allow residual covariances: belong2 ~~ belong3
SenseOfBelong.MM.config3<-'
SenseOfBelong=~belong1+belong2+belong3+belong4+belong5+belong6

belong2 ~~ belong5
belong2 ~~ belong3
'

SenseOfBelong.fit.config3<-cfa(model = SenseOfBelong.MM.config3,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_config3.txt")
summary(SenseOfBelong.fit.config3, fit.measures=T, standardized=T)
sink()

##check the modification indices:
SenseOfBelong.MI.config3<-modificationindices(SenseOfBelong.fit.config3, standardized = T)
SenseOfBelong.MI.config3<-SenseOfBelong.MI.config3 %>%
  mutate(Parameters=paste(lhs, op, rhs))
SenseOfBelong.MI.config3.summary<-SenseOfBelong.MI.config3  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())


##based on the modification indices, allow residual covariances: belong3 ~~ belong5
SenseOfBelong.MM.config4<-'
SenseOfBelong=~belong1+belong2+belong3+belong4+belong5+belong6

belong2 ~~ belong5
belong2 ~~ belong3
belong3 ~~ belong5
'

SenseOfBelong.fit.config4<-cfa(model = SenseOfBelong.MM.config4,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_config4.txt")
summary(SenseOfBelong.fit.config4, fit.measures=T, standardized=T)
sink()


##The fit is good, we continue with metric invariance:
#full metric
SenseOfBelong.MM.metric1<-'
SenseOfBelong=~belong1+belong2+belong3+belong4+belong5+belong6

belong2 ~~ belong5
belong2 ~~ belong3
belong3 ~~ belong5
'

SenseOfBelong.fit.metric1<-cfa(model = SenseOfBelong.MM.metric1,
                               data = pisa2018,
                               group = "CNT",
                               group.equal="loadings",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_metric1.txt")
summary(SenseOfBelong.fit.metric1, fit.measures=T, standardized=T)
sink()

##the drop in CFI is smaller than 0.02, and the increase in RMSEA is smaller than 0.03

#keep the full metric model
#change to marker variable approach
SenseOfBelong.MM.metric1.marker<-'
SenseOfBelong=~belong6+belong1+belong2+belong3+belong4+belong5

belong2 ~~ belong5
belong2 ~~ belong3
belong3 ~~ belong5
'

SenseOfBelong.fit.metric1.marker<-cfa(model = SenseOfBelong.MM.metric1.marker,
                                      data = pisa2018,
                                      group = "CNT",
                                      group.equal="loadings",
                                      estimator="MLR",
                                      missing="FIML")

sink("./Empirical results/PISA2018MeasurementModel2/SenseOfBelong_metric1_marker.txt")
summary(SenseOfBelong.fit.metric1.marker, fit.measures=T, standardized=T)
sink()


####-------------------------------------------------------------------------------------------
##Teacher Support
#start with configural invariance
TeacherSupport.MM.config1<-'
TeacherSupport=~support1+support2+support3+support4
'

TeacherSupport.fit.config1<-cfa(model = TeacherSupport.MM.config1,
                                data = pisa2018,
                                group = "CNT",
                                estimator="MLR",
                                missing="FIML",
                                std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/TeacherSupport_config1.txt")
summary(TeacherSupport.fit.config1, fit.measures=T, standardized=T)
sink()

##the fit is already good, move forward with full metric invariance
TeacherSupport.MM.metric1<-'
TeacherSupport=~support1+support2+support3+support4
'

TeacherSupport.fit.metric1<-cfa(model = TeacherSupport.MM.metric1,
                                data = pisa2018,
                                group = "CNT",
                                group.equal="loadings",
                                estimator="MLR",
                                missing="FIML",
                                std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/TeacherSupport_metric1.txt")
summary(TeacherSupport.fit.metric1, fit.measures=T, standardized=T)
sink()

#the full metric also has good fit
#
#switch to marker variable approach
TeacherSupport.MM.metric1.marker<-'
TeacherSupport=~support2+support1+support3+support4
'

TeacherSupport.fit.metric1.marker<-cfa(model = TeacherSupport.MM.metric1.marker,
                                       data = pisa2018,
                                       group = "CNT",
                                       group.equal="loadings",
                                       estimator="MLR",
                                       missing="FIML")

sink("./Empirical results/PISA2018MeasurementModel2/TeacherSupport_metric1_marker.txt")
summary(TeacherSupport.fit.metric1.marker, fit.measures=T, standardized=T)
sink()

####-------------------------------------------------------------------------------------------
##Perceived Competitiveness
#
#start with configural model
Competitiveness.MM.config1<-'
Competitiveness=~compete1+compete2+compete3+compete4
'

Competitiveness.fit.config1<-cfa(model = Competitiveness.MM.config1,
                                 data = pisa2018,
                                 group = "CNT",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Competitiveness_config1.txt")
summary(Competitiveness.fit.config1, fit.measures=T, standardized=T)
sink()

#check the modification indices:
Competitiveness.MI.config1<-modificationindices(Competitiveness.fit.config1, standardized = T)
Competitiveness.MI.config1<-Competitiveness.MI.config1 %>%
  mutate(Parameters=paste(lhs, op, rhs))
Competitiveness.MI.config1.summary<-Competitiveness.MI.config1  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())

##modification indices suggest residual covariances: compete2~~compete3
Competitiveness.MM.config2<-'
Competitiveness=~compete1+compete2+compete3+compete4

compete2~~compete3
'

Competitiveness.fit.config2<-cfa(model = Competitiveness.MM.config2,
                                 data = pisa2018,
                                 group = "CNT",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Competitiveness_config2.txt")
summary(Competitiveness.fit.config2, fit.measures=T, standardized=T)
sink()

##the fit is acceptable, move forward with full metric invariance
Competitiveness.MM.metric1<-'
Competitiveness=~compete1+compete2+compete3+compete4

compete2~~compete3
'

Competitiveness.fit.metric1<-cfa(model = Competitiveness.MM.metric1,
                                 data = pisa2018,
                                 group = "CNT",
                                 group.equal="loadings",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Competitiveness_metric1.txt")
summary(Competitiveness.fit.metric1, fit.measures=T, standardized=T)
sink()

##the drop in CFI is smaller than 0.02 and the increase in RMSEA is larger than 0.03
#switch to marker variable approach
Competitiveness.MM.metric1.marker<-'
Competitiveness=~compete1+compete2+compete3+compete4

compete2~~compete3
'

Competitiveness.fit.metric1.marker<-cfa(model = Competitiveness.MM.metric1.marker,
                                        data = pisa2018,
                                        group = "CNT",
                                        group.equal="loadings",
                                        estimator="MLR",
                                        missing="FIML")

sink("./Empirical results/PISA2018MeasurementModel2/Competitiveness_metric1_marker.txt")
summary(Competitiveness.fit.metric1.marker, fit.measures=T, standardized=T)
sink()

####-------------------------------------------------------------------------------------------
##Perceived Cooperativeness
#
#start with configural model
Cooperativeness.MM.config1<-'
Cooperativeness=~cooperate1+cooperate2+cooperate3+cooperate4
'

Cooperativeness.fit.config1<-cfa(model = Cooperativeness.MM.config1,
                                 data = pisa2018,
                                 group = "CNT",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Cooperativeness_config1.txt")
summary(Cooperativeness.fit.config1, fit.measures=T, standardized=T)
sink()

#check the modification indices:
Cooperativeness.MI.config1<-modificationindices(Cooperativeness.fit.config1, standardized = T)
Cooperativeness.MI.config1<-Cooperativeness.MI.config1 %>%
  mutate(Parameters=paste(lhs, op, rhs))
Cooperativeness.MI.config1.summary<-Cooperativeness.MI.config1  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())

##modification indices suggest residual covariances cooperate3~~cooperate4
Cooperativeness.MM.config2<-'
Cooperativeness=~cooperate1+cooperate2+cooperate3+cooperate4

cooperate3~~cooperate4
'

Cooperativeness.fit.config2<-cfa(model = Cooperativeness.MM.config2,
                                 data = pisa2018,
                                 group = "CNT",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Cooperativeness_config2.txt")
summary(Cooperativeness.fit.config2, fit.measures=T, standardized=T)
sink()

##the fit is good, proceed with full metric invariance
Cooperativeness.MM.metric1<-'
Cooperativeness=~cooperate1+cooperate2+cooperate3+cooperate4

cooperate3~~cooperate4
'

Cooperativeness.fit.metric1<-cfa(model = Cooperativeness.MM.metric1,
                                 data = pisa2018,
                                 group = "CNT",
                                 group.equal="loadings",
                                 estimator="MLR",
                                 missing="FIML",
                                 std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/Cooperativeness_metric1.txt")
summary(Cooperativeness.fit.metric1, fit.measures=T, standardized=T)
sink()

##the fit is good
#switch to marker variable approach
Cooperativeness.MM.metric1.marker<-'
Cooperativeness=~cooperate2+cooperate1+cooperate3+cooperate4

cooperate3~~cooperate4
'

Cooperativeness.fit.metric1.marker<-cfa(model = Cooperativeness.MM.metric1.marker,
                                        data = pisa2018,
                                        group = "CNT",
                                        group.equal="loadings",
                                        estimator="MLR",
                                        missing="FIML")

sink("./Empirical results/PISA2018MeasurementModel2/Cooperativeness_metric1_marker.txt")
summary(Cooperativeness.fit.metric1.marker, fit.measures=T, standardized=T)
sink()

####-------------------------------------------------------------------------------------------
##Bully Exposure
#
#start with configural model
BullyExposure.MM.config1<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6
'

BullyExposure.fit.config1<-cfa(model = BullyExposure.MM.config1,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_config1.txt")
summary(BullyExposure.fit.config1, fit.measures=T, standardized=T)
sink()

#check the modification indices:
BullyExposure.MI.config1<-modificationindices(BullyExposure.fit.config1, standardized = T)
BullyExposure.MI.config1<-BullyExposure.MI.config1 %>%
  mutate(Parameters=paste(lhs, op, rhs))
BullyExposure.MI.config1.summary<-BullyExposure.MI.config1  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())

##modification indices suggest residual covariance: bully4~~bully5 (matching theory of direct bully)
BullyExposure.MM.config2<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6

bully4~~bully5
'

BullyExposure.fit.config2<-cfa(model = BullyExposure.MM.config2,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_config2.txt")
summary(BullyExposure.fit.config2, fit.measures=T, standardized=T)
sink()

#check the modification indices:
BullyExposure.MI.config2<-modificationindices(BullyExposure.fit.config2, standardized = T)
BullyExposure.MI.config2<-BullyExposure.MI.config2 %>%
  mutate(Parameters=paste(lhs, op, rhs))
BullyExposure.MI.config2.summary<-BullyExposure.MI.config2  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())


##modification indices suggest residual covariance: bully3~~bully5 (direct bully)
BullyExposure.MM.config3<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6

bully4~~bully5
bully3~~bully5
'

BullyExposure.fit.config3<-cfa(model = BullyExposure.MM.config3,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_config3.txt")
summary(BullyExposure.fit.config3, fit.measures=T, standardized=T)
sink()

#check the modification indices:
BullyExposure.MI.config3<-modificationindices(BullyExposure.fit.config3, standardized = T)
BullyExposure.MI.config3<-BullyExposure.MI.config3 %>%
  mutate(Parameters=paste(lhs, op, rhs))
BullyExposure.MI.config3.summary<-BullyExposure.MI.config3  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())


##modification indices suggest residual covariances: bully3~~bully4 (direct bully)
BullyExposure.MM.config4<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6

bully4~~bully5
bully3~~bully5
bully3~~bully4
'

BullyExposure.fit.config4<-cfa(model = BullyExposure.MM.config4,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_config4.txt")
summary(BullyExposure.fit.config4, fit.measures=T, standardized=T)
sink()

#check the modification indices:
BullyExposure.MI.config4<-modificationindices(BullyExposure.fit.config4, standardized = T)
BullyExposure.MI.config4<-BullyExposure.MI.config4 %>%
  mutate(Parameters=paste(lhs, op, rhs))
BullyExposure.MI.config4.summary<-BullyExposure.MI.config4  %>%
  filter(mi>=20) %>%
  group_by(Parameters) %>%
  summarise(MICount=n())

##modification indices suggest residual covariances: bully1~~bully2 (relational bully)
BullyExposure.MM.config5<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6

bully4~~bully5
bully3~~bully5
bully3~~bully4
bully1~~bully2
'

BullyExposure.fit.config5<-cfa(model = BullyExposure.MM.config5,
                               data = pisa2018,
                               group = "CNT",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_config5.txt")
summary(BullyExposure.fit.config5, fit.measures=T, standardized=T)
sink()

##the fit is good, proceed with the full metric invariance
BullyExposure.MM.metric1<-'
BullyExposure=~bully1+bully2+bully3+bully4+bully5+bully6

bully4~~bully5
bully3~~bully5
bully3~~bully4
bully1~~bully2
'

BullyExposure.fit.metric1<-cfa(model = BullyExposure.MM.metric1,
                               data = pisa2018,
                               group = "CNT",
                               group.equal="loadings",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_metric1.txt")
summary(BullyExposure.fit.metric1, fit.measures=T, standardized=T)
sink()

##the drop in CFI is smaller than 0.02, and the increase in RMSEA is smaller than 0.03
#switch to marker variable approach
BullyExposure.MM.metric1.marker<-'
BullyExposure=~bully6+bully1+bully2+bully3+bully4+bully5

bully4~~bully5
bully3~~bully5
bully3~~bully4
bully1~~bully2
'

BullyExposure.fit.metric1.marker<-cfa(model = BullyExposure.MM.metric1.marker,
                                      data = pisa2018,
                                      group = "CNT",
                                      group.equal="loadings",
                                      estimator="MLR",
                                      missing="FIML")

sink("./Empirical results/PISA2018MeasurementModel2/BullyExposure_metric1_marker.txt")
summary(BullyExposure.fit.metric1.marker, fit.measures=T, standardized=T)
sink()



##############################################################################################
####################### PISA 2018 factor score DF ############################################
##############################################################################################


##--------------------------------------------------------------------------------------------
#Sense of Belonging
##first run the final measurement model with marker variable approach
SenseOfBelong.MM.metric1.marker<-'
SenseOfBelong=~belong6+belong1+belong2+belong3+belong4+belong5

belong2 ~~ belong5
belong2 ~~ belong3
belong3 ~~ belong5
'

SenseOfBelong.fit.metric1.marker<-cfa(model = SenseOfBelong.MM.metric1.marker,
                                      data = pisa2018,
                                      group = "CNT",
                                      group.equal="loadings",
                                      estimator="MLR",
                                      missing="FIML")

#compute factor scores and factor scores std
SenseOfBelong.FactorScores<-lavPredict(SenseOfBelong.fit.metric1.marker,
                                       se="standard",
                                       assemble = T)

SenseOfBelong.std<-attr(SenseOfBelong.FactorScores, "se")
SenseOfBelong.std<-unlist(SenseOfBelong.std, use.names = F)

SenseOfBelong.FactorScores<-SenseOfBelong.FactorScores %>%
  mutate(id=row_number(),
         SenseOfBelong_std=SenseOfBelong.std,
         female=pisa2018$female,
         AGE=pisa2018$AGE) %>%
  dplyr::select(id, SenseOfBelong, SenseOfBelong_std, CNT,female,AGE)



##--------------------------------------------------------------------------------------------
#Teacher Support
##first run the final measurement model with marker variable approach
TeacherSupport.MM.metric1.marker<-'
TeacherSupport=~support2+support1+support3+support4
'

TeacherSupport.fit.metric1.marker<-cfa(model = TeacherSupport.MM.metric1.marker,
                                       data = pisa2018,
                                       group = "CNT",
                                       group.equal="loadings",
                                       estimator="MLR",
                                       missing="FIML")

#compute factor scores and factor scores std
TeacherSupport.FactorScores<-lavPredict(TeacherSupport.fit.metric1.marker,
                                        se="standard",
                                        assemble = T)

TeacherSupport.std<-attr(TeacherSupport.FactorScores, "se")
TeacherSupport.std<-unlist(TeacherSupport.std, use.names = F)

TeacherSupport.FactorScores<-TeacherSupport.FactorScores %>%
  mutate(id=row_number(),
         TeacherSupport_std=TeacherSupport.std) %>%
  dplyr::select(id, TeacherSupport, TeacherSupport_std, CNT)


##--------------------------------------------------------------------------------------------
#perceived competitiveness
##first run the final measurement model with marker variable approach
Competitiveness.MM.metric1.marker<-'
Competitiveness=~compete1+compete2+compete3+compete4

compete2~~compete3
'

Competitiveness.fit.metric1.marker<-cfa(model = Competitiveness.MM.metric1.marker,
                                        data = pisa2018,
                                        group = "CNT",
                                        group.equal="loadings",
                                        estimator="MLR",
                                        missing="FIML")

#compute factor scores and factor scores std
Competitiveness.FactorScores<-lavPredict(Competitiveness.fit.metric1.marker,
                                         se="standard",
                                         assemble = T)

Competitiveness.std<-attr(Competitiveness.FactorScores, "se")
Competitiveness.std<-unlist(Competitiveness.std, use.names = F)

Competitiveness.FactorScores<-Competitiveness.FactorScores %>%
  mutate(id=row_number(),
         Competitiveness_std=Competitiveness.std) %>%
  dplyr::select(id, Competitiveness, Competitiveness_std, CNT)


##--------------------------------------------------------------------------------------------
#perceived cooperativeness
##first run the final measurement model with marker variable approach
Cooperativeness.MM.metric1.marker<-'
Cooperativeness=~cooperate2+cooperate1+cooperate3+cooperate4

cooperate3~~cooperate4
'

Cooperativeness.fit.metric1.marker<-cfa(model = Cooperativeness.MM.metric1.marker,
                                        data = pisa2018,
                                        group = "CNT",
                                        group.equal="loadings",
                                        estimator="MLR",
                                        missing="FIML")

#compute factor scores and factor scores std
Cooperativeness.FactorScores<-lavPredict(Cooperativeness.fit.metric1.marker,
                                         se="standard",
                                         assemble = T)

Cooperativeness.std<-attr(Cooperativeness.FactorScores, "se")
Cooperativeness.std<-unlist(Cooperativeness.std, use.names = F)

Cooperativeness.FactorScores<-Cooperativeness.FactorScores %>%
  mutate(id=row_number(),
         Cooperativeness_std=Cooperativeness.std) %>%
  dplyr::select(id, Cooperativeness, Cooperativeness_std, CNT)


##--------------------------------------------------------------------------------------------
#Bully Exposure
##first run the final measurement model with marker variable approach
BullyExposure.MM.metric1.marker<-'
BullyExposure=~bully6+bully1+bully2+bully3+bully4+bully5

bully4~~bully5
bully3~~bully5
bully3~~bully4
bully1~~bully2
'

BullyExposure.fit.metric1.marker<-cfa(model = BullyExposure.MM.metric1.marker,
                                      data = pisa2018,
                                      group = "CNT",
                                      group.equal="loadings",
                                      estimator="MLR",
                                      missing="FIML")

#compute factor scores and factor scores std
BullyExposure.FactorScores<-lavPredict(BullyExposure.fit.metric1.marker,
                                       se="standard",
                                       assemble = T)

BullyExposure.std<-attr(BullyExposure.FactorScores, "se")
BullyExposure.std<-unlist(BullyExposure.std, use.names = F)

BullyExposure.FactorScores<-BullyExposure.FactorScores %>%
  mutate(id=row_number(),
         BullyExposure_std=BullyExposure.std) %>%
  dplyr::select(id, BullyExposure, BullyExposure_std, CNT)

##put all factor scores and factor score uncertainty for all three factors into a df
pisa2018_factorScoreDF<-merge(SenseOfBelong.FactorScores, TeacherSupport.FactorScores,
                              by.x = "id", by.y="id") %>%
  dplyr::select(-CNT.y)

pisa2018_factorScoreDF<-merge(pisa2018_factorScoreDF, Competitiveness.FactorScores,
                              by.x = "id", by.y = "id") %>%
  dplyr::select(-CNT.x)

pisa2018_factorScoreDF<-merge(pisa2018_factorScoreDF, Cooperativeness.FactorScores,
                              by.x = "id", by.y = "id") %>%
  dplyr::select(-CNT.y)

pisa2018_factorScoreDF<-merge(pisa2018_factorScoreDF, BullyExposure.FactorScores,
                              by.x = "id", by.y = "id") %>%
  dplyr::select(-CNT.x)

write.csv(pisa2018_factorScoreDF, file = "./Empirical Illustration/pisa2018_factorScoreDF_updated.csv",
          row.names = F)



#############################################################################################################
####################### PISA 2018 extra analysis for the results ############################################
#############################################################################################################

library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(patchwork)
library(ggpattern)


###----------------------------------------------------------------------------------------------------
##model selection
##without clustering at the group level
LL<-c(-1027904.9595,
      -1021889.8998,
      -1019509.9231,
      -1018447.5757,
      -1017922.1570,
      -1017571.2325
)

Npar<-c(465,
        471,
        477,
        483,
        489,
        494
)

BIC_N<-c(2061566.1209,
         2049610.2749,
         2044924.5953,
         2042874.1740,
         2041897.6102,
         2041257.6559
)

BIC_G<-c(
  2057590.2373,
  2045583.0896,
  2040846.1082,
  2038744.3853,
  2037716.5197,
  2037033.8139
)

Class<-1:6

##CHull scree ratio
#
##select (-2)loglikelihood as the goodness of fit measures
#
CHull_df<-data.frame(Npar=Npar,
                     LL=LL)

CHull_df$CHull <- "-"

for (i in 2:(nrow(CHull_df) - 1)) {
  left_slope  <- (CHull_df$LL[i-1] - CHull_df$LL[i])   /
    (CHull_df$Npar[i]  - CHull_df$Npar[i-1])
  right_slope <- (CHull_df$LL[i]   - CHull_df$LL[i+1]) /
    (CHull_df$Npar[i+1] - CHull_df$Npar[i])
  CHull_df$CHull[i] <- round(left_slope / right_slope, 4)
}


##CHull and BIC plot
par(mfrow=c(1,2))
hull_indices <- chull(Npar, LL)
plot(Npar, LL, 
     pch=4, col="black", main="CHull Scree Plot",xlab="Number of free parameters", ylab="Loglikelihood")
lines(Npar[hull_indices], 
      LL[hull_indices], col="black", lwd=1)

plot(Class,BIC_G, pch=4, col="black",
     main="BIC_G plot", xlab="number of individual-clusters", ylab="BIC_G")
lines(Class, BIC_G, col="black", lwd=1)

par(mfrow=c(1,1))


###----------------------------------------------------------------------------------------------------
##model selection
##2 ind-cluster
LL_2IndClus<-c(-1021889.8998,
               -1021452.9706,
               -1021298.0756,
               -1021245.6659,
               -1021214.0862
)

Npar_2IndClus<-c(471,
                 473,
                 475,
                 477,
                 479
)

BIC_N_2IndClus<-c(2049610.2749,
                  2048761.1746,
                  2048476.1422,
                  2048396.0808,
                  2048357.6793
)

BIC_G_2IndClus<-c(2045583.0896,
                  2044716.8887,
                  2044414.7558,
                  2044317.5938,
                  2044262.0916
)


##3 ind-clus
LL_3IndClus<-c(-1019509.9231,
               -1018668.0252,
               -1018333.6580,
               -1018132.7907,
               -1017998.8150
)

Npar_3IndClus<-c(477,
                 480,
                 483,
                 486,
                 489
)

BIC_N_3IndClus<-c(2044924.5953,
                  2043277.9363,
                  2042646.3386,
                  2042281.7408,
                  2042050.9262
)

BIC_G_3IndClus<-c(2040846.1082,
                  2039173.7984,
                  2038516.5499,
                  2038126.3012,
                  2037869.8357
)

GClass<-1:5

##CHull scree ratio
#
#2indClus
CHull_df_2IndClus<-data.frame(Npar=Npar_2IndClus,
                              LL=LL_2IndClus)

CHull_df_2IndClus$CHull <- "-"

for (i in 2:(nrow(CHull_df_2IndClus) - 1)) {
  left_slope  <- (CHull_df_2IndClus$LL[i-1] - CHull_df_2IndClus$LL[i])   /
    (CHull_df_2IndClus$Npar[i]  - CHull_df_2IndClus$Npar[i-1])
  right_slope <- (CHull_df_2IndClus$LL[i]   - CHull_df_2IndClus$LL[i+1]) /
    (CHull_df_2IndClus$Npar[i+1] - CHull_df_2IndClus$Npar[i])
  CHull_df_2IndClus$CHull[i] <- round(left_slope / right_slope, 4)
}

CHull_df_2IndClus

#
#3indClus
CHull_df_3IndClus<-data.frame(Npar=Npar_3IndClus,
                              LL=LL_3IndClus)

CHull_df_3IndClus$CHull <- "-"

for (i in 2:(nrow(CHull_df_3IndClus) - 1)) {
  left_slope  <- (CHull_df_3IndClus$LL[i-1] - CHull_df_3IndClus$LL[i])   /
    (CHull_df_3IndClus$Npar[i]  - CHull_df_3IndClus$Npar[i-1])
  right_slope <- (CHull_df_3IndClus$LL[i]   - CHull_df_3IndClus$LL[i+1]) /
    (CHull_df_3IndClus$Npar[i+1] - CHull_df_3IndClus$Npar[i])
  CHull_df_3IndClus$CHull[i] <- round(left_slope / right_slope, 4)
}

CHull_df_3IndClus

# Layout: 7 rows — row 1 = title for panel 1, rows 2-3 = plots,
#                   row 4 = title for panel 2, rows 5-6 = plots
layout(matrix(c(1, 1, 
                2, 3, 
                2, 3, 
                4, 4, 
                5, 6,
                5, 6), nrow = 6, byrow = TRUE),
       heights = c(0.3, 1, 1, 0.3, 1, 1))

# --- Panel title: 2 Individual Clusters ---
par(mar = c(0, 0, 0, 0))
plot.new()
text(0.5, 0.5, "2 Individual-Clusters", font = 2, cex = 1.4, adj = c(0.5, 0.5))

# --- Plot 1: CHull ---
par(mar = c(4, 4, 3, 1))
hull_2 <- chull(Npar_2IndClus, LL_2IndClus)
plot(Npar_2IndClus, LL_2IndClus,
     pch = 4, col = "blue",
     main = "CHull Scree Plot",
     xlab = "Number of free parameters", ylab = "Log-likelihood")
lines(Npar_2IndClus[hull_2], LL_2IndClus[hull_2], col = "blue", lwd = 1)


# --- Plot 2: BIC_G ---
plot(GClass, BIC_G_2IndClus,
     pch = 4, col = "blue",
     main = "BIC_G Plot",
     xlab = "Number of group-clusters", ylab = "BIC_G")
lines(GClass, BIC_G_2IndClus, col = "blue", lwd = 1)

# --- Panel title: 3 Individual Clusters ---
par(mar = c(0, 0, 0, 0))
plot.new()
text(0.5, 0.5, "3 Individual-Clusters", font = 2, cex = 1.4, adj = c(0.5, 0.5))

# --- Plot 3: CHull ---
par(mar = c(4, 4, 3, 1))
hull_3 <- chull(Npar_3IndClus, LL_3IndClus)
plot(Npar_3IndClus, LL_3IndClus,
     pch = 4, col = "red",
     main = "CHull Scree Plot",
     xlab = "Number of free parameters", ylab = "Log-likelihood")
lines(Npar_3IndClus[hull_3], LL_3IndClus[hull_3], col = "red", lwd = 1)


# --- Plot 4: BIC_G ---
plot(GClass, BIC_G_3IndClus,
     pch = 4, col = "red",
     main = "BIC_G Plot",
     xlab = "Number of group-clusters", ylab = "BIC_G")
lines(GClass, BIC_G_3IndClus, col = "red", lwd = 1)

# Reset layout
par(mfrow = c(1, 1))

####-----------------------------------------------------------------------------
##group-cluster profile with bar chart
EmpResults_lines<-readLines("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/WGMix-SEM/Empirical Illustration/PISA2018StructuralModel2/Belong_3IncClass_2GClass_SE.txt",
                            encoding = "latin1")

nclus_ind<-3
GroupClus_Profile_start_idx<-grep("^ProbMeans", EmpResults_lines)-nclus_ind
GroupClus_Profile_end_idx<-grep("^ProbMeans", EmpResults_lines)-1
GroupClus_Profile_lines<-EmpResults_lines[GroupClus_Profile_start_idx:GroupClus_Profile_end_idx]

##turn it into data frame:
GroupClus_Profile_df<-read.table(
  text = GroupClus_Profile_lines,
  sep = "\t",
  header = FALSE,
  stringsAsFactors = FALSE
)

##extract the necessary columns to make it into a matrix:
nclus_group<-2
keep_cols<-seq(from=2, by=1, length.out=nclus_group) 
GroupClus_Profile_mat<-as.matrix(GroupClus_Profile_df[,keep_cols])
rownames(GroupClus_Profile_mat)<-paste0("IndClus", GroupClus_Profile_df[[1]])
colnames(GroupClus_Profile_mat)<-paste0("GroupClus", seq_len(nclus_group))

GroupClus_Profile_long<-as.data.frame(GroupClus_Profile_mat) %>%
  rownames_to_column(var = "IndClus") %>%
  pivot_longer(cols = starts_with("GroupClus"),
               names_to = "GroupClus",
               values_to = "proportion") %>%
  mutate(IndClus=case_when(
    IndClus=="IndClus1" ~ "Ind-clus 1: peer cooperation-driven",
    IndClus=="IndClus2" ~ "Ind-clus 2: bully-sensitive",
    IndClus=="IndClus3" ~ "Ind-clus 3: potential noise"
  ),
  GroupClus=case_when(
    GroupClus=="GroupClus1"~"Group-cluster1",
    GroupClus=="GroupClus2"~"Group-cluster2"
  ))

GroupClusProfile_BarGraph<-ggplot(GroupClus_Profile_long, aes(x=GroupClus, y = proportion, pattern=IndClus, colour=GroupClus, pattern_fill=GroupClus))+
  geom_bar_pattern(stat = "identity",
                   fill="white",
                   pattern_angle=45,
                   pattern_density=0.1,
                   pattern_spacing=0.03,
                   pattern_key_scale_factor=0.6,
                   linewidth=0.8) +
  labs(x="Group-Cluster", y="Proportion of Individual-clusters", 
       pattern="Individual-Clusters")+
  scale_colour_manual(values = c("Group-cluster1"="#66C2A5",
                                 "Group-cluster2"="#FC8D62"),
                      guide = "none")+
  scale_pattern_fill_manual(values = c("Group-cluster1"="#66C2A5",
                                       "Group-cluster2"="#FC8D62"),
                            guide = "none")+
  #scale_fill_manual(values = c("Ind-clus 1: peer cooperation-driven" = "#4C72B0",  # muted blue
  #                             "Ind-clus 2: bully-sensitive" = "#DD8452",  # muted orange
  #                             "Ind-clus 3: potential noise" = "#55A868"))+ # muted green
  scale_pattern_manual(values = c("Ind-clus 1: peer cooperation-driven"="stripe",
                                  "Ind-clus 2: bully-sensitive"="circle",
                                  "Ind-clus 3: potential noise"="none"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        #legend.text = element_text(size = 12),
        #legend.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))

GroupClusProfile_BarGraph

###--------------------------------------------------------------------------------
##geographical patterns

#read the GClass membership results in:
EmpResults_lines<-readLines("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/WGMix-SEM/Empirical Illustration/PISA2018StructuralModel2/Belong_3IncClass_2GClass_SE.txt",
                            encoding = "latin1")

start_idx<-grep("^ProbMeans", EmpResults_lines)+6
end_idx<-grep("^EstimatedValues-Regression", EmpResults_lines)-1

GClassMember_lines<-EmpResults_lines[start_idx:end_idx]
GClassMember_lines_parsed<-strsplit(GClassMember_lines,"\t")

GClassMember_df <- data.frame(
  Country = sapply(GClassMember_lines_parsed, `[`, 1),
  GClass1 = as.numeric(sapply(GClassMember_lines_parsed, `[`, 2)),
  GClass2 = as.numeric(sapply(GClassMember_lines_parsed, `[`, 3)),
  stringsAsFactors = FALSE
)

GClassMember_df[,2:3]<-t(apply(GClassMember_df[,2:3], 1, function(x) as.numeric(x==max(x))))
GClassMember_df[,3]<-ifelse(GClassMember_df[,3]==1, 2, 0)
GClassMember_df<-GClassMember_df %>%
  mutate(GClassMembership=GClass1+GClass2) %>%
  dplyr::select(Country, GClassMembership)

##rename the country to match the world_map country name
GClassMember_df <- GClassMember_df %>%
  mutate(region=case_when(
    Country=="AUT"~"Austria",
    Country=="BEL"~"Belgium",
    Country=="BIH"~"Bosnia and Herzegovina",
    Country=="BLR"~"Belarus",
    Country=="BRN"~"Brunei",
    Country=="CHE"~"Switzerland",
    Country=="CZE"~"Czech Republic",
    Country=="DEU"~"Germany",
    Country=="DNK"~"Denmark",
    Country=="ESP"~"Spain",
    Country=="EST"~"Estonia",
    Country=="FIN"~"Finland",
    Country=="FRA"~"France",
    Country=="GBR"~"UK",
    Country=="GRC"~"Greece",
    Country=="HKG"~"Hong Kong", ##subregion
    Country=="HRV"~"Croatia",
    Country=="HUN"~"Hungary",
    Country=="IRL"~"Ireland",
    Country=="ISL"~"Iceland",
    Country=="ITA"~"Italy",
    Country=="JPN"~"Japan",
    Country=="KOR"~"South Korea",
    Country=="LTU"~"Lithuania",
    Country=="LUX"~"Luxembourg",
    Country=="LVA"~"Latvia",
    Country=="MAC"~"Macao", ##subregion
    Country=="MDA"~"Moldova",
    Country=="MLT"~"Malta",
    Country=="MYS"~"Malaysia",
    Country=="NLD"~"Netherlands",
    Country=="NOR"~"Norway",
    Country=="PHL"~"Philippines",
    Country=="POL"~"Poland",
    Country=="PRT"~"Portugal",
    Country=="QCI"~"China",
    Country=="ROU"~"Romania",
    Country=="RUS"~"Russia",
    Country=="SGP"~"Singapore",
    Country=="SRB"~"Serbia",
    Country=="SVK"~"Slovakia",
    Country=="SVN"~"Slovenia",
    Country=="SWE"~"Sweden",
    Country=="TAP"~"Taiwan",
    Country=="UKR"~"Ukraine",
    Country=="VNM"~"Vietnam"
  ))

##load the world map data
world_map<-map_data("world")

world_map<-world_map %>%
  mutate(region = case_when(
    subregion=="Hong Kong" ~ "Hong Kong",
    subregion=="Macao" ~ "Macao",
    TRUE ~ region
  ))

##merge the world_map data with our GClass membership data
map_GClass <- world_map %>%
  left_join(GClassMember_df, by = "region")

ggplot(map_GClass, aes(long, lat, group=group, fill=factor(GClassMembership)))+
  geom_polygon(color="white")+
  labs(fill="GClass")+
  theme_minimal()+
  coord_cartesian(xlim = c(-25, 145), ylim = c(-30, 90))

GroupClusterMap<-ggplot(map_GClass, aes(long, lat, group = group, fill = factor(GClassMembership))) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("1" = "#66C2A5", "2" = "#FC8D62"),
    na.value = "grey80",
    labels = c("1" = "Group-Cluster 1", "2" = "Group-Cluster 2"),
    name = "Group-Clusters"
  ) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )+
  coord_cartesian(xlim = c(-25, 145), ylim = c(-5, 90))

combined_plot<-(GroupClusProfile_BarGraph | GroupClusterMap)+
  plot_layout(guides="collect") &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "center",
        legend.justification = "center")

combined_plot



#############################################################################################################
####################### PISA 2018 validation with posterior #################################################
#############################################################################################################

library(psychometric)
library(dplyr)

PISA2018_3IndClus_2GClass<-read.csv(file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/WGMix-SEM/Empirical Illustration/PISA2018StructuralModel2/belong_3IndClus_2GClass_Posterior.dat",
                                    header = T,
                                    sep = ",",
                                    na.strings = c("NA","."," "),
                                    check.names = F)

##select the latent variable scores after correcting for measurement errors + individual-cluster membership:
PISA2018_3IndClus_2GClass_val<-PISA2018_3IndClus_2GClass[,c(16,18,20,22,24,29)]

#excluding cluster 3
PISA2018_3IndClus_2GClass_noClus3<-PISA2018_3IndClus_2GClass_val %>% 
  filter(`Class#`!=3)
#
#regression including clus3
reg_withClus3<-lm(SenseOfBelong~TeacherSupport+Competitiveness+Cooperativeness+BullyExposure,
                  data = PISA2018_3IndClus_2GClass_val)
#regression excluding clus3
reg_noClus3<-lm(SenseOfBelong~TeacherSupport+Competitiveness+Cooperativeness+BullyExposure,
                data = PISA2018_3IndClus_2GClass_noClus3)
#
#compare the two regressions
summary(reg_withClus3)
summary(reg_noClus3)
#
#the regression coefficients are very similar --> clus3 is capturing noise

#
#Cluster 1:
PISA2018_3IndClus_2GClass_clus1<-PISA2018_3IndClus_2GClass_val %>% filter(`Class#`==1)
#Cluster 2:
PISA2018_3IndClus_2GClass_clus2<-PISA2018_3IndClus_2GClass_val %>% filter(`Class#`==2)
#Cluster 3:
PISA2018_3IndClus_2GClass_clus3<-PISA2018_3IndClus_2GClass_val %>% filter(`Class#`==3)

##Cluster 1: bivariate correlation between sense of belonging and other variables:
r_TeacherSupport_clus1<-cor.test(PISA2018_3IndClus_2GClass_clus1$SenseOfBelong, 
                                 PISA2018_3IndClus_2GClass_clus1$TeacherSupport)
r_TeacherSupport_clus1
r_Competitiveness_clus1<-cor.test(PISA2018_3IndClus_2GClass_clus1$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus1$Competitiveness)
r_Competitiveness_clus1
r_Cooperativeness_clus1<-cor.test(PISA2018_3IndClus_2GClass_clus1$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus1$Cooperativeness)
r_Cooperativeness_clus1
r_bully_clus1<-cor.test(PISA2018_3IndClus_2GClass_clus1$SenseOfBelong,
                        PISA2018_3IndClus_2GClass_clus1$BullyExposure)
r_bully_clus1

##Cluster 2: bivariate correlation between sense of belonging and other variables:
r_TeacherSupport_clus2<-cor.test(PISA2018_3IndClus_2GClass_clus2$SenseOfBelong, 
                                 PISA2018_3IndClus_2GClass_clus2$TeacherSupport)
r_TeacherSupport_clus2
r_Competitiveness_clus2<-cor.test(PISA2018_3IndClus_2GClass_clus2$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus2$Competitiveness)
r_Competitiveness_clus2
r_Cooperativeness_clus2<-cor.test(PISA2018_3IndClus_2GClass_clus2$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus2$Cooperativeness)
r_Cooperativeness_clus2
r_bully_clus2<-cor.test(PISA2018_3IndClus_2GClass_clus2$SenseOfBelong,
                        PISA2018_3IndClus_2GClass_clus2$BullyExposure)
r_bully_clus2

##Cluster 3: bivariate correlation between sense of belonging and other variables:
r_TeacherSupport_clus3<-cor.test(PISA2018_3IndClus_2GClass_clus3$SenseOfBelong, 
                                 PISA2018_3IndClus_2GClass_clus3$TeacherSupport)
r_TeacherSupport_clus3
r_Competitiveness_clus3<-cor.test(PISA2018_3IndClus_2GClass_clus3$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus3$Competitiveness)
r_Competitiveness_clus3
r_Cooperativeness_clus3<-cor.test(PISA2018_3IndClus_2GClass_clus3$SenseOfBelong, 
                                  PISA2018_3IndClus_2GClass_clus3$Cooperativeness)
r_Cooperativeness_clus3
r_bully_clus3<-cor.test(PISA2018_3IndClus_2GClass_clus3$SenseOfBelong,
                        PISA2018_3IndClus_2GClass_clus3$BullyExposure)
r_bully_clus3


##use one country as comparison AT:
AT<-PISA2018_3IndClus_2GClass[,-2] %>% filter(CNT=="AUT") %>%
  dplyr::select(SenseOfBelong.1, TeacherSupport.1, Competitiveness.1, Cooperativeness.1, BullyExposure.1)
AT_lm<-lm(SenseOfBelong.1~TeacherSupport.1+Competitiveness.1+Cooperativeness.1+BullyExposure.1, 
          data = AT)
summary(AT_lm)
