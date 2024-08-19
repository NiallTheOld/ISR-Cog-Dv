## RICLPM Cognitive Abilities and Regulation Capacities
library("psych")
library("ggplot2")
library("lavaan")
library("tidyverse")
library("plyr")
library("dplyr")
library("haven")



##Cog and derived dataset
HGrid_S2 <- read.table("mcs2_hhgrid.tab", header = TRUE, sep ="\t")
DerData_parent_S2 <- read.table("mcs2_parent_derived.tab", header = TRUE, sep ="\t")
FamDer_S3 <- read.table("mcs3_family_derived.tab", header = TRUE, sep ="\t")
IntViewData_cm_parent_S2 <- read.table("mcs2_parent_cm_interview.tab", header = TRUE, sep ="\t")
IntViewData_cm_parent_S3 <- read.table("mcs3_parent_cm_interview.tab", header = TRUE, sep ="\t")
IntViewData_cm_parent_S4 <- read.table("mcs4_parent_cm_interview.tab", header = TRUE, sep ="\t")
DerData_cm_S2 <- read.table("mcs2_cm_derived.tab", header =  TRUE, sep = "\t")
DerData_cm_S3 <- read.table("mcs3_cm_derived.tab", header =  TRUE, sep = "\t")
DerData_cm_S4 <- read.table("mcs4_cm_derived.tab", header =  TRUE, sep = "\t")
ChildCog_S2 <- read.table("mcs2_cm_cognitive_assessment.tab", header = TRUE, sep ="\t")
ChildCog_S3 <- read.table("mcs3_cm_cognitive_assessment.tab", header = TRUE, sep ="\t")
ChildCog_S4 <- read.table("mcs4_cm_cognitive_assessment.tab", header = TRUE, sep ="\t")
SelfRegIt_S2 <- read_dta("SELREG AGE 3.dta")
SelfRegIt_S3 <- read_dta("SELREG AGE 5.dta")
SelfRegIt_S4 <- read_dta("SELREG AGE 7.dta")



## Single identifier
HGrid_S2$CMID<- paste(HGrid_S2$MCSID, HGrid_S2$BCNUM00, sep = '_C')
FamDer_S3$CMID<- paste(FamDer_S3$MCSID, FamDer_S3$BCNUM00, sep = '_C')
DerData_cm_S2$CMID<- paste(DerData_cm_S2$MCSID, DerData_cm_S2$BCNUM00, sep = '_C')
DerData_cm_S3$CMID<- paste(DerData_cm_S3$MCSID, DerData_cm_S3$CCNUM00, sep = '_C')
DerData_cm_S4$CMID<- paste(DerData_cm_S4$MCSID, DerData_cm_S4$DCNUM00, sep = '_C')
IntViewData_cm_parent_S2$CMID<- paste(IntViewData_cm_parent_S2$MCSID, IntViewData_cm_parent_S2$BCNUM00, sep = '_C')
IntViewData_cm_parent_S3$CMID<- paste(IntViewData_cm_parent_S3$MCSID, IntViewData_cm_parent_S3$CCNUM00, sep = '_C')
IntViewData_cm_parent_S4$CMID<- paste(IntViewData_cm_parent_S4$MCSID, IntViewData_cm_parent_S4$DCNUM00, sep = '_C')
DerData_parent_S2$PNID<- paste(DerData_parent_S2$MCSID, DerData_parent_S2$BPNUM00, sep = '_P')
ChildCog_S2$CMID<- paste(ChildCog_S2$MCSID, ChildCog_S2$BCNUM00, sep = '_C')
ChildCog_S3$CMID<- paste(ChildCog_S3$MCSID, ChildCog_S3$CCNUM00, sep = '_C')
ChildCog_S4$CMID<- paste(ChildCog_S4$MCSID, ChildCog_S4$DCNUM00, sep = '_C')
SelfRegIt_S2$CMID<- paste(SelfRegIt_S2$MCSID, SelfRegIt_S2$CNUM, sep = '_C')
SelfRegIt_S3$CMID<- paste(SelfRegIt_S3$MCSID, SelfRegIt_S3$CNUM, sep = '_C')
SelfRegIt_S4$CMID<- paste(SelfRegIt_S4$MCSID, SelfRegIt_S4$CNUM, sep = '_C')


summary(ChildCog_S2$Name.VocabS2)

##Variable rename
#colnames(ChildCog_S2)[388] <- "Brack.StanS2"
#colnames(ChildCog_S2)[551] <- "Name.VocabS2"
#colnames(ChildCog_S3)[105] <- "Pic.SimS3"
#colnames(ChildCog_S3)[201] <- "Name.VocS3"
#colnames(ChildCog_S3)[330] <- "Pat.ConS3"
#colnames(ChildCog_S4)[411] <- "Pat.ConS4"
#colnames(ChildCog_S4)[372] <- "Word.Read.StanS4"
#colnames(ChildCog_S4)[443] <- "MathAbS4"
colnames(DerData_cm_S2)[11] <- "EmoDysS2"
colnames(DerData_cm_S2)[10] <- "SelfRegS2"
colnames(DerData_cm_S3)[8] <- "EmoDysS3"
colnames(DerData_cm_S3)[7] <- "SelfRegS3"
colnames(DerData_cm_S4)[15] <- "EmoDysS4"
colnames(DerData_cm_S4)[14] <- "SelfRegS4"
colnames(HGrid_S2)[8] <- "Male1"
colnames(FamDer_S3)[29] <- "PovMed"

# Testing to use all cog variables that Ioannis used
CMS2recode <- DerData_cm_S2 %>% 
  select("MCSID", "CMID","SelfRegS2","EmoDysS2","BDC06E00") %>% 
  mutate(EmoDys_narmS2 = ifelse(EmoDysS2 == -1,NA, EmoDysS2)) %>% 
  mutate(SelfReg_narmS2 = ifelse(SelfRegS2 == -1,NA,SelfRegS2)) %>% 
  mutate(Ethnic_narm = ifelse(BDC06E00 %in% c(-9, -1, -8), NA, BDC06E00)) %>% 
  mutate(Ethnic_Bi = ifelse(Ethnic_narm %in% c(2, 3, 4, 5, 6), 2, Ethnic_narm))
CMS3recode <- DerData_cm_S3 %>% 
  select("CMID","SelfRegS3","EmoDysS3") %>% 
  mutate(EmoDys_narmS3 = ifelse(EmoDysS3 == -1,NA, EmoDysS3)) %>% 
  mutate(SelfReg_narmS3 = ifelse(SelfRegS3 == -1,NA,SelfRegS3))
CMS4recode <- DerData_cm_S4 %>% 
  select("CMID","SelfRegS4","EmoDysS4") %>% 
  mutate(EmoDys_narmS4 = ifelse(EmoDysS4 == -1,NA, EmoDysS4)) %>% 
  mutate(SelfReg_narmS4 = ifelse(SelfRegS4 == -1,NA,SelfRegS4))
CogRecodeS2 <- ChildCog_S2 %>%
  select("CMID","BDBAST00", "BDSRCS00") %>% 
  mutate(BDSRCS00 = ifelse(BDSRCS00 %in% c(-1, -2, -3, -4, -7, -8, -6), NA, BDSRCS00)) %>% 
  mutate(BDBAST00 = ifelse(BDBAST00 %in%c (-8, -7, -6), NA, BDBAST00)) %>% 
  mutate_at(vars("BDBAST00", "BDSRCS00"), scale, center = TRUE, scale = TRUE)
CogRecodeS3 <- ChildCog_S3 %>%
  select("CMID", "CCNVTSCORE", "CCPCTSCORE", "CCPSABIL") %>% 
  mutate_at(vars("CCNVTSCORE", "CCPCTSCORE", "CCPSABIL"), scale, center = TRUE, scale = TRUE) 
CogRecodeS4 <- ChildCog_S4 %>%
  select("CMID","DCWRSD00", "DCMATHS7SA", "DCPCTS00") %>% 
  mutate(DCPCTS00 = ifelse(DCPCTS00 %in% c(-1), NA, DCPCTS00)) %>% 
  mutate(DCWRSD00 = ifelse(DCWRSD00 %in% c(-1), NA, DCWRSD00)) %>% 
  mutate(DCMATHS7SA = ifelse(DCMATHS7SA %in% c(-1), NA, DCMATHS7SA)) %>% 
  mutate_at(vars("DCWRSD00", "DCMATHS7SA", "DCPCTS00"), scale, center = TRUE, scale = TRUE)
ParEdVar <- DerData_parent_S2 %>% 
  select("MCSID","BDACAQ00", "BRESP00", "PNID") %>% 
  mutate(ParEd = ifelse(BDACAQ00 %in% c (95, 96), 0, BDACAQ00)) %>% 
  mutate(ParEd = ifelse(ParEd %in% c (1,2), 1, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd == 3, 2, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd %in% c (4,5), 3, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd %in% (-1), NA, ParEd))
GenVar <- HGrid_S2 %>%
  select("CMID", "Male1")  %>% 
  mutate(Male1 = ifelse(Male1 == -1,NA, Male1)) %>% 
  mutate(Male1 = ifelse(Male1 == 1,0, Male1)) %>% 
  mutate(Male1 = ifelse(Male1 == 2,1, Male1))
SelfRegIt_S2Rec <- SelfRegIt_S2  %>% 
  select("CMID", "A3ISR1", "A3ISR2", "A3ISR3", "A3ISR4", "A3ISR5", "A3ESR1", "A3ESR2", "A3ESR3", "A3ESR4", "A3ESR5") 
SelfRegIt_S3Rec <- SelfRegIt_S3  %>% 
  select("CMID", "A5ISR1", "A5ISR2", "A5ISR3","A5ISR4", "A5ISR5", "A5ESR1", "A5ESR2", "A5ESR3", "A5ESR4", "A5ESR5") 
SelfRegIt_S4Rec <- SelfRegIt_S4  %>% 
  select("CMID", "A7ISR1", "A7ISR2", "A7ISR3", "A7ISR4", "A7ISR5","A7ESR1", "A7ESR2", "A7ESR3", "A7ESR4", "A7ESR5") 
#RiskPovVar <- FamDer_S3 %>% 

## Remove missing and nas
CMS2recode <- DerData_cm_S2 %>% 
  select("MCSID", "CMID","SelfRegS2","EmoDysS2","BDC06E00") %>% 
  mutate(EmoDys_narmS2 = ifelse(EmoDysS2 == -1,NA, EmoDysS2)) %>% 
  mutate(SelfReg_narmS2 = ifelse(SelfRegS2 == -1,NA,SelfRegS2)) %>% 
  mutate(Ethnic_narm = ifelse(BDC06E00 %in% c(-9, -1, -8), NA, BDC06E00)) %>% 
  mutate(Ethnic_Bi = ifelse(Ethnic_narm %in% c(2, 3, 4, 5, 6), 2, Ethnic_narm))
CMS3recode <- DerData_cm_S3 %>% 
  select("CMID","SelfRegS3","EmoDysS3") %>% 
  mutate(EmoDys_narmS3 = ifelse(EmoDysS3 == -1,NA, EmoDysS3)) %>% 
  mutate(SelfReg_narmS3 = ifelse(SelfRegS3 == -1,NA,SelfRegS3))
CMS4recode <- DerData_cm_S4 %>% 
  select("CMID","SelfRegS4","EmoDysS4") %>% 
  mutate(EmoDys_narmS4 = ifelse(EmoDysS4 == -1,NA, EmoDysS4)) %>% 
  mutate(SelfReg_narmS4 = ifelse(SelfRegS4 == -1,NA,SelfRegS4))
CogRecodeS2 <- ChildCog_S2 %>%
  select("CMID","Brack.StanS2", "Name.VocabS2") %>% 
  mutate(Brack.StanS2 = ifelse(Brack.StanS2 %in% c(-1, -2, -3, -4, -7, -8, -6), NA, Brack.StanS2)) %>% 
  mutate(Name.VocabS2 = ifelse(Name.VocabS2 %in%c (-8, -7, -6), NA, Name.VocabS2)) %>% 
  mutate_at(vars("Brack.StanS2", "Name.VocabS2"), scale, center = TRUE, scale = TRUE)
CogRecodeS3 <- ChildCog_S3 %>%
  select("CMID", "Pic.SimS3", "Name.VocS3", "Pat.ConS3") %>% 
  mutate_at(vars("Pic.SimS3", "Name.VocS3", "Pat.ConS3"), scale, center = TRUE, scale = TRUE) 
CogRecodeS4 <- ChildCog_S4 %>%
  select("CMID","Pat.ConS4", "Word.Read.StanS4", "MathAbS4") %>% 
  mutate(Pat.ConS4 = ifelse(Pat.ConS4 %in% c(-1), NA, Pat.ConS4)) %>% 
  mutate(Word.Read.StanS4 = ifelse(Word.Read.StanS4 %in% c(-1), NA, Word.Read.StanS4)) %>% 
  mutate(MathAbS4 = ifelse(MathAbS4 %in% c(-1), NA, MathAbS4)) %>% 
  mutate_at(vars("Pat.ConS4", "Word.Read.StanS4", "MathAbS4"), scale, center = TRUE, scale = TRUE)
ParEdVar <- DerData_parent_S2 %>% 
  select("MCSID","BDACAQ00", "BRESP00") %>% 
  mutate(ParEd = ifelse(BDACAQ00 %in% c (95, 96), 0, BDACAQ00)) %>% 
  mutate(ParEd = ifelse(ParEd %in% c (1,2), 1, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd == 3, 2, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd %in% c (4,5), 3, ParEd)) %>% 
  mutate(ParEd = ifelse(ParEd %in% (-1), NA, ParEd))
GenVar <- HGrid_S2 %>%
  select("CMID", "Male1")  %>% 
  mutate(Male1 = ifelse(Male1 == -1,NA, Male1))
SelfRegIt_S2Rec <- SelfRegIt_S2  %>% 
  select("CMID", "A3ISR1", "A3ISR2", "A3ISR3", "A3ISR4", "A3ISR5", "A3ESR1", "A3ESR2", "A3ESR3", "A3ESR4", "A3ESR5") 
SelfRegIt_S3Rec <- SelfRegIt_S3  %>% 
  select("CMID", "A5ISR1", "A5ISR2", "A5ISR3","A5ISR4", "A5ISR5", "A5ESR1", "A5ESR2", "A5ESR3", "A5ESR4", "A5ESR5") 
SelfRegIt_S4Rec <- SelfRegIt_S4  %>% 
  select("CMID", "A7ISR1", "A7ISR2", "A7ISR3", "A7ISR4", "A7ISR5","A7ESR1", "A7ESR2", "A7ESR3", "A7ESR4", "A7ESR5") 
RiskPovVar <- FamDer_S3 %>% 
  select("CMID", "PovMed") %>% 
  mutate(PovMed_narm = ifelse(PovMed == -1,NA, PovMed))
RiskPovVar$CMID<- paste(RiskPovVar$MCSID, RiskPovVar$CPNUM00, sep = '_P')



## merging within sweeps
MergeWithinS2 <- merge(merge(CogRecodeS2, CMS2recode, by = "CMID"), GenVar, by = "CMID")
MergeWithinS3 <- merge(CogRecodeS3, CMS3recode, by = c("CMID"))
MergeWithinS4 <- merge(CogRecodeS4, CMS4recode, by = c("CMID"))

## merge between sweeps
RegItems <- merge(merge(SelfRegIt_S2Rec, SelfRegIt_S3Rec, by = "CMID"), SelfRegIt_S4Rec, by = "CMID") 
ECR234NI<- merge(merge(MergeWithinS2, MergeWithinS3, by = "CMID"), MergeWithinS4, by = "CMID")
ECR234<- merge(ECR234NI, RegItems, by =c("CMID"))
ECR234<- merge(ParEdVar, RegItems, by =c("MCSID"))

table(ECR234$Male1)

# Ioannis code

BSRCOG<-'
#FACTOR MODEL OF BEHAVIOURAL SELF-REGULATION
SI1 =~ A3ISR1 + A1*A3ISR2 + B1*A3ISR3 + C1*A3ISR4 + D1*A3ISR5
SI2 =~ A5ISR1 + A1*A5ISR2 + B1*A5ISR3 + C1*A5ISR4 + D1*A5ISR5
SI3 =~ A7ISR1 + A1*A7ISR2 + B1*A7ISR3 + C1*A7ISR4 + D1*A7ISR5

A3ISR1 ~~ A5ISR1 + A7ISR1
A5ISR1 ~~ A7ISR1
A3ISR2 ~~ A5ISR2 + A7ISR2
A5ISR2 ~~ A7ISR2
A3ISR3 ~~ A5ISR3 + A7ISR3
A5ISR3 ~~ A7ISR3
A3ISR4 ~~ A5ISR4 + A7ISR4
A5ISR4 ~~ A7ISR4
A3ISR5 ~~ A5ISR5 + A7ISR5
A5ISR5 ~~ A7ISR5

A3ISR1 | .con11*t1 + .con12*t2
A5ISR1 | .con11*t1 + .con12*t2
A7ISR1 | .con11*t1 + .con12*t2
A3ISR2 | .con13*t1 + .con14*t2
A5ISR2 | .con13*t1 + .con14*t2
A7ISR2 | .con13*t1 + .con14*t2
A3ISR3 | .con15*t1 + .con16*t2
A5ISR3 | .con15*t1 + .con16*t2
A7ISR3 | .con15*t1 + .con16*t2
A3ISR4 | .con17*t1 + .con18*t2
A5ISR4 | .con17*t1 + .con18*t2
A7ISR4 | .con17*t1 + .con18*t2
A3ISR5 | .con19*t1 + .con20*t2
A5ISR5 | .con19*t1 + .con20*t2
A7ISR5 | .con19*t1 + .con20*t2

#FACTOR MODEL OF IQ
BWAVE =~ BDBAST00 + BDSRCS00
CWAVE =~ CCNVTSCORE + CCPCTSCORE + CCPSABIL
DWAVE =~ DCWRSD00 + DCMATHS7SA + DCPCTS00

SI1 ~ 0*1
SI2 ~ 1
SI3 ~ 1

SI1 ~~ 0*SI1
SI2 ~~ 0*SI2
SI3 ~~ 0*SI3

BWAVE ~~ 0*BWAVE
CWAVE ~~ 0*CWAVE
DWAVE ~~ 0*DWAVE

TCOG =~ 1*BWAVE + 1*CWAVE + 1*DWAVE
TSI =~ 1*SI1 + 1*SI2 + 1*SI3
TSI ~~ TCOG
TSI~~TSI
TCOG~~TCOG

OCOG1 =~ 1*BWAVE
OCOG2 =~ 1*CWAVE
OCOG3 =~ 1*DWAVE
OSI1 =~ 1*SI1
OSI2 =~ 1*SI2
OSI3 =~ 1*SI3

OCOG1~~OCOG1
OCOG2~~OCOG2
OCOG3~~OCOG3
OSI1~~OSI1
OSI2~~OSI2
OSI3~~OSI3

#AUTOREGRESSIVE
OSI2 ~ OSI1
OSI3 ~ OSI2
OCOG2 ~ OCOG1
OCOG3 ~ OCOG2

OSI1 ~~ OCOG1
OSI2 ~~ OCOG2
OSI3 ~~ OCOG3
#CROSS-LAGGED REGRESSIONS
OSI2 ~ OCOG1
OSI3 ~ OCOG2
OCOG2 ~ OSI1
OCOG3 ~ OSI2
DCWRSD00 ~~ CCNVTSCORE
'

fit<-lavaan(model=BSRCOG,data = ECR234, ordered = c('A3ISR1', 'A5ISR1', 'A7ISR1',
                                                           'A3ISR2', 'A5ISR2', 'A7ISR2', 'A3ISR3','A5ISR3', 'A7ISR3', 'A3ISR4','A5ISR4', 'A7ISR4', 'A3ISR5', 'A5ISR5', 'A7ISR5'), estimator='WLSMV', missing='pairwise', orthogonal=T, mimic='Mplus')
summary(fit, fit.measures = T, standardized = T)

hist(ECR234$BDBAST00)

BSRCOG<-'
#FACTOR MODEL OF BEHAVIOURAL SELF-REGULATION
SI1 =~ A3ISR1 + A1*A3ISR2 + B1*A3ISR3 + C1*A3ISR4 + D1*A3ISR5
SI2 =~ A5ISR1 + A1*A5ISR2 + B1*A5ISR3 + C1*A5ISR4 + D1*A5ISR5
SI3 =~ A7ISR1 + A1*A7ISR2 + B1*A7ISR3 + C1*A7ISR4 + D1*A7ISR5

A3ISR1 ~~ A5ISR1 + A7ISR1
A5ISR1 ~~ A7ISR1
A3ISR2 ~~ A5ISR2 + A7ISR2
A5ISR2 ~~ A7ISR2
A3ISR3 ~~ A5ISR3 + A7ISR3
A5ISR3 ~~ A7ISR3
A3ISR4 ~~ A5ISR4 + A7ISR4
A5ISR4 ~~ A7ISR4
A3ISR5 ~~ A5ISR5 + A7ISR5
A5ISR5 ~~ A7ISR5

A3ISR1 | .con11*t1 + .con12*t2
A5ISR1 | .con11*t1 + .con12*t2
A7ISR1 | .con11*t1 + .con12*t2
A3ISR2 | .con13*t1 + .con14*t2
A5ISR2 | .con13*t1 + .con14*t2
A7ISR2 | .con13*t1 + .con14*t2
A3ISR3 | .con15*t1 + .con16*t2
A5ISR3 | .con15*t1 + .con16*t2
A7ISR3 | .con15*t1 + .con16*t2
A3ISR4 | .con17*t1 + .con18*t2
A5ISR4 | .con17*t1 + .con18*t2
A7ISR4 | .con17*t1 + .con18*t2
A3ISR5 | .con19*t1 + .con20*t2
A5ISR5 | .con19*t1 + .con20*t2
A7ISR5 | .con19*t1 + .con20*t2

#FACTOR MODEL OF IQ
BWAVE =~ BDBAST00 + BDSRCS00
CWAVE =~ CCNVTSCORE + CCPCTSCORE + CCPSABIL
DWAVE =~ DCWRSD00 + DCMATHS7SA + DCPCTS00

SI1 ~ 0*1
SI2 ~ 1
SI3 ~ 1

SI1 ~~ 0*SI1
SI2 ~~ 0*SI2
SI3 ~~ 0*SI3

BWAVE ~~ 0*BWAVE
CWAVE ~~ 0*CWAVE
DWAVE ~~ 0*DWAVE

TCOG =~ 1*BWAVE + 1*CWAVE + 1*DWAVE
TSI =~ 1*SI1 + 1*SI2 + 1*SI3
TSI ~~ TCOG
TSI~~TSI
TCOG~~TCOG

OCOG1 =~ 1*BWAVE
OCOG2 =~ 1*CWAVE
OCOG3 =~ 1*DWAVE
OSI1 =~ 1*SI1
OSI2 =~ 1*SI2
OSI3 =~ 1*SI3

OCOG1~~OCOG1
OCOG2~~OCOG2
OCOG3~~OCOG3
OSI1~~OSI1
OSI2~~OSI2
OSI3~~OSI3

#AUTOREGRESSIVE
OSI2 ~ OSI1
OSI3 ~ OSI2
OCOG2 ~ OCOG1
OCOG3 ~ OCOG2

OSI1 ~~ OCOG1
OSI2 ~~ OCOG2
OSI3 ~~ OCOG3
#CROSS-LAGGED REGRESSIONS
OSI2 ~ OCOG1
OSI3 ~ OCOG2
OCOG2 ~ OSI1
OCOG3 ~ OSI2
DCPCTS00 ~~ CCPCTSCORE'

fit<-cfa(model=BSRCOG,data = ECR234, group = "Male1", ordered = c('A3ISR1', 'A5ISR1', 'A7ISR1',
                                                           'A3ISR2', 'A5ISR2', 'A7ISR2', 'A3ISR3','A5ISR3', 'A7ISR3', 'A3ISR4','A5ISR4', 'A7ISR4', 'A3ISR5', 'A5ISR5', 'A7ISR5'), estimator='WLSMV', missing='pairwise', orthogonal=T, mimic='Mplus')
summary(fit, fit.measures = T, standardized = T)
