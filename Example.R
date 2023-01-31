#load packages required
l<-c("netmeta","readxl","mvtnorm")
lapply(l, require, character.only = TRUE)

#change working directory accordingly
#load the functions required
source("Pscores_function.R")
source("league_table.R")
source("league_table_var.R")
source("intersect2.R")
source("Prepare_Multi.R")
source("Prepare_Single.R")
source("Prepare_function.R")
source("pscrs.R")
source("pscore_graph.R")

#example dataset
data <- read_excel("Schizophrenia dataset.xls")

#Efficacy Outcome
#Use netmeta package to perform the NMA
pairwise.data <-pairwise (treat=list(TreatA, TreatB),mean=list(PA,PB),sd=list(sdA,sdB),n=list(Na,Nb),
                          data = data, studlab = data$`study ID`, sm = "SMD")
net_schiz<-netmeta(TE,seTE,treat1,treat2,studlab,sm="SMD",data=pairwise.data,tol.multiarm = 0.1,ref="PBO")

#x is the list of outcomes; even when a single outcome is explored it should be included in a list.
#type is the type of outcome; it can be either "B" for Beneficial, or "H" for Harmful
#correlation is set by default to NULL or 0 when a single outcome is considered
schiz<-list(net_schiz)
p_scores(x=schiz,CIV=0,correlation=NULL,type="H")

#pscores graph- exploring the P-scores for a range of values considered for CIV
#beta_ is the range of CIV explored-it should be a vector
pscore_graph_t(x=schiz,beta_=seq(0,1,0.01),type="H")

#Weight Outcome
pair_weight<-pairwise (treat=list(TreatA, TreatB),mean=list(meanweightA,meanweightB),sd=list(SDweightA,SDweightB),n=list(nweightA,nweightB),
                      data = data, studlab = data$`study ID`, sm = "SMD")
net_weight<-netmeta(TE,seTE,treat1,treat2,studlab,sm="SMD",data=pair_weight,tol.multiarm = 0.5,tol.multiarm.se=0.5,ref="PBO")
weight<-list(net_weight)
p_scores(weight,0,NULL,"H")
pscore_graph_t(weight,seq(0,1,0.01),"H")

#Drop out all cause (Acceptability)
pair_drp<-pairwise (treat=list(TreatA, TreatB),event=list(DOtotalAP1,DOtotalAP2),n=list(NAP1r,NAP2r),
                    data = data, studlab = data$`study ID`, sm = "OR")
#transform OR to SMD
pair_drp_sel<-data.frame((pair_drp$TE*sqrt(3))/pi,((pair_drp$TE-1.96*pair_drp$seTE)*sqrt(3))/pi,((pair_drp$TE+1.96*pair_drp$seTE)*sqrt(3))/pi,pair_drp$studlab,pair_drp$treat1,pair_drp$treat2)
names(pair_drp_sel)<-c("TE","CIL","CIU","studlab","treat1","treat2")
pair_drp_sel$seTE<-(pair_drp_sel$CIU-pair_drp_sel$CIL)/3.92
net_drp<-netmeta(TE,seTE,treat1,treat2,studlab,sm="SMD",data=pair_drp_sel,tol.multiarm = 0.5,tol.multiarm.se=0.5,ref="PBO")

acceptability<-list(net_drp)
p_scores(acceptability,0,NULL,"H")
pscore_graph_t(acceptability,seq(0,1,0.01),"H")

#Exploring two outcomes: efficacy and acceptability assuming their correlation=0
t<-list(net_schiz,net_drp)
p_scores(x=t,CIV=c(0,0),correlation=NULL,type=c("H","H"))

#Exploring three outcomes: Efficacy, Weight and acceptability assuming they are independent
z<-list(net_schiz,net_weight,net_drp)
p_scores(x=z,c(0,0,0),correlation =NULL,type=c("H","H","H"))

#Exploring three outcomes: Efficacy, Weight and acceptability assuming dependencies described in matrix cn and CIV 0.5 for weight outcome
cn<-matrix(c(1,-0.5,0.2,-0.5,1,0,0.2,0,1),3,3)
p_scores(x=z,CIV=c(0,0.5,0),correlation=cn,type=c("H","H","H"))

#Exploring two outcomes: efficacy and weight assuming their independence
ew<-list(net_schiz,net_weight)
p_scores(ew,c(0,0),NULL,c("H","H"))
