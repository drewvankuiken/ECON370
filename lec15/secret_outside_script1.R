library(data.table)
data_path = "/Users/drewvankuiken/Dropbox/ECON370/local/lec15/"
NHIS_data = fread(paste0(data_path,"adult19.csv"))
NHIS_data = NHIS_data[SEX_A<7]    #remove responses not male or female
setnames(NHIS_data,c("HEIGHTTC_A","WEIGHTLBTC_A"),c("height","weight"))
NHIS_data = NHIS_data[height<96]  #remove non-numerical responses
NHIS_data = NHIS_data[weight<900] #remove non-numerical responses
NHIS_data[,sex:="Male"]; NHIS_data[SEX_A==2,sex:="Female"] # create sex var
NHIS_plot_data = copy(NHIS_data); NHIS_plot_data[,sex:="All"]
NHIS_plot_data = rbind(NHIS_plot_data[,.(sex,height,weight)],
                       NHIS_data[,.(sex,height,weight)])
his   = NHIS_data[,height]           # store heights

head(his)

# a million issues getting this to run, seems like it works half the time
library(mclust)
GMM         = Mclust(his,G=2,control=emControl(tol=c(1e-15,1e-15)))
GMM_mu      = GMM$parameters$mean
GMM_sig2    = GMM$parameters$variance$sigmasq
GMM_probs   = GMM$parameters$pro
mclust_moms = data.table(sex  = c("Female","Male"),
                         mu   = GMM_mu,
                         sig2 = GMM_sig2,
                         prob = GMM_probs)
mclust_moms = mclust_moms[order(sex,decreasing = T)]
