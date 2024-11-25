# Fit model 100 times and take the result with lowest BIC
# EVV is a model about the covariance matrix:
# Basically, we are only assuming the "volume" of M/F vcov mat are the same
# The same volume means the determinants of the two matrices are the same
bic = Inf 
for(i in 1:100){ 
  temp=Mclust(NHIS_data[,.(height,weight)],G=2,modelNames = "EVV",
              control=emControl(tol=c(1e-15,1e-15),itmax=c(1000,1000)))
  if(temp$bic<bic) GMM = temp; bic = GMM$bic
}
## --- Store estimated parameters
mus_est    = GMM$parameters$mean
vcovs_est  = list(GMM$parameters$variance$sigma[,,1],
                  GMM$parameters$variance$sigma[,,2])
alphas_est = GMM$parameters$pro
Mid        = which.max(mus_est["height",])
ids        = c(Mid,setdiff(1:2,Mid))

## --- Give Male/Female labels to objects
colnames(mus_est)[ids] = c("Male","Female")
names(vcovs_est)[ids]  = c("Male","Female")
names(alphas_est)[ids] = c("Male","Female")
