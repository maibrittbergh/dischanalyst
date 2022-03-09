#' Trend of Values under quantile based Threshold U
#'
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by dischanalyst. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station. Must equal entry in data list.
#' @param quantile numeric; If Input is 0.1; 90% of all Values are bigger than this Value.
#' @param graphic default=T; if T, returns Histogram and Densityplot. if F; returns vector containing trends, calculated with zyp/stats.
#'
#' @return Graph/ list containing: intercept created by \link[zyp]{zyp.trend.vector}, slope created by \link[zyp]{zyp.trend.vector}, significance (Kendall's P-Value) for the final detrended time-series, intercept_ls}{intercept created by \link[stats]{lm}, slope created by \link[stats]{lm}}
#'
#' @examples
#'
#' @import stats
#' @import zyp
#'
#'@examples
#'\dontrun{ Quantile_freq(data, "COCHEM", 0.1)}
#'


Quantile_freq=function(data,station, quantile, graphic=T){

data=data[[station]]
Value=data[,2]
thres=quantile(Value, probs=quantile)
val=which(Value<=thres)
years=as.numeric(substr(data[val, 1], 1,4))
results=data.frame(cbind(val,years))
range=min(years):max(years)



plot=  hist(results$years, prob=T, col="darkcyan", border="white", breaks=range, main = paste("Histogram and Densityplot of Values under", thres ,"(",quantile, "quantile )- At", station),xlab="Years" )
       lines(density(results$years), col = "brown3", lwd = 2)


trendzyp=zyp.trend.vector(plot$density)
trendzyp
l=1:length(plot$density)
trendlm=lm(plot$density~l)
trendlm

res= list(trendzyp[11], trendzyp[2], trendzyp[6], trendlm$coefficients[1] ,trendlm$coefficients[2] )

names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm" )

if (graphic==T){return(print(plot))
}else{return(res)}
}

