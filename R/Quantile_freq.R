
#' Quantile_freq
#'
#' @description function enables user to see whether the frequency of daily averages under a quantile based threshold is increasing or decreasing.
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station. Must equal one entry of the data list.
#' @param quantile numeric; If Input is 0.1; 90% of all Values are bigger than this Value.
#' @param graphic default=T; if T, returns Histogram and Densityplot. if F; returns vector containing trends, calculated with zyp/stats.
#'
#'@import stats
#'@import zyp
#'
#' @return Graph/ list
#' \describe{
#'   \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
#'   \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
#'   \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
#'   \item{intercept_ls}{intercept created by \link[stats]{lm}}
#'   \item{slope_ls}{slope created by \link[stats]{lm}}
#' }
#' @export
#'
#' @examples
#' \dontrun{ Quantile_freq(data, "COCHEM", 0.1)}
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

