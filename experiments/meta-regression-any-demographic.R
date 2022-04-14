library(readxl)
library(metafor)
library(tools)
library(gridExtra)
library(grid)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
input_file = args[1];
demographic = gsub("\\(E\\)","", gsub(" ", "", args[2]));
demo_vs_allele_file = args[3];
p_value_file = args[4];
forset_plot_file = args[5];

pred_length = 100
cex_lab_value = 0.8 # magnification of x and y labels relative to cex
cex_main_value = 0.8 # magnification of titles relative to cex
las_value = 1
pch_value = 19
size_min_value = 0.5
size_ratio_value = 0.5


# read.csv, csv file
# read.delim, txt file                             
# read_excel, xls/xlsx file
FileReadFunc = function(path) {
  if(file_ext(path) == 'txt')
    data = read.delim(path)
  if(file_ext(path) == 'csv')
    data = read.csv(path)
  if(file_ext(path) == 'xls' || file_ext(path) == 'xlsx')
    data = read_excel(path)
  return(data)
}


#Commenting out removemissingfunc and filtermerg func
RemoveMissingFunc = function(data, desiredCols) {
  #Filter out the rows which have NAs in desired Cols
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


FilterMergFunc = function(data, desiredCols) {
  missingVec = is.na(data[, desiredCols])
  return(data[missingVec, ])
}


## Added 12/7/20
##Updated 5/18/21
RemoveNonEuroFunc = function(data){
  #Create a list
  NonEuro = c('GOBS', 'IMH', 'UNICAMP', 'Meth-CT', 'MIRECC', 'Meth-CT', 'MIRECC',
              'UKBB_NonEuropean', 'OSAKA', 'PING_NonEuropean', 'UKBB')
  #Keep the Study in data which are not in NonEuro
  return(data[!is.element(data$Study,NonEuro),])
}

      
DataStats = function(data) {
  # mean,median,25th and 75th quartiles,min,max
  print(summary(data))
  # Tukey min,lower-hinge, median,upper-hinge,max
  print(fivenum(data))
}

# define a function to adjust the digits of the statistics
# digits = function(regression_object){
#   if(abs(regression_object$beta[2])<0.001){
#     round_decimal=5
#   }else{round_decimal=2}
#   return(round_decimal)
# }

print(demographic)

#Data cleaning
data = FileReadFunc(input_file)
print(data[demographic])
      
# Remove missing data in Effect and Age
data = RemoveMissingFunc(data, c('EFFECT', demographic))
# Remove mergeD cohorts based on column N
print(dim(data))
data = FilterMergFunc(data, 'N')
print(dim(data))
# Remove Non European cohorts
data = RemoveNonEuroFunc(data)
#print(dim(data))
# Sort data based on Demographic
data = data[order(data[demographic]),]
print(dim(data))
      
###convert data types and calculate N,SE,Z
#sapply(data,class)
cols=c("EFFECT","SAMPLE_SIZE",demographic,"CI_LB","CI_UB","N","TOTAL_N","PCT")
data[cols]=lapply(data[cols],as.character)
data[cols]=lapply(data[cols],as.numeric)
# Extract features to run meta regression
# convert EFFECT SAMPLE_SIZE Age CI_LB CI_UB N TOTAL_N PCT to numeric
data$N = data$SAMPLE_SIZE
data$SE =(data$CI_UB - data$CI_LB)/(2*1.96)
data$Z = data$EFFECT/data$SE      

#### Generate meta regression plot (age_allele)
pdf(paste0(demo_vs_allele_file))

print(data[demographic])

res = rma.uni(yi=data$EFFECT, sei=data$SE, mods=data[demographic], verbose = TRUE, method="FE", control=list(stepadj=0.4, maxiter= 1000), digits = 5) 
# preds = predict(res, newmods=seq(min(data$Age),max(data$Age),length=pred_length))
size = size_min_value + size_ratio_value * (data$N - min(data$N))/(max(data$N) - min(data$N))
print(res)
print(res$pval)
print(res$beta)

scatter_function = function(demographic_var){
  variable = data[,demographic_var]
  plot(variable, 
       data$EFFECT, 
       pch=pch_value, 
       cex=size, 
       ylab="Unstandardized Effect Size", 
       xlab=demographic,
       cex.lab=cex_lab_value, 
       cex.main=cex_main_value,
       las=las_value, 
       bty="l",
       xlim=c(min(data[demographic]),max(data[demographic])))
  intercept1 = paste("(",round(res$ci.lb[2],2),", ",round(res$ci.ub[2],2),")",sep="")
  intercept2 = paste("(",round(res$ci.lb[1],2),", ",round(res$ci.ub[1],2),")",sep="")
  annotation = c(paste("Beta:", round(res$beta[2],2),intercept1), 
                 paste("Intercept:", round(res$beta[1],2), intercept2), 
                 paste("P-value:", round(res$QMp,2)), 
                 paste("Number of Study:", res$k))
  legend("bottomright", annotation, bty = "n")
}
scatter_function(demographic)

#lines(seq(min(data$Age),max(data$Age),length=pred_length), preds$pred)
#lines(seq(min(data$Age),max(data$Age),length=pred_length), preds$ci.lb, lty="dashed")
#lines(seq(min(data$Age),max(data$Age),length=pred_length), preds$ci.ub, lty="dashed")
      
# convert the result into a dataframe
# result=as.data.frame(preds)
# result$Age = seq(min(data$Age),max(data$Age),length=pred_length)      
      
# scatter_plot = ggplot(data, aes(x=Age, y=EFFECT)) +
#   geom_point(aes(size=size), show.legend = FALSE) +
#   scale_size(range=c(0,3)) + 
#   xlab('Mean Age') + 
#   ylab('Unstandardized Effect Size')+
  #  scale_x_continuous(name="Mean Age", breaks=c(seq(min(data$Age),max(data$Age)+10,10)))+
  # scale_y_continuous(name = "Unstandardized Effect Size", breaks = c(seq(min(data$EFFECT),max(data$EFFECT),30))) +
#   geom_line(data=result, aes(x=Age, y=pred)) +
#   geom_line(data=result, aes(x=Age, y=ci.lb), linetype="dashed") +
#   geom_line(data=result, aes(x=Age, y=ci.ub), linetype="dashed") +
#   theme_classic() +
#   theme(plot.margin=unit(c(0.5,2,0,0.5),'cm'), 
#         axis.title.x = element_text(face='bold'),
#         axis.title.y = element_text(face='bold'))

# calculate the statistics
# round_decimal=2
# beta_ci = paste("(",format(round(res$ci.lb[2],round_decimal),scientific=FALSE),", ",format(round(res$ci.ub[2],round_decimal),scientific=FALSE),")",sep="")
# intercept_ci = paste("(",round(res$ci.lb[1],2),", ",round(res$ci.ub[1],2),")",sep="")
# beta = paste(format(round(res$beta[2],round_decimal),scientific=FALSE),beta_ci)
# intercept = paste(round(res$beta[1],2), intercept_ci)
# p_value = paste(round(res$QMp,2))
# num_study = paste(res$k)

# create the table
# table = matrix(c(beta, p_value, num_study), ncol = 3, byrow = TRUE)
# colnames(table) = c('Beta\n(95%CI)','Beta P Value','Number of Study')
# table = as.table(table)
      
# mytheme <- gridExtra::ttheme_default(
#   core = list(fg_params=list(cex = 0.8)),
#   colhead = list(fg_params=list(cex = 0.8)),
#   rowhead = list(fg_params=list(cex = 0.8)))
# ss <- tableGrob(table, rows=NULL, cols=colnames(table), theme=mytheme)
# grid.draw(ss)
# grid.arrange(scatter_plot, ss, nrow=2, heights=c(5,1))      
      
                
#####forest plot - added 11/18/20
pdf(paste0(forset_plot_file))

ci_min = as.numeric(min(data$CI_LB))
ci_max = as.numeric(max(data$CI_UB))
ci_gap = abs((abs(ci_max) - abs(ci_min))) / 2
x_pos = ci_min - abs(ci_min)*0.3
forest(x = res, 
       ilab = data[demographic],
       ilab.xpos = x_pos, 
       slab=paste(data$Study), 
       cex=.75, 
       addfit=FALSE, 
       header=TRUE)
y_pos = length(data$Study) + 2
print(y_pos)

par(col='black',cex=.75)
text(x_pos, y_pos, demographic)

x_prediction = function(demographic_var){
  var = data[,demographic_var]
  prediction = predict(res,newmods=mean(var))
}
x = x_prediction(demographic)
par(col='firebrick2',col.lab='black')
addpoly(x$pred, sei=x$se, mlab="Discovery", efac = 1,cex=1.2,rows=-0.5,digits=5,col='firebrick2',border='firebrick2')

# Plot for beta and sample size
par(col='black',cex=0.75,font=1)
plot(log(data$N), data$EFFECT, ylim=range(c(data$EFFECT-data$SE, data$EFFECT+data$SE)), 
     pch=pch_value, xlab="log(Cohort Sample Size)", ylab="Effect Size +/- SD")
arrows(log(data$N), data$EFFECT-data$SE, log(data$N), data$EFFECT+data$SE, length=0.05, angle=90, code=3)

# Output p value
print(c('p value: ', res$QMp))
writeLines(as.character(res$QMp), p_value_file)
dev.off()
