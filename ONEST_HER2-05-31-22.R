# library(raters)
library(irr)
# library(ONEST)
library(combinat)
library(openxlsx)
library(ggplot2)
library(reshape2)


#######################################################################################
#######################################################################################
#Backend functions for ONEST technique
#######################################################################################
#######################################################################################

#Get functions
source('/path/to/ONEST_core.R')
source('/path/to/agree_utils.R')

#######################################################################################
#######################################################################################
#Making ONEST plots from pathologist data
#######################################################################################
#######################################################################################

#Load data
setwd('path/to/HER2_discordance')
data = read.xlsx(xlsxFile = "Combined ONEST Scores.xlsx", sheet=1, fillMergedCells = TRUE, colNames = TRUE)

#Prepare columns by combining Observer row with HER2/ER row
obs = colnames(data)[-1]
assay = as.character(data[1,][-1])
newCol = character(length=length(assay))

for (i in 1:length(assay)){
  newCol[i] = paste(assay[i], obs[i], sep='_')
}

colnames(data)[1] = as.character(data[1,1])
colnames(data)[-1] = newCol
data = data[-1,]
#There are only 170 samples
data = data[1:170,]


#Separate data by assay
c(1, grep('HER2', colnames(data)))
her2 = data[, c(1, grep('HER2', colnames(data)))]
rownames(her2) = her2[,1]
her2 = her2[,-1]



#######################################################################################
#######################################################################################
#HER2
#######################################################################################
#######################################################################################

#ONEST plots for 4 category score
results = ONEST_plot(her2, plotI=T, metric = 'OPA')
write.table(results$concord, 'ONEST_concord/all_HER2_concordance-opa.txt', sep='\t', row.names = F, quote = F)
write.table(results$stats, 'ONEST_concord/all_HER2_plotStats-opa.txt', sep='\t', row.names = F, quote = F)

#ONEST plots for 3 category score
her2low = categ_split(her2, type='combined_all', f1 = 0, f2 = c(1,2))
results = ONEST_plot(her2low, plotI=T, metric = 'OPA')
write.table(results$concord, 'ONEST_concord/HER2/HER2low_all_HER2_concordance-opa.txt', sep='\t', row.names = F, quote = F)
write.table(results$stats, 'ONEST_concord/HER2/HER2low_all_HER2_plotStats-opa.txt', sep='\t', row.names = F, quote = F)


#ONEST plots of cases with score of X only (by at least one rater)
# onlyComp = c(0, "1,2", 3)
onlyComp = c(0,1,2,3)
for(i in 1:length(onlyComp)){
  f1 = onlyComp[i]
  cat('Working on',f1,'only...','\n')
  sub = categ_split(her2, type='only', f1 = f1)
  results = ONEST_plot(sub, plotI=T, metric = 'OPA')
  write.table(results$concord, paste0('ONEST_concord/HER2/',f1,'only_HER2_concordance-opa.txt'), sep='\t', row.names = F, quote = F)
  write.table(results$stats, paste0('ONEST_concord/HER2/',f1,'only_HER2_plotStats-opa.txt'), sep='\t', row.names = F, quote = F)
}

#ONEST plots of scores grouped by X vs not X
# score = c(0,1,2,3)
score = c(0,'1,2',3)
score = c(1,2)
for(i in 1:length(score)){
  f1 = score[i]
  cat('Working on',f1,'vs not',f1,'...','\n')
  sub = categ_split(her2low, type='not', f1 = f1)
  results = ONEST_plot(sub, plotI=T, metric = 'OPA')
  write.table(results$concord, paste0('ONEST_concord/HER2/',f1,'vnot',f1,'_HER2_concordance-opa.txt'), sep='\t', row.names = F, quote = F)
  write.table(results$stats, paste0('ONEST_concord/HER2/',f1,'vnot',f1,'_HER2_plotStats-opa.txt'), sep='\t', row.names = F, quote = F)
  
  consist = data.frame(results$modelData$consistency)
  consist$path_number = 2:(nrow(consist)+1)
  diff = data.frame(results$modelData$difference)
  diff$path_number = 2:(nrow(consist))
  mData = merge(consist, diff, by = 'path_number', all = TRUE)
  write.table(mData, paste0('ONEST_concord/HER2/',f1,'vnot',f1,'_HER2_modelData-opa.txt'), sep='\t', row.names = F, quote = F)
}

#ONEST plots for scores >= 2 vs < 2 
sub = her2
sub[her2<2] = 0
sub[her2>=2] = 1
results = ONEST_plot(sub, plotI=T, metric = 'OPA')
write.table(results$concord, paste0('ONEST_concord/HER2/','gt2_HER2_concordance-opa.txt'), sep='\t', row.names = F, quote = F)
write.table(results$stats, paste0('ONEST_concord/HER2/','gt2_HER2_plotStats-opa.txt'), sep='\t', row.names = F, quote = F)

consist = data.frame(results$modelData$consistency)
consist$path_number = 2:(nrow(consist)+1)
diff = data.frame(results$modelData$difference)
diff$path_number = 2:(nrow(consist))
mData = merge(consist, diff, by = 'path_number', all = TRUE)
write.table(mData, paste0('ONEST_concord/HER2/','gt2_HER2_modelData-opa.txt'), sep='\t', row.names = F, quote = F)

#All combinations of two score groupings
# combs = combn(score, 2)
# #Cycle through all combinations to gather dataframes for ONEST concordance and plot stats
# for(i in 1:ncol(combs)){
#   f1 = combs[1,i]
#   f2 = combs[2,i]
#   cat('Working on',f1,'vs',f2,'...','\n')
#   sub = categ_split(her2low, type='versus', f1 = f1, f2 = f2)
#   results = ONEST_plot(sub, plotI=F, metric = 'fkappa')
#   write.table(results$concord, paste0('ONEST_concord/',f1,'v',f2,'_HER2_concordance-fkappa.txt'), sep='\t', row.names = F, quote = F)
#   write.table(results$stats, paste0('ONEST_concord/',f1,'v',f2,'_HER2_plotStats-fkappa.txt'), sep='\t', row.names = F, quote = F)
# }

#######################################################################################
#######################################################################################
#Plotting HER2 ONEST plots
#######################################################################################
#######################################################################################

desiredPlots = c('all_HER2','HER2low_all_HER2',
                 '0vnot0_HER2', '1,2vnot1,2_HER2', '3vnot3_HER2', 'gt2_HER2',
                 '0only_HER2', '1only_HER2', '2only_HER2', '1,2only_HER2', '3only_HER2')
                 # '0v1_HER2', 
                 # '0v1,2_HER2', '0v3_HER2', '1,2v3_HER2')
# desiredPlots = c('all_HER2',
#                  '0only_HER2', '1only_HER2', '2only_HER2', '3only_HER2')

labels = c('HER2 IHC, 4 category score (0, 1+, 2+, 3+)', 'HER2 IHC, 3 category score (0, Low, 3+)',
           '0 vs. not 0 HER2 IHC score', 'Low vs. not Low HER2 IHC score', '3+ vs. not 3+ HER2 IHC score',
           '< 2+ vs. >= 2+ HER2 IHC score',
           '0 score HER2 cases only, by at least one pathologist', '1+ score HER2 cases only, by at least one pathologist', 
           '2+ score HER2 cases only, by at least one pathologist', 'Low HER2 cases only, by at least one pathologist',
           '3+ score HER2 cases only, by at least one pathologist')

# labels = c('HER2 IHC, 4 category score (0, 1+, 2+, 3+)',
#            '0 score HER2 cases only, by at least one (of the 18) raters', '1+ score HER2 cases only, by at least one (of the 18) raters', 
#            '2+ score HER2 cases only, by at least one (of the 18) raters',
#            '3+ score HER2 cases only, by at least one (of the 18) raters')


datadir = 'ONEST_concord/HER2/Fleiss_Kappa'
datadir = 'ONEST_concord/HER2/OPA'
datadir = 'ONEST_concord/HER2/ICC'

flist = list.files(datadir)

for (i in 1:length(desiredPlots)){
  #Find files with similar matching names
  print(paste0('Working on ', desiredPlots[i],' plots...'))
  loadFiles = flist[grep(paste0("^",desiredPlots[i]), flist)]
  concord = read.delim(paste0(datadir,'/',loadFiles[grep('concord',loadFiles)]), sep='\t')
  plot_data = read.delim(paste0(datadir,'/',loadFiles[grep('plotStats',loadFiles)]), sep='\t')
  ONEST_plot_fromData(concord, plot_data, name=labels[i], file=desiredPlots[i], ylab="Overall Percent Agreement", color='black', percent = T)
  if(length(loadFiles)==3){
    model_data = read.delim(paste0(datadir,'/',loadFiles[grep('modelData',loadFiles)]), sep='\t')
    ONEST_plotModel_fromData(model_data, name=labels[i], file=desiredPlots[i], percent=TRUE)
  }
}

#######################################################################################
#######################################################################################
#Creating table of inter-rater reliability metrics for HER2 IHC
#######################################################################################
#######################################################################################

group = c('4 cat', '3 cat Low',
          '0 only', '1 only', '2 only', '3 only', 'Low only',
          '0 vs not 0', 'Low vs not Low', '3 vs not 3',
          '< 2 vs >= 2')

her2 = data.frame(lapply(her2, as.numeric))
her2low = categ_split(her2, type='combined_all', f1 = 0, f2 = c(1,2))

pAgg = 85

metTab = data.frame(group)
metTab$OPA = NA
metTab$Fkappa = NA
metTab$ICC = NA
rownames(metTab) = metTab$group
library(raters)

metTab = fillTable(her2, metTab, "4 cat", noICC=F, perAgree = pAgg)
mher2low=her2low
mher2low[mher2low=='1,2']=1
mher2low[mher2low==3]=2
mher2low = data.frame(lapply(mher2low, as.numeric))
metTab = fillTable(mher2low, metTab, "3 cat Low", noICC=F, perAgree = pAgg)

onlyComp = c(0,1,2,3,'1,2')
for(i in 1:length(onlyComp)){
  f1 = onlyComp[i]
  cat('Working on',f1,'only...','\n')
  if(f1=='1,2'){
    sub = categ_split(mher2low, type='only', f1 = 1)
    metTab = fillTable(sub, metTab, 'Low only', noICC=F, perAgree = pAgg)
  }else{
    sub = categ_split(her2, type='only', f1 = f1)
    metTab = fillTable(sub, metTab, paste0(f1,' only'), noICC=F, perAgree = pAgg)
  }
}

score = c(0,'1,2',3)
# score = c(0,3)
for(i in 1:length(score)){
  f1 = score[i]
  if(f1=='1,2'){
    groupN = 'Low vs not Low'
  } else{
    groupN = paste0(f1, ' vs not ', f1)
  }
  cat('Working on',groupN,'...','\n')
  sub = categ_split(her2low, type='not', f1 = f1)
  sub[sub=='not0'] = 1
  sub[sub=='0'] = 0
  sub[sub=='not3'] = 0
  sub[sub=='3'] = 1
  if(f1!='1,2'){
    sub = data.frame(lapply(sub, as.numeric))
  }
  metTab = fillTable(sub, metTab, groupN, noICC=F, perAgree = pAgg)
}

# For < 2+ vs >= 2+
sub = her2
sub[her2<2] = 0
sub[her2>=2] = 1
metTab = fillTable(sub, metTab, '< 2 vs >= 2', noICC=F, perAgree = pAgg)

write.table(metTab, 'HER2-discordance-metrics.txt', sep='\t', row.names=F, quote=F)


#######################################################################################
#######################################################################################
#Plotting HER2 stacked bar graph and boxplot distribtions
#######################################################################################
#######################################################################################

staResCase = perCaseBar(her2, name='',file='HER2_all',catLabs=c('0', '1+','2+', '3+'), legendTitle = 'HER2 IHC score', C=100)
write.table(staResCase$percent, 'HER2-sorted-ratings-frequency-matrix.txt', sep='\t', row.names=F, quote=F)
cBar = staResCase$casePlot
catLabsP=c('0', '1+','2+', '3+')[4:1]
cBar = cBar + scale_fill_manual(labels = catLabsP, guide = guide_legend(reverse = TRUE),
                                values=c('red','green4','blue','black'))
ggsave(paste0('HER2_all','_stackedBarbyCases.jpg'), plot=cBar, device="jpeg", width=10, height=5, units="in", dpi=300)

staResPath = perPathBar(her2, name='Percent of HER2 status assigned by each pathologist',file='HER2_all',catLabs=c('0', '1+','2+', '3+'), legendTitle = 'HER2 IHC score')
pBar = staResPath$pathPlot
pBar = pBar + scale_fill_manual(labels = catLabsP, guide = guide_legend(reverse = TRUE),
                                values=c('red','green4','blue','black'))
ggsave(paste0('HER2_all','_stackedBarbyPath.jpg'), plot=pBar, device="jpeg", width=10, height=5, units="in", dpi=300)


stackedD = staResCase$percent
#Find the total amount of concordant/discordant ratings within the X only cases
#Make a contingency table
scoreLabel = c('0 only',
               '1 only',
               '2 only',
               '3 only')
count_cols = colnames(stackedD)[grep('counts', colnames(stackedD))]
all_counts = stackedD[,count_cols]
cont_table = data.frame(scoreLabel)
cont_table[,count_cols] = NA
cont_table$total = NA
ratings_cont_table = cont_table
case_cont_table = cont_table
#Consensus number of ratings for case to be considered as ambiguous or the opposing category
N=1

for(i in 1:ncol(all_counts)){
  count_col = colnames(all_counts)[i]
  onlyScore = all_counts[all_counts[,count_col] >= 1,count_cols]
  #get concordant/discordant/total ratings for each other score
  ratings_cont_table[i, -1] = c(colSums(onlyScore), sum(onlyScore))
  #get number of cases scored another category by at least N ratings
  case_cont_table[i, -1] = c(colSums(onlyScore>=N), nrow(onlyScore))
}

write.table(ratings_cont_table, 'HER2-ratings-contingency-table.txt', sep='\t', row.names=F, quote=F)
write.table(case_cont_table, 'HER2-cases-contingency-table.txt', sep='\t', row.names=F, quote=F)

spAgData = spAgree(her2, weighting='identity')
