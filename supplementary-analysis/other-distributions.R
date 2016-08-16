setwd('/Users/rickdale/Dropbox/projects/studies/rickmorten/cornishetal/PloS\ ONE\ revision/supplementary-analysis')
set.seed(2) # stable randomization seed for statistics in paper

###################################################################
# google ngram analysis; from Sindi & Dale, 2016
###################################################################
a = read.table('allWords1900on.txt',header=F)
colnames(a) = list('wd','yr','fq','pgs','bks','prop')
# first digit is Benford's law; let's exemplify it with these Google Ngram words
digits = unlist(strsplit(paste(substr(as.character(a$fq),1,1),collapse=''),''))
plot(table(digits)[1:10],ylab='Digit frequency',xlab='Digit',type='b',lwd=2)
# now test scaling
strings = sample(as.character(a$fq),5000) # first 5,000 words
pdf(file="Figure-Google-Ngram.pdf",width=6,height=6)
buildScalingGraph(strings,'Frequency digit length','Proportion strings connected','Word frequencies',c(4,7),c(0,1)) # see function definition below
dev.off()
# so while it shows Benford's, it does not show our network-type scaling

###################################################################
# for fun, let's do some words from the Ngram set!
###################################################################
strings = unique(tolower(sample(a$wd,1000)))
buildScalingGraph(strings[nchar(strings)>=3&nchar(strings)<=6],'Length of word','Proportion words connected','Ngram Words',c(3,6),c(0,.2))

###################################################################
# password numbers 
###################################################################
a = readChar('10-million-combos.txt',file.info('10-million-combos.txt')$size)
lines = unlist(strsplit(a,'\n'))
length(lines)
strings = c()
for (i in sample(1:10000000,5000)) { # sample of 5000 passwords
  s = unlist(strsplit(lines[i],'\t'))[2]
  res = regexpr("\\d\\d\\d\\d*",s)
  strings = c(strings,regmatches(s,res))
}
pdf(file="Figure-Passwords.pdf",width=6,height=6)
buildScalingGraph(strings[nchar(strings)>=3 & nchar(strings)<=6],
                  'Password number length','Proportion strings connected','Password digit sequences',c(3,6),c(0,1))
dev.off()
# does not show scaling at all; all strings independent of others;
# minor patterning may be due to common bigrams in dates (e.g., 1969, etc.)
# https://archive.org/details/10MillionPasswords

###################################################################
# based on Towse et al. PLOS ONE paper on joint random generation; get windows as strings
# these are subjects from the "neglect" condition, partner A
###################################################################
a = read.table('neglect_subjects_towse_et_al (a).txt',header=F) 
a[a==10] = 0
# first few subjects appended
pstr = paste(paste(as.character(a$V1),collapse=''),
             paste(as.character(a$V2),collapse=''),
             paste(as.character(a$V3),collapse=''),
             paste(as.character(a$V4),collapse=''),
             paste(as.character(a$V5),collapse=''),
             collapse='')
pstr = gsub(' ','',pstr)
strings = c()
while (nchar(pstr)>6) {
  sz = sample(c(3:6),1)
  strings = c(strings,substr(pstr,1,sz))
  pstr = substr(pstr,sz+1,nchar(pstr))
}
pdf(file="Figure-Random-Nums.pdf",width=6,height=6)
buildScalingGraph(strings,'Digit window size','Proportion strings connected','Human-generated random digits',c(3,6),c(0,1))
dev.off()

###################################################################
# test to confirm code is doing what we think:
###################################################################
strings = c('123456','1234','345','2345','456','2345','3456','1234','123','123456','123','12345','456')
buildScalingGraph(strings,'Length','Proportion strings connected','Test sequences',c(3,6),c(0,1))

###################################################################
# reproduce prior CHILDES analysis to make sure!
###################################################################
a = read.table('/Users/rickdale/Dropbox/new.projects/cornish-strings/cornish-strings-repo/data/Mega-Eng-POS-CHILDES.txt',sep='\t')
a = a[1:1000,]
pos = unique(unlist(strsplit(paste(a$V2,collapse=''),' ')))
a$string = ''
convert = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n')
for (i in 1:1000) {
  print(i)
  thisStr = substr(as.character(a[i,]$V2),1,nchar(as.character(a[i,]$V2))-1)
  a[i,]$string = paste(unlist(lapply(unlist(strsplit(thisStr,' ')),function(x) {
    return(convert[which(x==pos)])
  })),collapse='')
}
strings = a$string
buildScalingGraph(strings[nchar(strings)>=3&nchar(strings)<=6],'Length of sentence','Proportion sentences connected','CHILDES',c(3,6),c(0,1))

###################################################################
# function to build rescaling graph for one set of strings
###################################################################
buildScalingGraph = function(strings,xlab,ylab,gtitle,xlim,ylim) {
  for (i in 1:length(strings)) {
    # print(i)
    s = strings[i]
    # shuffle for baseline
    s = paste(unlist(strsplit(s,''))[sample(1:nchar(s))],collapse='')
    stringBigs = apply(embed(unlist(strsplit(s,'')),2)[,2:1],1,function(x) 
      { 
        return(paste(x,collapse='')) 
      })
    x = c()
    for (bg in stringBigs) {
      x = c(x,grep(bg,strings)) # does include itself; so at least 1 per set
    }
    if (i==1) {
      scaleData = data.frame(len=nchar(s),prop=length(unique(x))/length(strings))
    } else {
      scaleData = rbind(scaleData,data.frame(len=nchar(s),prop=length(unique(x))/length(strings)))
    }
  }
  plot(scaleData$len,scaleData$prop,col='red',xlab=xlab,ylab=ylab,ylim=ylim,xlim=xlim,main=gtitle,xaxt='n')
  axis(1, at=c(2:10))
  abline(lm(scaleData$prop~scaleData$len),col='red',lwd=3)
  allScale = scaleData
  allScale$shuff = T
  
  for (i in 1:length(strings)) {
    # print(i)
    s = strings[i] # no shuffling, use blue for observed scaling
    stringBigs = apply(embed(unlist(strsplit(s,'')),2)[,2:1],1,function(x) 
    { 
      return(paste(x,collapse='')) 
    })
    x = c()
    for (bg in stringBigs) {
      x = c(x,grep(bg,strings)) # does include itself; so at least 1 per set
    }
    if (i==1) {
      scaleData = data.frame(len=nchar(s),prop=length(unique(x))/length(strings))
    } else {
      scaleData = rbind(scaleData,data.frame(len=nchar(s),prop=length(unique(x))/length(strings)))
    }
  }
  points(scaleData$len,scaleData$prop,col='blue')
  abline(lm(scaleData$prop~scaleData$len),col='blue',lwd=3)
  scaleData$shuff = F
  allScale = rbind(allScale,scaleData)
  print(summary(lm(prop~len*shuff,data=allScale))) # get stats for this ; check for interaction b/w condition and length
}

##################################################################
# reproduce the scaling analysis for experimental data
###################################################################
getScalingData = function(strings) {
  for (i in 1:length(strings)) {
    # print(i)
    s = strings[i]
    # shuffle for baseline
    s = paste(unlist(strsplit(s,''))[sample(1:nchar(s))],collapse='')
    stringBigs = apply(embed(unlist(strsplit(s,'')),2)[,2:1],1,function(x) 
    { 
      return(paste(x,collapse='')) 
    })
    x = c()
    for (bg in stringBigs) {
      x = c(x,grep(bg,strings)) # does include itself; so at least 1 per set
    }
    if (i==1) {
      scaleData = data.frame(len=nchar(s),prop=length(unique(x))/length(strings))
    } else {
      scaleData = rbind(scaleData,data.frame(len=nchar(s),prop=length(unique(x))/length(strings)))
    }
  }
  allScale = scaleData
  allScale$shuff = T
  
  for (i in 1:length(strings)) {
    # print(i)
    s = strings[i] # no shuffling, use blue for observed scaling
    stringBigs = apply(embed(unlist(strsplit(s,'')),2)[,2:1],1,function(x) 
    { 
      return(paste(x,collapse='')) 
    })
    x = c()
    for (bg in stringBigs) {
      x = c(x,grep(bg,strings)) # does include itself; so at least 1 per set
    }
    if (i==1) {
      scaleData = data.frame(len=nchar(s),prop=length(unique(x))/length(strings))
    } else {
      scaleData = rbind(scaleData,data.frame(len=nchar(s),prop=length(unique(x))/length(strings)))
    }
  }
  scaleData$shuff = F
  allScale = rbind(allScale,scaleData)
  return(allScale)
}

dPath = '/Users/rickdale/Dropbox/new.projects/cornish-strings/cornish-strings-repo/data/'
for (chain in 1:8) {
  a = read.table(paste(dPath,'chain',chain,'.chain',sep=''))
  for (gen in 1:11) {
    dat = getScalingData(as.character(a[,gen]))
    dat$gen = gen
    if (chain==1 & gen==1) {      
      allDat = dat
    } else {
      allDat = rbind(allDat,dat)
    }
  }
}
subd = allDat[allDat$gen==11&!allDat$shuff,]
plot(subd$len,subd$prop,col='blue',ylim=c(0,1),cex=1.2)
abline(lm(prop~len,data=subd),col='blue',lwd=2)
subd = allDat[allDat$gen==11&allDat$shuff,]
points(subd$len,subd$prop,col='red',ylim=c(0,1),cex=.5,pch=15)
abline(lm(prop~len,data=subd),col='red',lwd=2)
subd = allDat[allDat$gen>=9,] # interaction invariably significant at later generations (after many iterations, across seeds)
summary(lm(prop~len*shuff,data=subd))



