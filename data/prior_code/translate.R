# for visualization purposes we translated the raw data into 
# a format that included capital letters; this translation scheme
# is found in the main analysis document, but we include both 
# raw and translated data here, too
setwd('/Users/rickdale/Dropbox/new.projects/cornish-strings/cornish-strings-repo/data/prior_code')
cToAll = c('FCMLSPZ','SGTVBZ','KXLSWCZ','PNFBJQZ','NPXQJL','HPVNCTZL','GBRLVFZ','SRZMLP')
cFromAll = c('dafecbh','abdcef','feacbdg','eacfbdg','adfcbe','fabdcehi','ebdfacg','cbadef')



# NB: some mappings are arbitrary based on later generations (e.g., h in Gen 1, g in Gen 3)

for (chain in 1:8) {
  # load this chain file
  # strs = read.table(paste('chain',chain,'.chain',sep=''),sep='\t')  
  strs = readChar(paste('chain',chain,'.chain',sep=''),file.info(paste('chain',chain,'.chain',sep=''))$size)
  cTo = unlist(strsplit(cToAll[chain],''))
  cFrom = unlist(strsplit(cFromAll[chain],''))
  for (charNum in 1:length(cFrom)) { strs = gsub(cFrom[charNum],cTo[charNum],strs) }     
  writeChar(strs,paste('../chain',chain,'.chain',sep=''))
}


