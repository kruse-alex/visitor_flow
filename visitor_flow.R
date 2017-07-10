# create user paths from data frame
seq = mydata %>%
group_by(user.id) %>%
summarise(Path = paste(list(screen_name), sep = ", "))
 
# remove ugly stuff from paths
seq$Path = gsub("c\\(|)|\"|([\n])|-","", seq$Path)
seq$Path = gsub("\\","", seq$Path, fixed = T)
 
# split path column into single columns and create sequence object
seq_table = cSplit(as.data.table(seq), "Path", ",")
seq_table = seqdef(seq_table)
 
# create empty df for later
orders.plot = data.frame()
 
# save sequence object as df
orders = as.data.frame(seq_table[2:length(seq_table)])
 
# convert ugly % to END
orders = as.data.frame(lapply(orders, function(y) gsub("%", "END", y)))
orders[length(orders)+1] = "END"
 
# transform data to long table format for ploting
for (i in 2:ncol(orders)) {
 
ord.cache = orders %>%
group_by(orders[ , i-1], orders[ , i]) %>%
summarise(n=n())
 
colnames(ord.cache)[1:2] = c('from', 'to')
 
ord.cache$from = paste(ord.cache$from, '(', i-1, ')', sep='')
ord.cache$to = paste(ord.cache$to, '(', i, ')', sep='')
 
orders.plot = rbind(orders.plot, as.data.frame(ord.cache))
 
}
 
# plot sankey
plot(gvisSankey(orders.plot, from='from', to='to', weight='n',
options=list(height=900, width=1800,
sankey="{link:{colorMode: 'source',
color:{fill:'source'}}}")))
