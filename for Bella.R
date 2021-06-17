# taking the verb stem only
datv=simple[simple$BaseWordClass=="v",]

# taking only the affixes without pepen
tab2 = datv[,x]

# indicationg the filled and empty rows by number
tab2[tab2==""]=0
tab2[tab2!="0"]=1
for (i in 1:ncol(tab2)) {
  tab2[,i] = as.numeric(tab2[,i])
}
counts = apply(tab2,2,sum)
counts

# counting pe- cooccurences
tab2 = datv_pe2
tab2[tab2==""]=0
tab2[tab2!="0"]=1
for (i in 1:ncol(tab2)) {
  tab2[,i] = as.numeric(tab2[,i])
}
counts_pe = apply(tab2,2,sum)
counts_pe

# counting peN- cooccurences
tab2 = datv_peN2
tab2[tab2==""]=0
tab2[tab2!="0"]=1
for (i in 1:ncol(tab2)) {
  tab2[,i] = as.numeric(tab2[,i])
}
counts_peN = apply(tab2,2,sum)
counts_peN

# creating counts dataframe
counts2 = data.frame(counts)
counts2
counts2$Affix <- rownames(counts2)
names(counts_pe) = c("Pe", "i", "kan", "Ber", "Beran", "Beri", "Berkan", "Di", "Dii", "Dikan", "Diper", "MeN", "MeNi", "MeNkan", "Memper", "Memperi", "Memperkan", "Peran", "Ter", "Teri", "Others")
counts_pe2 = data.frame(counts_pe)
counts_pe2$Affix <- rownames(counts_pe2)
names(counts_peN) = c("PeN", "i", "kan", "Ber", "Beran", "Beri", "Berkan", "Di","Dian" , "Dii", "Dikan", "Diperi","Ke", "Kean", "MeN", "MeNi", "MeNkan", "Memper", "Memperi", "Memperkan", "Peran", 
                      "Ter", "Teri", "Others", "Memberkan")
counts_peN2 = data.frame(counts_peN)
counts_peN2$Affix <- rownames(counts_peN2)

allcounts = merge(counts_pe2, counts2, by = "Affix")
allcounts = merge(allcounts, counts_peN2, by = "Affix")
allcounts = allcounts[,c(1, 3, 2, 4)]
allcounts # note: we missed 5 affixes which are not the intersection of pe- and peN- (dian, diperi, kean, ke, memberkan exisist in pen- coocurences while not in pe) 
colSums(allcounts[,-1])

# from the counts, peN is more productive than pe; moreover, peN also cooccurs with ber- even more than pe-
simple[simple$PenBer == "Pen-&ber-",c(1, 2, 3, 4, 5, 10, 11, 29, 51, 54, 66)][1:10,]

X11()
par(mfrow = c(1,2))
plot(allcounts$counts,allcounts$counts_pe, type="n", xlab = "Base Counts", ylab = "Pe Counts")
text(allcounts$counts,allcounts$counts_pe, allcounts$Affix, col = "red")
plot(allcounts$counts,allcounts$counts_peN, type="n", xlab = "Base Counts", ylab = "PeN Counts")
text(allcounts$counts,allcounts$counts_peN, allcounts$Affix, col = "blue") # VErY NICELY CORRELATED!

legend("bottomright", # places a legend at the appropriate place 
       c("pe-","peN-"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","blue")) # gives the legend lines the correct color and width
