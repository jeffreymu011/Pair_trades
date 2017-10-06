
trades <- read.table("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/pair_catalog.txt")

pdf("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/S&P_Utilities.pdf")

for (line in as.character(trades$V1)){
  print(line)
  
  eval(parse(text=line))
}
dev.off()

dev.off()
