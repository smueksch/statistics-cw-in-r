#work_dir <- "<insert working directory>"
#outlier_out <- paste(work_dir, "output-with-outlier.txt", sep="/")
#no_outlier_out <- paste(work_dir, "output-without-outlier.txt", sep="/")

#sink(outlier_out)

PAphos <- c(64,60,71,61,54,77,81,93,93,51,76,96,77,93,95,54,168,99) # Plant-available phosphorus
inorg <- c(0.4,0.4,3.1,0.6,4.7,1.7,9.4,10.1,11.6,12.6,10.9,23.1,    # Inorganic phosphorus
           23.1,21.6,23.1,1.9,26.8,29.9)
org <-  c(53,23,19,34,24,65,44,31,29,58,37,46,50,44,56,36,58,51)    # Organic phosphorus

par(mfrow=c(1,2))

plot(inorg,PAphos,xlab="Inorganic phosphate", ylab="Plant available phosphate")
plot(org,PAphos,xlab="Organic phosphate", ylab="Plant available phosphate")

inorg_mdl = lm(PAphos ~ inorg)
org_mdl = lm(PAphos ~ org)
inorg_org_mdl = lm(PAphos ~ inorg + org)

print(inorg_mdl)
print(summary(inorg_mdl))

print(org_mdl)
print(summary(org_mdl))

print(inorg_org_mdl)
print(summary(inorg_org_mdl))

#### Models without the outlier 168 ####

sink(no_outlier_out)

# Plant-available phosphorus
PAphos_noout <- c(64,60,71,61,54,77,81,93,93,51,76,96,77,93,95,54,99)

# Inorganic phosphorus
inorg_noout <- c(0.4,0.4,3.1,0.6,4.7,1.7,9.4,10.1,11.6,12.6,10.9,23.1,
           23.1,21.6,23.1,1.9,29.9)

# Organic phosphorus
org_noout <-  c(53,23,19,34,24,65,44,31,29,58,37,46,50,44,56,36,51)

par(mfrow=c(1,2))

inorg_noout_mdl = lm(PAphos_noout ~ inorg_noout)
org_noout_mdl = lm(PAphos_noout ~ org_noout)
inorg_org_noout_mdl = lm(PAphos_noout ~ inorg_noout + org_noout)

print(inorg_noout_mdl)
print(summary(inorg_noout_mdl))

print(org_noout_mdl)
print(summary(org_noout_mdl))

print(inorg_org_noout_mdl)
print(summary(inorg_org_noout_mdl))

plot(inorg_noout,PAphos_noout,xlab="Inorganic phosphate", ylab="Plant available phosphate")
plot(org_noout,PAphos_noout,xlab="Organic phosphate", ylab="Plant available phosphate")

sink() # Redirect output to console again.
