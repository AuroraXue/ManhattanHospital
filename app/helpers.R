#setwd('/home/kltduong/_code/machine_learning/surgeon_score/repo/lib/Compare HAI/')
hospitals <- read.csv("data/Hospital-Acquired_Infections__Beginning_2008.csv", na.strings=c("","Not Available"), stringsAsFactors=FALSE)
hospitals[,"Infections.observed"] <- as.numeric(as.character(hospitals[,"Infections.observed"]))

#hospital.list <- hospitals[hospitals$Year == "2008",]
#hospital.list = aggregate(hospital.list$Infections.observed, by=list(Category=hospital.list$Hospital.Name), FUN=sum)
#hospital.list = na.omit(hospital.list)
#colnames(hospital.list) = c("Name", "Infections.observed")

ny = tolower(c("BELLEVUE HOSPITAL CENTER", "HARLEM HOSPITAL CENTER", "HOSPITAL FOR SPECIAL SURGERY", "LENOX HILL HOSPITAL", "METROPOLITAN HOSPITAL CENTER", "MOUNT SINAI BETH ISRAEL/PETRIE CAMPUS", "MOUNT SINAI HOSPITAL", "N Y EYE AND EAR INFIRMARY", "NEW YORK-PRESBYTERIAN HOSPITAL", "NYU HOSPITALS CENTER", "ROCKEFELLER UNIVERSITY HOSPITAL", "ST LUKE'S ROOSEVELT HOSPITAL"))
hospitals = hospitals[tolower(hospitals$Hospital.Name) %in% ny, ]
