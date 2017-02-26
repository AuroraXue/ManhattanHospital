hospitals <- read.csv("data/Healthcare_Associated_Infections_-_Hospital_final.csv", na.strings=c("","Not Available"), stringsAsFactors=FALSE)
cityzip <- read.csv("data/cityzip.csv", stringsAsFactors=FALSE)
statenums <- read.csv("data/Healthcare_Associated_Infections_State.csv", stringsAsFactors=FALSE, na.strings=c("","Not Available"))
stateabbs <- read.csv("data/states.csv", stringsAsFactors=FALSE)

hospitals[,"Score"] <- as.numeric(as.character(hospitals[,"Score"]))
hospitals <- hospitals[complete.cases(hospitals),]
keepmeasures <- c("HAI_1_SIR" = "Central line-associated blood stream infections (CLABSI)",
                  "HAI_2_SIR" = "Catheter-Associated Urinary Tract Infections (CAUTI)",
                  "HAI_3_SIR" = "Surgical Site Infection from colon surgery (SSI: Colon)",
                  "HAI_4_SIR" = "Surgical Site Infection from abdominal hysterectomy (SSI: Hysterectomy)",
                  "HAI_5_SIR" = "Methicillin-resistant Staphylococcus Aureus (MRSA) Blood Laboratory-identified Events (Bloodstream infections)",
                  "HAI_6_SIR" = "Clostridium difficile (C.diff.) Laboratory-identified Events (Intestinal infections)")
hospitals <- hospitals[hospitals$Measure.Name %in% keepmeasures,]
hospitals[,"Measure.Name"] <- as.factor(hospitals[,"Measure.Name"])

hospitals$latlon <- gsub("[\\(\\)]", "", regmatches(hospitals[, "Location"], gregexpr("\\(([^A-Z]){10,}?\\)", hospitals[, "Location"])))
locations <- strsplit(hospitals$latlon, ", ")

options(digits=16) # To maintain spatial accuracy

library(plyr)
locations <- ldply(locations)
names(locations) <- c("Latitude","Longitude")
locations$Latitude <- as.numeric(locations$Latitude)
locations$Longitude <- as.numeric(locations$Longitude)
hospitals$Latitude <- locations$Latitude
hospitals$Longitude <- locations$Longitude
hospitals <- hospitals[, !(names(hospitals) %in% c("Location","latlon"))]

options(digits=8)
statenums <- statenums[,-4]
statenums <- statenums[complete.cases(statenums),]
statenums <- statenums[statenums$Provider.ID %in% stateabbs$Abbreviation, ]
for(i in 1:length(statenums$Provider.ID)) {
    statenums$state[i] <- stateabbs[stateabbs$Abbreviation == statenums$Provider.ID[i], "State"]
}

