# We need to copy-paste table 2 (without headers) of page 62 of the report 
# http://www.oie.int/fileadmin/Home/fr/Our_scientific_expertise/docs/pdf/AMR/Survey_on_monitoring_antimicrobial_agents_Dec2016.pdf
esvac <- readLines("esvac.txt")
esvac <- sub("^#.*", "", esvac)
esvac <- sub(" *", "", esvac)
esvac <- esvac[esvac != ""]
esvac <- matrix(esvac, ncol = 4, byrow = TRUE)
esvac <- as.data.frame(esvac, stringsAsFactors = FALSE)
names(esvac) <- c("ab_vet_med", "ab_oie", "IU/mg", "mg/IU")
esvac[, 3] <- as.numeric(esvac[, 3])
esvac[, 4] <- as.numeric(esvac[, 4])
# Writing to disk:
if (!dir.exists("data")) dir.create("data")
write.csv(esvac, "data/esvac.csv", FALSE, row.names = FALSE)