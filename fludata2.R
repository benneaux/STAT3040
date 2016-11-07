require(tm)
require(pdftools)
require(stringi)
require(xlsx)

#setwd("Data/Reports/errors")
files <- list.files(pattern = "xlsx$")

data <- matrix(nrow=length(files),ncol = 12)
val1 <- vector(length=length(files))
val2 <- vector(length=length(files))
val3 <- vector(length=length(files))
val4 <- vector(length=length(files))
val5 <- vector(length=length(files))
val6 <- vector(length=length(files))
val7 <- vector(length=length(files))
val8 <- vector(length=length(files))
val9 <- vector(length=length(files))
val10 <- vector(length=length(files))
val11 <- vector(length=length(files))
val12 <- vector(length=length(files))

for(i in 1:length(files)){
  tryCatch({
    txt <- read.xlsx(files[i], 1)
    if(
      is.na(stri_extract_first_regex(txt[2,1],"[0-9]{2} [A-Z][a-z]{1,10} [0-9]{4}"))) {
      val1[i] <- unlist(stri_extract_first_regex(txt[2,1],"[0-9]{1} [A-Z][a-z]{1,10} [0-9]{4}"))
    } else {
      val1[i] <- unlist(stri_extract_first_regex(txt[2,1],"[0-9]{2} [A-Z][a-z]{1,10} [0-9]{4}"))
    }
    val2[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[4,1],"(received [0-9]*|[0-9]* responses)"),"\\p{N}"))
    val3[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[4,1],"[0-9]{3,} people|from [0-9]{3,}"),"\\p{N}"))
    val4[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[4,1],"[0-9]{3,} household|[0-9]{3,}\r\nhousehold"),"\\p{N}"))
    val5[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[5,5],"[0-9]{3,}+/"),"\\p{N}"))
    val6[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[5,5],"/+[0-9]{3,}"),"\\p{N}"))
    val7[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[5,5],"[0-9]{3,} participants|vaccine so far. Of the [0-9]{3,}"),"\\p{N}"))
    val8[i] <- unlist(stri_extract_all_charclass(stri_extract_all_regex(txt[5,5],"patients, [0-9]{3,}"),"\\p{N}"))
    val9[i] <- unlist(stri_extract_all_regex(stri_extract_first_regex(txt[5,5],"\\d[.]+\\d\\s% of vaccinated|\\d\\s% of vaccinated|reported by \\d[.]+\\d\\s%"),"\\d[.]+\\d|\\d"))
    val10[i] <- unlist(stri_extract_all_regex(stri_extract_first_regex(txt[5,5],"\\d{0,}[.]{0,}\\d\\s*% of unvaccinated|\\d\\s*% of unvaccinated|vaccinated participants and \\d{0,}[.]{0,}+\\d\\s*%"),"\\d[.]+\\d|\\d\\s*%"))
    val11[i] <- unlist(stri_extract_all_regex(stri_extract_last_regex(txt[5,5],"\\d{0,}[.]{0,}\\d\\s*% of vaccinated|\\d\\s*% of vaccinated|reported by \\d[.]+\\d\\s*%"),"\\d[.]+\\d|\\d\\s*%"))
    val12[i] <- unlist(stri_extract_all_regex(stri_extract_last_regex(txt[5,5],"\\d{0,}[.]{0,}\\d\\s*% of unvaccinated|\\d\\s*% of unvaccinated|vaccinated participants and \\d{0,}[.]{0,}+\\d\\s*%"),"\\d[.]+\\d|\\d\\s*%"))    
    vals <- as.data.frame(cbind(val1,val2,val3,val4,val5,val6,val7,val8,val9,val10,val11,val12))
  },
  error=function(e){})
}
data[,1:12] <- vals
colnames(vals) <- c("Week_end",
                    "Responses",
                    "Self_Report",
                    "Other_Report",
                    "Vaccinated_Respondents",
                    "Total_Respondents",
                    "Clinical_Staff",
                    "Clinical_Staff_Vaccinated",
                    "ILI_Vaccinated",
                    "ILI_Unvaccinated",
                    "ILI_wAbsence_Vaccinated",
                    "ILI_wAbsence_Unvaccinated")
write.csv(vals,file="fludata2.csv")
