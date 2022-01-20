UNIMAS = read.csv("UNIMAS.csv")
UMT = read.csv("UMT.csv")
UMS = read.csv("UMS.csv")
UUM = read.csv("UUM.csv")

uni_name = "Univesity Malaysia Terengganu (UMT)"
UMT =cbind(UMT, uni_name)

uni_name = "Univesity Malaysia Sabah (UMS)"
UMS =cbind(UMS, uni_name)

uni_name = "Univesity Malaysia Sarawak (UNIMAS)"
UNIMAS =cbind(UNIMAS, uni_name)

uni_name = "Univesity Utara Malaysia (UUM)"
UUM =cbind(UUM, uni_name)

data = rbind(UMS, UMT, UNIMAS, UUM)

##Faculty
FacUM = read.csv("UMfac.csv")
FacUSM = read.csv("USMfac.csv")
FacUKM = read.csv("UKMfac.csv")
FacUTM = read.csv("UTMfac.csv")
FacUUM = read.csv("UUMfac.csv")
FacUMS = read.csv("UMSfac.csv")
FacUMT = read.csv("UMTfac.csv")
FacUNIMAS = read.csv("UNIMASfac.csv")
FacUMP = read.csv("UMPfac.csv")
FacUNIMAP = read.csv("UNIMAPfac.csv")
FacUMK = read.csv("UMKfac.csv")
FacUTHM = read.csv("UTHMfac.csv")
FacUTEM = read.csv("UTEMfac.csv")
FacUPSI = read.csv("UPSIfac.csv")
FacUnisza = read.csv("UNISZAfac.csv")
FacUPNM = read.csv("UPNMfac.csv")

faculty = rbind(FacUM,FacUSM, FacUKM,FacUTM,FacUUM,FacUMS,FacUMT,FacUNIMAS,FacUMP,
                FacUNIMAP, FacUMK, FacUTHM, FacUTEM, FacUPSI, FacUnisza, FacUPNM)
