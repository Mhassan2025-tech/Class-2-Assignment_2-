# Create Variables

Height <- c(1.5,1.6,1.9,1.45)
c(56,60,71,68) -> Weight

# Assignment operators

Smoking_Status = c("Yes","No","Yes","No")

# Arithmetic Operators
BMI = Weight/(Height^2)

# Comparison operator

BMI >25
BMI < 25

# Logical Operator

(BMI > 25) & (Smoking_Status=="Yes")
(BMI < 25) | (Smoking_Status=="No")

# Creating Vectors

Patient_ID = c(25,50,75,100)
Patient_Name = c("Hassan","Ahmed","Irfan","Asghar")
Weight = c(45,55,56,61)
Height = c(1.50,1.60,1.65,1.70)
Logical_Vector = c(TRUE,FALSE)

# All Vectors are combined
Combined_Data=cbind(Patient_ID,Patient_Name,Weight,Height)


# Creating Matrix
My_Matrix= matrix(1:9,nrow = 3,ncol = 3,byrow = TRUE)

My_Matrix[3,]

# Import Data from CSV

Data_1 = read.csv(file.choose())
Data_2 = read.csv(file.choose())

# Replace Missing Values from padj with 1

Data_1$padj[is.na(Data_1$padj)]=1
Data_2$padj[is.na(Data_2$padj)]=1

# Gene Status Classification by Using Function

Classify_Gene_Expression = function(logFC,padj) {
  if(logFC > 1 & padj < 0.05) {
    return("Upregulated")}
   else if(logFC < -1 & padj < 0.05) {
    return("Downregulated")} else {
      return("Not_Significant")
    }
}

# Add Status Column in Dataset by using For Loop

Data_1$Status = NA
for (i in 1:nrow(Data_1)) {
  Data_1$Status[i] = Classify_Gene_Expression(Data_1$logFC[i],Data_1$padj[i])
}

Data_2$Status = NA
for (i in 1:nrow(Data_2)) {
  Data_2$Status[i] = Classify_Gene_Expression(Data_2$logFC[i],Data_2$padj[i])
}
setwd("C:/Users/AHLp/OneDrive/Desktop/AI & Biotech/Assignments/Script/Results")

# Export Resutls

write.csv(Data_1,"Results.csv",row.names = FALSE)
write.csv(Data_2,"Results.csv",row.names = FALSE)

# Summary Count of Each status

table(Data_1$Status)
table(Data_2$Status)

