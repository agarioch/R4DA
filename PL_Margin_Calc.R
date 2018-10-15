# Read sample profit and loss data
pl <- read.csv("Test_PL_Data.csv")
# Convert amounts to account color for margin calcs
pl$Amt <- ifelse(pl$Account == "Sales", pl$Amount * -1, pl$Amount)
# Approach one: drop account and amount and aggregate
om <- pl[pl$Account != "BTL", !(names(om) %in% c("Account", "Amount"))]
aggregate(om$Amt~., om, sum)
# Approach two: just drop account but also aggregate amount
om2 <- pl[pl$Account != "BTL", names(om) != "Account"]
aggregate(cbind(om2$Amt, om2$Amount)~., om, sum)