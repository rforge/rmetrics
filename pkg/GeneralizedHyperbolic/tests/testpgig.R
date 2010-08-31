require(GeneralizedHyperbolic)
### Test pgig
### compare with figures supplied by Slevinsky and Safouhi

testData <- matrix(c(0.1, 1, 1, 0,
                     0.2, 1, 1, 0,
                     0.5, 1, 1, 0,
                     2, 2, 1, 1,
                     2, 2, 1, -1,
                     3, 2, 2, 2,
                     3, 2, 2, -2,
                     5, 3, 3, 4,
                     6, 3, 4, 5,
                     20, 1, 1, 0),
                   byrow = TRUE, ncol = 4)
testResults <- c(0.9986941930676488,
                 0.9726685983580131,
                 0.7797339136097861,
                 0.6181518477661696,
                 0.1159938316382597,
                 0.3060636385450593,
                 0.1469821513077778*10^(-2),
                 0.8868482756840722*10^(-1),
                 0.1199275449037456*10^(-1),
                 0.4824320137970867*10^(-5))
numTests <- NROW(testData)

results <- numeric(numTests)

for(i in 1:numTests){
  results[i] <- pgigIBF(testData[i, 1], param = testData[i, 2:4],
                     lower.tail = FALSE)
}

results - testResults
max(abs(results - testResults))

for(i in 1:numTests){
  results[i] <- pgigIBF(testData[i, 1], param = testData[i, 2:4],
                     lower.tail = FALSE, ibfTol = .Machine$double.eps)
}

results - testResults
max(abs(results - testResults))

intResults <- numeric(numTests)
for(i in 1:numTests){
  intResults[i] <- integrate(dgig, lower = testData[i, 1], upper = Inf,
                       param = testData[i, 2:4])$value
}

intResults - testResults
max(abs(testResults - intResults))

results - intResults
max(abs(results - intResults))

