# ---
# formula
# ---

(obj <- amBoxplot(count ~ spray, data = InsectSprays))
print(obj)

# Adding parameters
amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
          xlab = "spray", col = c("darkblue", "gray"))

# Transpose
amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50), xlab = "spray", horiz = TRUE)

# Using a custom colum to identify outliers
InsectSprays$id <- paste0("ID : ", 1:nrow(InsectSprays))
amBoxplot(count ~ spray, data = InsectSprays, id = "id")

# Parameter for amOptions
amBoxplot(count ~ spray, data = InsectSprays, main = "amcharts")

# ---
# data.frame
# ---

don <- data.frame(a = 1:10, b = 1:5)
amBoxplot(don, ylim = c(0,15))

# Parameter for amOptions
amBoxplot(count ~ spray, data = InsectSprays, creditsPosition = "top-right")

# ---
# matrix
# ---

x <- matrix(nrow = 10, ncol = 5, rnorm(50))

amBoxplot(x) # on columns
colnames(x) <- LETTERS[1:5]
amBoxplot(x) # with names
amBoxplot(x, use.cols = FALSE, col = c("blue", "red"))

# Parameter for amOptions
amBoxplot(x, export = TRUE, exportFormat = "SVG")

# ---
# vector
# ---

amBoxplot(rnorm(100))

# Parameter for amOptions
amBoxplot(x, zoom = TRUE)
