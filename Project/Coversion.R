# Load packages
require(knitr)
require(markdown)

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("StatisticInference_PA_Part1.Rmd")
markdownToHTML('StatisticInference_PA_Part1.md', 'StatisticInference_PA_Part1.html', options=c("use_xhml"))
system("pandoc -s StatisticInference_PA_Part1.html -o StatisticInference_PA_Part1.pdf")
