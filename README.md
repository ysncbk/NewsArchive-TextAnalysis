# NewsArchive-TextAnalysis
Jacobs Uni News Archive -Text Analysis by R


Jacobs University Newsarchive
Download the Jacobs University press release archives
https://www.jacobs-university.de/press-release-archive-2008
https://www.jacobs-university.de/press-release-archive-2015
1. Convert the html to text files and separate the individual news items. The individual press
release items serve as documents.
2. Remove stop words and perform stemming.
3. Perform a frequency analysis to compute the term-document (TD) matrix. What are the
most common terms?
4. Compute inverse-document frequency (IDF) and term importance (TI). What are now the
most common terms?
5. Compute pairwise cosine and Euclidean distance between all documents.
6. Apply a multi-dimensional scaling approach to the distance matrix and render a 2D scatter-
plot. Compare the two distance metrics.
7. Bonus: Capture the year of release during parsing and color code the scatterplot by time.
Produce a Word Cloud for each year.
