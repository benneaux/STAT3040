---
title: Time-series Analysis of Self-reported Influenza-like Illness
author: [Poster Author 1, Poster Author 2, Poster Author 3]
email: author@university.edu
institute: School of Science and IT, UoN
longinstitute: School of Science and IT, University of Newcastle, Australia
web: 'newcastle.edu.au'
# biblio-files: parsed-references.bib
posteroptions: width=90,height=110,scale=1.2 #,grid
headerheight: 13cm
# large, Large, LARGE, huge, Huge, veryHuge, VeryHuge, VERYHuge
titlefont: size=\veryHuge,series=\bfseries
authorfont: size=\huge
institutefont: size=\Large
knit: (function(input, encoding, make = TRUE) { source('Templates/templates/makefile-renderer.R', local = TRUE) })
---

<!-- %% filter=Templates/templates/poster-filters.py -->
<!-- %% biblatex -->



[columns=2]

[column]

# Introduction

### Graphs

![two figures side-by-side]({width=0.5\linewidth}presentation-examplefig,{width=0.5\linewidth}presentation-examplefig-magenta)

<!-- Comments -->
### Default lists

<!-- - Citations [@Macherey2006] and @Macherey2006 -->
- references have a clickable link to Pubmed or Amazon
- Standard abreviations \\eg and \\ie for \eg and \ie
- Units like \pps{900}
- **Highlights** and *highlights*

### Numbered lists

1.  First paragraph
2.  Second paragraph
3.  Third paragraph

    Continued paragraphs

# Iris data

![iris data](poster-r-example-figures/iris-1.pdf)

# Blub



[column] <!-- end of column -->

# Big figure



# Baz

\lipsum[6-7]

### Table

<!-- this is still latex :-) -->
\begin{table}
    \rowcolors{2}{kuldark!10}{kuldark!20}
    \begin{tabular}{lrrrllll}
            \rowcolor{kuldark!20}
                &     &                     &         &      &          &
                \multicolumn{2}{c}{\cellcolor{kuldark!20}Blub} \\
        Bla & Blub & Bla & Blub & Bla & Blub &
        Bla & Blub \\
        Bla & Blub & Bla & Blub & Bla & Blub & Blablublbaba & Blubblabalbal \\
        Blub & Bla & Blub & Bla & Blub & Bla & Blub & Bla \\
        Bla & Blub & Bla & Blub & Bla & Blub & Bla & Blub \\
        Blub & Bla & Blub & Bla & Blub & Bla & Blub & Bla \\
        Bla & Blub & Bla & Blub & Bla & Blub & Bla & Blub \\
        Blub & Bla & Blub & Bla & Blub & Bla & Blub & Bla \\
    \end{tabular}
    \caption{\lipsum[9]}
    \label{tab:blub}
\end{table}

\vskip0.4cm

[/columns] <!-- End of Section -->

[columns=2] <!-- New Section -->

[column=0.4]

# Conclusions

\lipsum[13]

[column]

# Bibliography

<!-- \printbibliography -->

\vskip4.4cm

[/columns]
