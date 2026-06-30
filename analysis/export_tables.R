###############################################################################
#
# AGD-PSO Framework
#
# File:
#    export_tables.R
#
# Description:
#    Export analysis tables.
#
###############################################################################

exportTables <- function(

    statisticsTable,
    kruskalResults

){

    outputDir <- "output/tables"

    ###########################################################################
    # Statistics
    ###########################################################################

    write.csv(

        statisticsTable,

        file.path(

            outputDir,

            "statistics.csv"

        ),

        row.names = FALSE

    )

    ###########################################################################
    # Kruskal
    ###########################################################################

    write.csv(

        kruskalResults,

        file.path(

            outputDir,

            "kruskal.csv"

        ),

        row.names = FALSE

    )

    ###########################################################################
    # Markdown
    ###########################################################################

    exportMarkdown(

        statisticsTable,

        file.path(

            outputDir,

            "statistics.md"

        )

    )

    exportMarkdown(

        kruskalResults,

        file.path(

            outputDir,

            "kruskal.md"

        )

    )

    ###########################################################################

    cat(

        "Tables exported to:\n",

        normalizePath(outputDir),

        "\n\n"

    )

}

###############################################################################
#
# Markdown exporter
#
###############################################################################

exportMarkdown <- function(

    table,

    file

){

    header <-

        paste(

            names(table),

            collapse = " | "

        )

    separator <-

        paste(

            rep("---", ncol(table)),

            collapse = " | "

        )

    lines <- c(

        paste("|", header, "|"),

        paste("|", separator, "|")

    )

    for(i in seq_len(nrow(table))){

        row <-

            paste(

                table[i, ],

                collapse = " | "

            )

        lines <-

            c(

                lines,

                paste("|", row, "|")

            )

    }

    writeLines(

        lines,

        file

    )

}