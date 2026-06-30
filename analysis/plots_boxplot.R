###############################################################################
#
# AGD-PSO Framework
#
# File:
#    plots_boxplot.R
#
# Description:
#    Boxplots of experimental results.
#
###############################################################################

plotBoxplots <- function(summaryData){

    cat("Generating boxplots...\n")

    benchmarks <-

        sort(

            unique(summaryData$Benchmark)

        )

    metrics <-

        c(

            "Fitness",

            "Objective",

            "Violation"

        )

    ###########################################################################
    # One figure for each benchmark and metric
    ###########################################################################

    for(benchmark in benchmarks){

        benchmarkData <-

            summaryData |>

            filter(

                Benchmark == benchmark

            )

        for(metric in metrics){

            ###################################################################
            # Plot
            ###################################################################

            p <-

                ggplot(

                    benchmarkData,

                    aes(

                        x = Algorithm,

                        y = .data[[metric]],

                        fill = Algorithm

                    )

                ) +

                geom_boxplot(

                    width = 0.65,

                    outlier.size = 2

                ) +

                geom_jitter(

                    width = 0.15,

                    alpha = 0.50,

                    size = 1.5

                ) +

                labs(

                    title =

                        paste(

                            benchmark,

                            "-",

                            metric

                        ),

                    x = "Algorithm",

                    y = metric

                ) +

                theme_bw(

                    base_size = 12

                ) +

                theme(

                    legend.position = "none"

                )

            ###################################################################
            # Save
            ###################################################################

            ggsave(

                filename =

                    file.path(

                        "output",

                        "figures",

                        paste0(

                            "boxplot_",

                            benchmark,

                            "_",

                            tolower(metric),

                            ".pdf"

                        )

                    ),

                plot = p,

                width = 6,

                height = 5

            )

        }

    }

}