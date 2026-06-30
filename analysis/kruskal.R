###############################################################################
#
# AGD-PSO Framework
#
# File:
#    kruskal.R
#
# Description:
#    Kruskal-Wallis statistical tests.
#
###############################################################################

runKruskal <- function(summaryData){

    benchmarks <-

        unique(summaryData$Benchmark)

    results <-

        data.frame(

            Benchmark = character(),

            Statistic = numeric(),

            DF = integer(),

            PValue = numeric(),

            Significant = logical(),

            stringsAsFactors = FALSE

        )

    ###########################################################################
    # Execute one test for each benchmark
    ###########################################################################

    for(benchmark in benchmarks){

        data <-

            subset(

                summaryData,

                Benchmark == benchmark

            )

        test <-

            kruskal.test(

                Fitness ~ Algorithm,

                data = data

            )

        results <-

            rbind(

                results,

                data.frame(

                    Benchmark = benchmark,

                    Statistic =
                        unname(test$statistic),

                    DF =
                        unname(test$parameter),

                    PValue =
                        test$p.value,

                    Significant =
                        test$p.value < 0.05

                )

            )

    }

    ###########################################################################
    # Formatting
    ###########################################################################

    results$Statistic <-

        round(

            results$Statistic,

            4

        )

    results$PValue <-

        signif(

            results$PValue,

            4

        )

    ###########################################################################
    # Print
    ###########################################################################

    cat("\n")
    cat("=========================================================\n")
    cat("KRUSKAL-WALLIS TEST\n")
    cat("=========================================================\n\n")

    print(results)

    cat("\n")

    return(results)

}