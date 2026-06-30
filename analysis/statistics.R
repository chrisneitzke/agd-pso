###############################################################################
#
# AGD-PSO Framework
#
# File:
#    statistics.R
#
# Description:
#    Descriptive statistics.
#
###############################################################################

generateStatistics <- function(summaryData){

    statistics <-

        summaryData |>

        group_by(

            Benchmark,
            Algorithm

        ) |>

        summarise(

            Runs = n(),

            MeanFitness =
                mean(Fitness),

            SDFitness =
                sd(Fitness),

            BestFitness =
                min(Fitness),

            MeanObjective =
                mean(Objective),

            SDObjective =
                sd(Objective),

            BestObjective =
                ifelse(

                    all(is.na(Objective)),

                    NA,

                    ifelse(

                        first(Benchmark) == "G01",

                        max(Objective),

                        min(Objective)

                    )

                ),

            MeanViolation =
                mean(Violation),

            SDViolation =
                sd(Violation),

            SuccessRate =
                100 * mean(Success),

            MeanTime =
                mean(Time),

            SDTime =
                sd(Time),

            .groups = "drop"

        )

    ###########################################################################
    # Rounding
    ###########################################################################

    statistics <-

        statistics |>

        mutate(

            MeanFitness =
                round(MeanFitness, 6),

            SDFitness =
                round(SDFitness, 6),

            BestFitness =
                round(BestFitness, 6),

            MeanObjective =
                round(MeanObjective, 6),

            SDObjective =
                round(SDObjective, 6),

            BestObjective =
                round(BestObjective, 6),

            MeanViolation =
                round(MeanViolation, 6),

            SDViolation =
                round(SDViolation, 6),

            SuccessRate =
                round(SuccessRate, 2),

            MeanTime =
                round(MeanTime, 3),

            SDTime =
                round(SDTime, 3)

        )

    ###########################################################################
    # Print
    ###########################################################################

    cat("\n")
    cat("=========================================================\n")
    cat("DESCRIPTIVE STATISTICS\n")
    cat("=========================================================\n\n")

    print(statistics)

    cat("\n")

    statistics

}