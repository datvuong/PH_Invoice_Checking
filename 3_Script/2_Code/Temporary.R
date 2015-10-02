library(ggplot2)

varianceGroup <- Final %>% 
        group_by(varianceGroup=ifelse(Variance==0,"Equal",
                                        ifelse(Variance>0,"LazadaHigher",
                                               "3PLInvoiceHigher"))) %>%
        summarize(count=n())

ggplot(varianceGroup, aes(varianceGroup,count, width=.5)) +
        geom_bar(stat = 'identity', fill="#ff9494") + coord_flip() +
        ggtitle("Variance Group")

lazadaHigher <- filter(Final, Variance>0)

rateCardSummary <- lazadaHigher %>% group_by(RateCard) %>%
        summarize(count=n())

ggplot(rateCardSummary, aes(RateCard,count)) +
        geom_bar(stat = 'identity', fill="#ff9494") + coord_flip() +
        ggtitle("Lazada Higher breakdown by Ratecard")
