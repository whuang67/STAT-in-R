library(ggplot2)
library(gridExtra)
library(dplyr)
kran <- read.csv("C:/users/whuang67/downloads/new folder/Cleaned_Ticket_Redemption_Data.csv", header = TRUE)


abc <- kran %>% group_by(PriceType, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(PriceType) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(PriceType) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))

abc$PriceType <- factor(abc$PriceType, levels = c("YT", "SA", "SC", "SU",
                                                    "UI", "SP", "C", "P"))
abc1$PriceType <- factor(abc1$PriceType, levels = c("YT", "SA", "SC", "SU",
                                                    "UI", "SP", "C", "P"))
abc2$PriceType <- factor(abc2$PriceType, levels = c("YT", "SA", "SC", "SU",
                                                    "UI", "SP", "C", "P"))

plot1_PriceType <- ggplot() +
  geom_bar(mapping = aes(x = PriceType, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",
           color = "white") +
  geom_text(mapping = aes(x = PriceType, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("PriceType") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Price Type")


plot2_PriceType <- ggplot() +
  geom_bar(mapping = aes(x = PriceType, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = PriceType, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Price Type")

g <- ggplotGrob(plot1_PriceType + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2_PriceType, plot1_PriceType, legend, ncol = 1, nrow =3, heights = c(7/15, 7/15, 1/15))

#_______________________________________________________________________________________________________

abc <- kran %>% group_by(Theatre, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(Theatre) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(Theatre) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))

abc$Theatre <- factor(abc$Theatre, levels = c("Tryon Festival Theatre", "Colwell Playhouse",
                                                  "FGH Stage, Salon Style", "Foellinger Great Hall",
                                                  "Studio Theatre", "Krannert Room"))
abc1$Theatre <- factor(abc1$Theatre, levels = c("Tryon Festival Theatre", "Colwell Playhouse",
                                                    "FGH Stage, Salon Style", "Foellinger Great Hall",
                                                    "Studio Theatre", "Krannert Room"))
abc2$Theatre <- factor(abc2$Theatre, levels = c("Tryon Festival Theatre", "Colwell Playhouse",
                                                    "FGH Stage, Salon Style", "Foellinger Great Hall",
                                                    "Studio Theatre", "Krannert Room"))

levels(abc$Theatre)
plot1_Theatre <- ggplot() +
  geom_bar(mapping = aes(x = Theatre, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Theatre, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("Theatre") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Theatre")


plot2_Theatre <- ggplot() +
  geom_bar(mapping = aes(x = Theatre, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Theatre, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Theatre")

g <- ggplotGrob(plot1_Theatre + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2_Theatre, plot1_Theatre, legend, ncol = 1, nrow =3, heights = c(7/15, 7/15, 1/15))


#______________________________________________________________________________________________________

abc <- kran %>% group_by(Disposition, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(Disposition) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(Disposition) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))


abc$Disposition <- factor(abc$Disposition, levels = c("M (Mailed)", "PAH (Print at Home)",
                                                      "C (Counter)", "MD1 (Mobile Delivery - pdf only)",
                                                      "MD2 (Mobile Delivery - pdf + passbook)",
                                                      "W (Will Call)", "NA"))
abc1$Disposition <- factor(abc1$Disposition, levels = c("M (Mailed)", "PAH (Print at Home)",
                                                        "C (Counter)", "MD1 (Mobile Delivery - pdf only)",
                                                        "MD2 (Mobile Delivery - pdf + passbook)",
                                                        "W (Will Call)", "NA"))
abc2$Disposition <- factor(abc2$Disposition, levels = c("M (Mailed)", "PAH (Print at Home)",
                                                        "C (Counter)", "MD1 (Mobile Delivery - pdf only)",
                                                        "MD2 (Mobile Delivery - pdf + passbook)",
                                                        "W (Will Call)", "NA"))
levels(abc$Disposition)

plot1_Disposition <- ggplot() +
  geom_bar(mapping = aes(x = Disposition, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Disposition, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("Disposition") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Disposition")


plot2_Disposition <- ggplot() +
  geom_bar(mapping = aes(x = Disposition, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Disposition, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Disposition")

g <- ggplotGrob(plot1_Disposition + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2_Disposition, plot1_Disposition, legend, ncol = 1, nrow =3, heights = c(7/15, 7/15, 1/15))
#___________________________________________________________________________________________________________

abc <- kran %>% group_by(PurchasedOnline, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(PurchasedOnline) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(PurchasedOnline) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))

plot1_PurchasedOnline <- ggplot() +
  geom_bar(mapping = aes(x = PurchasedOnline, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",color = "white",
           width = 0.3) +
  geom_text(mapping = aes(x = PurchasedOnline, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("PurchasedOnline") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Purchased Online")


plot2_PurchasedOnline <- ggplot() +
  geom_bar(mapping = aes(x = PurchasedOnline, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",
           width = 0.3,color = "white") +
  geom_text(mapping = aes(x = PurchasedOnline, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Purchased Online")

g <- ggplotGrob(plot1_PurchasedOnline + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2_PurchasedOnline, plot1_PurchasedOnline, legend, ncol = 1, nrow =3, heights = c(7/15, 7/15, 1/15))
#_______________________________________________________________________________________________________________


abc <- kran %>% group_by(Producer, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(Producer) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(Producer) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))

abc$Producer <- factor(abc$Producer, levels = c("Lyric Theatre at Illinois",
                                                "Dance at Illinois",
                                                "Marquee", "Champaign Urbana Symphony Orchestra",
                                                "Illinois Theatre",
                                                "Sinfonia da Camera",
                                                "School of Music"))
abc1$Producer <- factor(abc1$Producer, levels = c("Lyric Theatre at Illinois",
                                                  "Dance at Illinois",
                                                  "Marquee", "Champaign Urbana Symphony Orchestra",
                                                  "Illinois Theatre",
                                                  "Sinfonia da Camera",
                                                  "School of Music"))
abc2$Producer <- factor(abc2$Producer, levels = c("Lyric Theatre at Illinois",
                                                  "Dance at Illinois",
                                                  "Marquee", "Champaign Urbana Symphony Orchestra",
                                                  "Illinois Theatre",
                                                  "Sinfonia da Camera",
                                                  "School of Music"))
levels(abc$Producer)


plot1_Producer <- ggplot() +
  geom_bar(mapping = aes(x = Producer, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Producer, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("Producer") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Producer")


plot2_Producer <- ggplot() +
  geom_bar(mapping = aes(x = Producer, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Producer, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Producer")

g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6/15, 8/15, 1/15))
#______________________________________________________________________________________________________________


abc <- kran %>% group_by(Category, Redeemed) %>% summarize(Total = n())
abc1 <- abc %>% group_by(Category) %>% summarise(Total1 = sum(Total))
abc2 <- abc %>% group_by(Category) %>% mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4))


abc$Category <- factor(abc$Category, levels = c("Dance", "Family", "Opera",
                                                "Orchestra/Soloist", "Chamber Group/Soloist",
                                                "Performance Art", "Theatre",
                                                "Music-Contemporary", "Music-Instrumental",
                                                "Music-Choral",
                                                "Dessert and Conversation"))
abc1$Category <- factor(abc1$Category, levels = c("Dance", "Family", "Opera",
                                                  "Orchestra/Soloist", "Chamber Group/Soloist",
                                                  "Performance Art", "Theatre",
                                                  "Music-Contemporary", "Music-Instrumental",
                                                  "Music-Choral",
                                                  "Dessert and Conversation"))
abc2$Category <- factor(abc2$Category, levels = c("Dance", "Family", "Opera",
                                                  "Orchestra/Soloist", "Chamber Group/Soloist",
                                                  "Performance Art", "Theatre",
                                                  "Music-Contemporary", "Music-Instrumental",
                                                  "Music-Choral",
                                                  "Dessert and Conversation"))
levels(kran$Category)

plot1_Category <- ggplot() +
  geom_bar(mapping = aes(x = Category, y = Total, fill = as.factor(Redeemed)),
           data =abc,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Category, y = ifelse(Total1 < 5000, Total1+ 2000, 0.47*Total1), label = Total1),
            data = abc1) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Count") + xlab("Category") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -17),
        axis.title.x = element_blank()) +
  ggtitle("Category")


plot2_Category <- ggplot() +
  geom_bar(mapping = aes(x = Category, y = Percentage, fill = as.factor(Redeemed)),
           data = abc2,
           stat = "identity",color = "white") +
  geom_text(mapping = aes(x = Category, y = Percentage, label = paste0(Percentage*100, "%"),
                          fill = as.factor(Redeemed)),
            data =abc2,
            position = position_stack(vjust = 0.5),
            angle = -30) +
  scale_fill_discrete(name="Redeemed", labels = c("No", "Yes")) +
  ylab("Percentage") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -17),
        axis.title.x = element_blank()) +
  ggtitle("Category")

g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))


#__________________________________Ticket Sold_______________________________________________________________

data = kran

tmp <- data %>% 
  select(TicketSold ,Redeemed) %>% 
  mutate(TicketSold = cut(TicketSold, breaks = quantile(TicketSold, probs = seq(0, 1, by = 1/6)), include.lowest = TRUE)) %>% 
  count(TicketSold, Redeemed)
levels(tmp$TicketSold) <- c("[9,268]","(268,459]","(459,734]","(734,875]","(875,1247]","(1247,2024]")
totals <- tmp %>% 
  group_by(TicketSold) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(TicketSold) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4))

plot1 <- ggplot(aes(x = TicketSold,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(TicketSold, 400+total, label = total, fill = NULL), data = totals) +
  geom_text(aes(x = TicketSold,
                y= n, label = paste0(percentage*100, "%"),
                fill = as.factor(Redeemed)),
            position = position_stack(vjust = 0.5), data = percentage) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("Ticket Sold") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = TicketSold,
                y = n,
                group = as.factor(Redeemed)),
            data = tmp[which(tmp$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = TicketSold,
                 y = n,
                 group = as.factor(Redeemed)),
             data = tmp[which(tmp$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Ticket Sold")



g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob

legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
grid.arrange(plot1, legend, ncol = 1, nrow =2, heights = c(14/15, 1/15))


#_________________ Ticket Sold plot2
plot1 <- ggplot(aes(x = TicketSold,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(TicketSold, total + 2000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("TicketSold") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Ticket Sold")

plot2 <- ggplot(aes(y = percentage, x = TicketSold,label = paste0(percentage*100,"%"),
                    fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity", color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("TicketSold") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = TicketSold,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = TicketSold,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Ticket Sold")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))


#__________________________________Capacity_______________________________________________________________

tmp <- data %>% 
  select(Capacity ,Redeemed) %>% 
  mutate(Capacity = cut(Capacity, breaks = quantile(Capacity, probs = seq(0, 1, by = 1/6)), include.lowest = TRUE)) %>% 
  count(Capacity, Redeemed)
totals <- tmp %>% 
  group_by(Capacity) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(Capacity) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)

plot1 <- ggplot(aes(x = Capacity,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(Capacity, total + 400, label = total, fill = NULL), data = totals) +
  geom_text(aes(Capacity, label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), data = percentage) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("Capacity") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = Capacity,
                y = n,
                group = as.factor(Redeemed)),
            data = tmp[which(tmp$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = Capacity,
                 y = n,
                 group = as.factor(Redeemed)),
             data = tmp[which(tmp$Redeemed == 1),],
             shape = 16,
             size = 1.5)

g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot1, legend, ncol = 1, nrow =2, heights = c(14/15, 1/15))


#_________________ Capacity 2
plot1 <- ggplot(aes(x = Capacity,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(Capacity, total + 2000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("Capacity") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Capacity")

plot2 <- ggplot(aes(y = percentage, x = Capacity,label = paste0(percentage,"%"),
                    fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity", color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("Capacity") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = Capacity,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = Capacity,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Capacity")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))


#__________________________________Price_______________________________________________________________

tmp <- data %>% 
  select(Price ,Redeemed) %>%   
  mutate(Price = cut(Price, breaks = quantile(Price,prob=seq(0,1,1/4)), include.lowest = TRUE)) %>%
  # mutate(Price = cut(Price, breaks = c(-1,unique(quantile(Price[Price>0],prob=seq(0,1,1/3)))), include.lowest = TRUE)) %>%
  count(Price, Redeemed)
totals <- tmp %>% 
  group_by(Price) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(Price) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)

plot1 <- ggplot(aes(x = Price,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white", width = 0.5) +
  geom_text(aes(Price, total + 2000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("Price") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Price")

plot2 <- ggplot(aes(y = percentage, x = Price,label = paste0(percentage,"%"),
                    fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity", width = 0.5,color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("Price") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = Price,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = Price,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Price")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))

#__________________________________Days in Between _______________________________________________________________


tmp <- data %>% 
  select(DaysBetween ,Redeemed) %>% 
  na.omit() %>%
  mutate(DaysBetween = cut(DaysBetween, breaks = quantile(DaysBetween, probs = seq(0, 1, by = 1/5)), include.lowest = TRUE)) %>%
  # mutate(DaysBetween = cut(DaysBetween, breaks = c(-1,0:100,Inf), include.lowest = TRUE)) %>%
  count(DaysBetween, Redeemed)
totals <- tmp %>% 
  group_by(DaysBetween) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(DaysBetween) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)

plot1 <- ggplot(aes(x = DaysBetween,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(DaysBetween, total + 1000, label = total, fill = NULL), data = totals) +
  geom_text(size = 3,
            mapping = aes(y = n,
                          x = DaysBetween,
                          label = paste0(percentage,"%")),
            position = position_stack(vjust = 0.5),
            data = percentage)+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("Days Between") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = DaysBetween,
                y = n,
                group = as.factor(Redeemed)),
            data = tmp[which(tmp$Redeemed == 1),],
            linetype = "dashed",
            size = 1) + 
  geom_point(aes(x = DaysBetween,
                 y = n,
                 group = as.factor(Redeemed)),
             data = tmp[which(tmp$Redeemed == 1),],
             shape = 16,
             size = 2)


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot1, legend, ncol = 1, nrow =2, heights = c(14/15, 1/15))

#_________________ Days in Between 2
plot1 <- ggplot(aes(x = DaysBetween,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(DaysBetween, total + 2000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("DaysBetween") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Days in Between")

plot2 <- ggplot(aes(y = percentage, x = DaysBetween,label = paste0(percentage,"%"),
                    fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity", color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("DaysBetween") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = DaysBetween,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = DaysBetween,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Days in Between")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))



#__________________________________Quantity Purchased_______________________________________________________________

tmp <- data %>% 
  select(QuantityPurchased ,Redeemed) %>% 
  mutate(QuantityPurchased = cut(QuantityPurchased, breaks = c(0:10,Inf), include.lowest = TRUE)) %>%
  count(QuantityPurchased, Redeemed)
levels(tmp$QuantityPurchased) <- c("1","2","3","4","5","6","7","8","9",'10',">10")
totals <- tmp %>% 
  group_by(QuantityPurchased) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(QuantityPurchased) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)

plot1 <- ggplot(aes(x = QuantityPurchased,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(size = 2.5,aes(QuantityPurchased, total + 1000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("QuantityPurchased") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Quantity Purchased")

plot2 <- ggplot(aes(y = percentage, x = QuantityPurchased,label = paste0(percentage,"%"), fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity",color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("Quantity Purchased") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = QuantityPurchased,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = QuantityPurchased,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Quantity Purchased")

g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(7/15, 7/15, 1/15))

#__________________________________Quantity Purchased Total_______________________________________________________________

data = kran
tmp <- data %>% 
  select(QuantityPurchasedTotal ,Redeemed) %>% 
  mutate(QuantityPurchasedTotal = cut(QuantityPurchasedTotal, breaks = c(0:10,Inf), include.lowest = TRUE)) %>%
  count(QuantityPurchasedTotal, Redeemed)
levels(tmp$QuantityPurchasedTotal) <- c("1","2","3","4","5","6","7","8","9",'10',">10")
totals <- tmp %>% 
  group_by(QuantityPurchasedTotal) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(QuantityPurchasedTotal) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)

plot1 <- ggplot(aes(x = QuantityPurchasedTotal,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(QuantityPurchasedTotal, total + 400, label = total, fill = NULL), data = totals) +
  geom_text(aes(QuantityPurchasedTotal, label = paste0(percentage,"%")),
            position = position_stack(vjust = 0.5),
            data = percentage) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("QuantityPurchasedTotal") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = QuantityPurchasedTotal,
                y = n,
                group = as.factor(Redeemed)),
            data = tmp[which(tmp$Redeemed == 1),],
            linetype = "dashed",
            size = 1) + 
  geom_point(aes(x = QuantityPurchasedTotal,
                 y = n,
                 group = as.factor(Redeemed)),
             data = tmp[which(tmp$Redeemed == 1),],
             shape = 16,
             size = 2)


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot1, legend, ncol = 1, nrow =2, heights = c(14/15, 1/15))


#_________________ QuantityPurchasedTotal 2
library(ggplot2)
plot1 <- ggplot(aes(x = QuantityPurchasedTotal,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(QuantityPurchasedTotal, total + 2000, label = total, fill = NULL), data = totals) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("QuantityPurchasedTotal") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Quantity Purchased Total")

plot2 <- ggplot(aes(y = percentage, x = QuantityPurchasedTotal,label = paste0(percentage,"%"),
                    fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity", color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("QuantityPurchasedTotal") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = QuantityPurchasedTotal,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = QuantityPurchasedTotal,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Quantity Purchased Total")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))

#__________________________________Event Purchased Total_______________________________________________________________


tmp <- data %>% 
  select(EventPurchasedTotal ,Redeemed) %>% 
  mutate(EventPurchasedTotal = cut(EventPurchasedTotal,
                                   breaks = quantile(EventPurchasedTotal,
                                                     probs = c(0,
                                                               17565/51863,
                                                               27032/51863,
                                                               32544/51863,
                                                               35942/51863,
                                                               38616/51863,
                                                               1)),
                                   include.lowest = TRUE)) %>%
  count(EventPurchasedTotal, Redeemed)
levels(tmp$EventPurchasedTotal) <- c("1", "2", "3", "4", "5", ">5")
totals <- tmp %>% 
  group_by(EventPurchasedTotal) %>% 
  summarize(total = sum(n))
percentage <- tmp %>% 
  group_by(EventPurchasedTotal) %>% 
  mutate(total = sum(n), percentage = round(n/sum(n),4)*100)


plot1 <- ggplot(aes(x = EventPurchasedTotal,y=n, fill = as.factor(Redeemed)), data = tmp) + 
  geom_bar(stat='identity',color = "white") +
  geom_text(aes(EventPurchasedTotal, total + 1400, label = total, fill = NULL), data = totals) +
#   geom_text(aes(EventPurchasedTotal, label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), data = percentage) +
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes")) + ylab("Count") + xlab("EventPurchasedTotal") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  ggtitle("Event Purchased Total")

plot2 <- ggplot(aes(y = percentage, x = EventPurchasedTotal,
                    label = paste0(percentage,"%"), fill = as.factor(Redeemed)), data = percentage) + 
  geom_bar(stat="identity",color = "white")+ 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_discrete(name="Redeemed",labels = c("No", "Yes"))+ylab("Percentage") + xlab("Quantity Purchased") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -15),
        axis.title.x = element_blank()) +
  geom_line(aes(x = EventPurchasedTotal,
                y = percentage,
                group = as.factor(Redeemed)),
            data = percentage[which(percentage$Redeemed == 1),],
            linetype = "dashed") + 
  geom_point(aes(x = EventPurchasedTotal,
                 y = percentage,
                 group = as.factor(Redeemed)),
             data = percentage[which(percentage$Redeemed == 1),],
             shape = 16,
             size = 1.5) +
  ggtitle("Event Purchased Total")


g <- ggplotGrob(plot1 + theme(legend.position = "bottom"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]


grid.arrange(plot2, plot1, legend, ncol = 1, nrow =3, heights = c(6.3/15, 7.7/15, 1/15))


#__________________________________________________________________________________________________


ggplot(data = kran) +
  geom_histogram(mapping = aes(x = EventPurchasedTotal),
                 stat = "count")




grid.arrange(plot1_PriceType + ggtitle("Price Type"),
             plot1_Theatre + ggtitle("Theater"),
             plot1_Disposition + ggtitle("Disposition"),
             plot1_PurchasedOnline + ggtitle("Purchased Online"),
             plot1_Producer + ggtitle("Producer"),
             plot1_Category + ggtitle("Category"),
             legend,
             ncol = 2, nrow =4,
             heights = c(5/16, 5/16, 5/16, 1/16), widths = c(1/2, 1/2))

grid.arrange(plot2_PriceType + ggtitle("Price Type"),
             plot2_Theatre + ggtitle("Theater"),
             plot2_Disposition + ggtitle("Disposition"),
             plot2_PurchasedOnline + ggtitle("Purchased Online"),
             plot2_Producer + ggtitle("Producer"),
             plot2_Category + ggtitle("Category"),
             legend,
             ncol = 2, nrow =4,
             heights = c(5/16, 5/16, 5/16, 1/16), widths = c(1/2, 1/2))
