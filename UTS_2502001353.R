#--------------------Import necessary library

library(tidyverse)
library(dplyr)
library(ggpubr)
library(pastecs)
library(psych)
library(data.table)
library(ggdensity)
library(hrbrthemes)
library(mapdata)
library(maps)
library(usmap)
library(viridis)
library(shadowtext)

#-------------------- Import File
df <-read_csv('SampleSuperstore.csv')

#-------------------- Change data type accordingly
df$`Postal Code` <- as.character(df$`Postal Code`)
df$Discount <- df$Discount * 100


#-------------------- Drop unecessary column
# Because this Dataset is all about sales in US, There is no need to state the
# country
df = subset(df, select = -c(Country))


#-------------------- See if there is any missing value
colSums(is.na(df))


#-------------------- See if there is any duplicated row and count it
df_duplicated <- df
df_duplicated <- setDT(df_duplicated)[, list(Count = .N), names(df)]
nrow(df) - nrow(df_duplicated)

# Remove the temporary data frame
rm(df_duplicated)


#-------------------- Univariate Plot (Numerical)

#---------- Sales
# Figure 1.
hist(df$Sales, main = "Sales Data Histogram", breaks = seq(0, 25000, 1000), 
     xlab = "Sales (US$)", ylab = "Frequency", labels = TRUE,
     col = "#4FC1E8", ylim = c(0, 12000))


# Figure 2.
hist(df$Sales, main = "Sales Data Histogram", breaks = seq(0, 25000, 50), 
     xlab = "Sales (US$)", ylab = "Frequency", labels = TRUE,
     col = "#4FC1E8", xlim = c(0, 1000))
lines(density(df$Sales), lwd = 2)
  

# Figure 3.
ggplot(df, aes(x = Sales))+
  geom_density(color = "midnightblue", fill = "skyblue", alpha = 0.4)+
  scale_x_continuous(limits = c(0, 1000)) +
  ggtitle("Sales Data Density Plot") +
  geom_vline(aes(xintercept = mean(df$Sales)), color="blue",
             linetype="dashed", size=.8)+
  geom_vline(aes(xintercept = median(df$Sales)), color="red",
             linetype="dashed", size=.8)+
  xlab("Sales (US$)") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))


# Figure 4.
ggplot(df, aes(x = Sales)) +
  geom_boxplot(color = "midnightblue", fill = "skyblue", alpha = 0.4,
               outlier.color = "skyblue")+
  scale_x_continuous(breaks = seq(0, 25000, 1000))+
  ggtitle("Sales Data Box Plot") +
  xlab("Sales (US$)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Quantity
# Figure 5. 
hist(df$Quantity, main = "Quantity Data Histogram", 
     xlab = "Purchased Item Quantity", ylab = "Frequency", labels = TRUE,
     col = "#4FC879", breaks = seq(0, 14,1))


# Figure 6.
ggplot(df, aes(x = Quantity))+
  geom_density(color = "#046306", fill = "#4FC879", alpha = 0.4)+
  scale_x_continuous(breaks = seq(0, 14, 1)) +
  geom_vline(aes(xintercept = mean(Quantity)), color="blue",
             linetype="dashed", size=.8)+
  geom_vline(aes(xintercept = median(Quantity)), color="red",
             linetype="dashed", size=.8)+
  ggtitle("Quantity Data Density Plot") +
  xlab("Purchased Item Quantity") +
  theme_ipsum()+
  theme(plot.title = element_text(hjust = 0.5))


# Figure 7.
ggplot(df, aes(x = Quantity)) +
  geom_boxplot(color = "#046306", fill = "#4FC879", alpha = 0.4,
              outlier.colour = "#4FC879")+
  scale_x_continuous(breaks = seq(0, 14, 1)) +
  ggtitle("Quantity Data Box Plot") +
  xlab("Purchased Item Quantity")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Discount
# Figure 8. 
hist(df$Discount, main = "Discount Data Histogram", 
     xlab = "Purchased Item Discount", ylab = "Frequency", labels = TRUE,
     col = "#800080", breaks = seq(0, 80, 5)) 


# Figure 9.
ggplot(df, aes(x = Discount))+
  geom_density(color = "#800080", fill = "663046", alpha = 0.4)+
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  geom_vline(aes(xintercept = mean(Discount)), color="blue",
             linetype="dashed", size=.8)+
  geom_vline(aes(xintercept = median(Discount)), color="red",
             linetype="dashed", size=.8)+
  ggtitle("Discount Data Density Plot") +
  xlab("Purchased Item Discount") +
  theme_ipsum()+
  theme(plot.title = element_text(hjust = 0.5))


# Figure 10.
ggplot(df, aes(x = Discount)) +
  geom_boxplot(color = "#800080", fill = "663046", alpha = 0.4,
               outlier.colour = "#663046", outlier.alpha = 0.007)+
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  ggtitle("Discount Data Box Plot") +
  xlab("Purchased Item Discount")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Profit
# Figure 11.
hist(df$Profit, main = "Profit Data Histogram",
     breaks = seq(-10000, 10000, 500), 
     xlab = "Profit (US$)", ylab = "Frequency", labels = TRUE,
     col = "#FCAE1E")


# Figure 12.
hist(df$Profit, main = "Profit Data Histogram",
     breaks = seq(-10000, 10000, 25),
     xlab = "Profit (US$)", ylab = "Frequency", labels = TRUE,
     col = "#FCAE1E",  xlim = c(-500, 500))


# Figure 13.
ggplot(df, aes(x = Profit))+
  geom_density(color = "#D16002", fill = "#FCAE1E", alpha = 0.4)+
  scale_x_continuous(breaks = seq(-7000, 9000, 25), limits = c(-500, 500)) +
  geom_vline(aes(xintercept = mean(Profit)), color="blue",
             linetype="dashed", size=.8)+
  geom_vline(aes(xintercept = median(Profit)), color="red",
             linetype="dashed", size=.8)+
  ggtitle("Profit Data Density Plot") +
  xlab("Profit (US$)") +
  theme_ipsum()+
  theme(plot.title = element_text(hjust = 0.5))


# Figure 14.
ggplot(df, aes(x = Profit)) +
  geom_boxplot(color = "#D16002", fill = "#FCAE1E", alpha = 0.4,
               outlier.colour = "#FCAE1E")+
  scale_x_continuous(breaks = seq(-7000, 9000, 500))+
  ggtitle("Profit Data Box Plot") +
  xlab("Profit (US$)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


# Figure 15.
ggplot(df, aes(x = Profit)) +
  geom_boxplot(color = "#D16002", fill = "#FCAE1E", alpha = 0.4,
               outlier.colour = "#FCAE1E")+
  scale_x_continuous(breaks = seq(-7000, 9000, 20), limits = c(-500, 500))+
  ggtitle("Profit Data Box Plot") +
  xlab("Profit (US$)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#-------------------- Univariate Plot (Categorical)

#---------- Shipment Type
# Figure 16.
ship_mode <- plyr::count(df$`Ship Mode`)
ship_mode <- ship_mode %>%
  arrange(freq)
ship_mode$x <- factor(ship_mode$x, levels = ship_mode$x)


ggplot(ship_mode, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("Ship Mode Data Bar Plot") +
  xlab("Shipment Type")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Segment
# Figure 17.
segment <- plyr::count(df$Segment)
segment <-  segment %>%
  arrange(freq)

ggplot(segment, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("Customer Segment Data Bar Plot") +
  xlab("Customer Segment")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))


#---------- City
# Figure 18.
city <- plyr::count(df$City)
city <-  city %>%
  arrange(desc(freq)) %>%
  slice(1:15)
city$x <- factor(city$x, levels = city$x)

ggplot(city, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("City Data Bar Plotof the Top 15 Frequencies") +
  xlab("City")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()


#---------- State
# Figure 19.
state <- plyr::count(df$State)
state <-  state %>%
  arrange(freq)
state$x <- factor(state$x, levels = state$x)

ggplot(state, aes(x = fct_rev(fct_inorder(x)), y = freq,
                  fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_dodge (width = 1),
            hjust = -0.5, color = "black")+
  ggtitle("State Data Bar Plot") +
  xlab("State")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()


# Figure 20.
state <- state %>%
  rename("state" = "x")

plot_usmap(data = state, values = "freq", color = "#E3242B",
           exclude = c("AK", "HI"),labels = TRUE) +
  scale_fill_viridis(option = "plasma", direction = -1, alpha = .8)+
  labs(title = "States Data Map Plot",
       subtitle = "Excluding Alaska and Hawaii")+
  theme(plot.title = element_text(face="bold"))


#---------- Postal Code
# Figure 21.
pc <- plyr::count(df$`Postal Code`)
pc <-  pc %>%
  arrange(desc(freq)) %>%
  slice(1:15)
pc$x <- factor(pc$x, levels = pc$x)

ggplot(pc, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_dodge (width = 1),
            hjust = -0.5, color = "black")+
  ggtitle("Postal Code Data Bar Plot of the Top 15 Frequencies") +
  xlab("Postal Code")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()


#---------- Region
# Figure 22.
region <- plyr::count(df$Region)
region <-  region %>%
  arrange(freq)
region$x <- factor(region$x, levels = region$x)

ggplot(region, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("Region Data Bar Plot") +
  xlab("Region")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Category
# Figure 23.
category <- plyr::count(df$Category)
category <-  category %>%
  arrange(freq)
category$x <- factor(category$x, levels = category$x)

ggplot(category, aes(x = fct_inorder(x), y = freq, fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("Category Data Bar Plot") +
  xlab("Category")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Sub-Category
# Figure 24.
sub_category <- plyr::count(df$`Sub-Category`)
sub_category <-  sub_category %>%
  arrange(freq)
sub_category$x <- factor(sub_category$x, levels = sub_category$x)

ggplot(sub_category, aes(x = fct_rev(fct_inorder(x)), y = freq,
                         fill = x, colour = x))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = freq), size = 3,
            position = position_stack(vjust = 0.5), color = "black")+
  ggtitle("Sub-Category Data Bar Plot") +
  xlab("Sub-Category")+
  ylab("Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()


#-------------------- Bivariate Plot

#---------- Ship mode-Segment
# Figure 25.
ship_seg <- plyr::count(df[,c("Ship Mode", "Segment")])
ship_seg <- ship_seg %>%
  arrange(freq)
ship_seg$Ship.Mode <- factor(ship_seg$Ship.Mode, levels = ship_mode$x)
ship_seg$Segment <- factor(ship_seg$Segment, levels = segment$x)

ggplot(ship_seg, aes(x = fct_inorder(Segment), y = freq,
                     fill = Ship.Mode, colour = Ship.Mode))+
  geom_bar(stat = "identity", color = "black", position = "dodge")+
  geom_text(aes(label = freq), size = 3, position = position_dodge(.9),
            vjust = 1.5, color = "black")+
  ggtitle("Ship Mode Data Bar Plot Based on Segment") +
  xlab("Customer Segment")+
  ylab("Frequency")+
  labs(fill = "Shipment type")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Category-Segment
# Figure 26.
cat_seg <- plyr::count(df[,c("Category", "Segment")])
cat_seg <- cat_seg %>%
  arrange(freq)
cat_seg$Category <- factor(cat_seg$Category, levels = category$x)
cat_seg$Segment <- factor(cat_seg$Segment, levels = segment$x)

ggplot(cat_seg, aes(x = fct_inorder(Segment), y = freq,
                     fill = Category, colour = Category))+
  geom_bar(stat = "identity", color = "black", position = "dodge")+
  geom_text(aes(label = freq), size = 3, position = position_dodge(.9),
            vjust = 1.5, color = "black")+
  ggtitle("Customer Segment Data Bar Plot Based on Category") +
  xlab("Customer Segment")+
  ylab("Frequency")+
  labs(fill = "Item Category")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#---------- Profit-Sales
# Figure 27.
ggplot(df, aes(x = Sales, y = Profit)) +
  geom_point(size = 1.3, color = "#BF40BF", alpha = .4) +
  ggtitle("Scatter Plot of Profit and Sales Data")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_x_continuous(breaks = seq(0, 25000, 1000))+
  scale_y_continuous(breaks = seq(-7000, 9000, 1000))+
  xlab("Sales (US$)")+
  ylab("Profit (US$)")


#---------- Category-Sub-Category
# Figure 28.
cat_sub <- plyr::count(df[,c("Category", "Sub-Category")])
cat_sub <- cat_sub %>%
  arrange(freq)
cat_sub$Sub.Category <- factor(cat_sub$Sub.Category, levels = sub_category$x)
cat_sub$Category <- factor(cat_sub$Category, levels = category$x)

ggplot(cat_sub, aes(x = fct_rev(fct_inorder(Category)), y = freq,
                    fill = fct_rev(Sub.Category),
                    colour = fct_rev(Sub.Category)))+
  geom_bar(stat = "identity", color = "black", position = "dodge")+
  geom_text(aes(label = freq), size = 3, position = position_dodge(0.9),
            hjust = -.1, color = "black")+
  ggtitle("Sub-Category Bar Plot Based on Its Category") +
  xlab("Category")+
  ylab("Frequency")+
  labs(fill = "Sub-Category")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()

#---------- State-Sales
# Figure 29.
State_Sales <- df[,c("State", "Sales")]
State_Sales <- State_Sales %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Sales))%>%
  arrange(Total_Sales) %>%
  rename("state" = "State")
State_Sales$state <- factor(State_Sales$state, levels = State_Sales$state)

ggplot(State_Sales, aes(x = fct_rev(fct_inorder(state)), fill = Total_Sales,
                        color = Total_Sales, y = Total_Sales))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = Total_Sales), size = 3,
            position = position_dodge (width = 1),
            hjust = -0.1, color = "black")+
  scale_fill_viridis(option = "plasma", alpha = .8, direction = -1)+
  ggtitle("Sales in Each State Data Bar Plot") +
  xlab("State")+
  ylab("Total Sales (US$)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  labs(fill = "Total Sales (US$)")+
  coord_flip()

# Figure 30.
plot_usmap(data = State_Sales, values = "Total_Sales", color = "white",
           exclude = c("AK", "HI"),labels = TRUE) +
  scale_fill_viridis(option = "plasma", direction = -1, alpha = .8)+
  labs(title = "Sales in Each State Data Map Plot",
       subtitle = "Excluding Alaska and Hawaii", fill = "Total Sales (US$)")+
  theme(plot.title = element_text(face="bold"))

#---------- State-Profit
# Figure 31.
State_Profit <- df[,c("State", "Profit")]
State_Profit <- State_Profit %>%
  group_by(State) %>%
  summarise(Total_Profit = sum(Profit))%>%
  arrange(Total_Profit) %>%
  rename("state" = "State")
State_Profit$state <- factor(State_Profit$state, levels = State_Profit$state)

ggplot(State_Profit, aes(x = fct_rev(fct_inorder(state)), fill = Total_Profit,
                        color = Total_Profit, y = Total_Profit))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(aes(label = Total_Profit), size = 3,
            hjust = 0.5 - sign(State_Profit$Total_Profit)/1.9,
            position = position_dodge (width = 1), color = "black")+
  scale_fill_viridis(option = "plasma", alpha = .8, direction = -1)+
  ggtitle("Profit in Each State Data Bar Plot") +
  xlab("State")+
  ylab("Total Profit (US$)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  labs(fill = "Total Profit (US$)")+
  coord_flip()

# Figure 32.
plot_usmap(data = State_Profit, values = "Total_Profit", color = "white",
           exclude = c("AK", "HI"),labels = TRUE) +
  scale_fill_viridis(option = "plasma", direction = -1, alpha = .8)+
  labs(title = "Profit in Each State Data Map Plot",
       subtitle = "Excluding Alaska and Hawaii", fill = "Total Profit (US$)")+
  theme(plot.title = element_text(face="bold"))


#-------------------- Multivariate Graph

#---------- Heatmap of all Numeric Data
# Figure 33.
numericdf <- select_if(df, is.numeric)
corr <- round(cor(numericdf, method = "pearson"), digits = 2) %>%
  as.data.frame()
corr$cnames <- rownames(corr)
corr <- corr %>% pivot_longer(!cnames)


ggplot(corr,aes(x = cnames, y = name, fill = value)) +
  geom_tile(color = "white") +
  geom_shadowtext(aes(label = value), size = 5,
                  bg.color = "black", color = "white") +
  scale_fill_viridis(option = "plasma",
                     name = "Pearson's correlation coefficient") +
  ggtitle("Heatmap of the Numeric Data")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("")+
  ylab("")+
  coord_fixed()


#---------- Profit, Sales, Segment
# Figure 34.
ggplot(df, aes(x = Sales, y = Profit)) +
  geom_point(size = 1.3, alpha = .8, aes(fill = Segment,
                             color = Segment, shape = Segment)) +
  ggtitle("Scatter Plot of Profit and Sales Data
  Based on Customer Segment and Category")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  scale_x_continuous(breaks = seq(0, 25000, 2000))+
  scale_y_continuous(breaks = seq(-7000, 9000, 1000))+
  facet_wrap(~Category)+
  xlab("Sales (US$)")+
  ylab("Profit (US$)")

#---------- Segment, Sub-Category, Quantity
# Figure 35.
Segm_Sub <- df[, c("Segment", "Sub-Category", "Quantity")]
Segm_Sub <- Segm_Sub %>%
  group_by(Segment, `Sub-Category`) %>%
  summarise(Total_Item_Purchased = sum(Quantity))%>%
  arrange(Total_Item_Purchased)
Segm_Sub$`Sub-Category` <- factor(Segm_Sub$`Sub-Category`,
                                  levels = sub_category$x)

ggplot(Segm_Sub, aes(x = fct_rev(fct_inorder(Segment)),
                    y = Total_Item_Purchased,
                    fill = fct_rev(`Sub-Category`),
                    colour = fct_rev(`Sub-Category`)))+
  geom_bar(stat = "identity", color = "black", position = "dodge")+
  geom_text(aes(label = Total_Item_Purchased), size = 3,
            position = position_dodge(0.9),
            hjust = -.1, color = "black")+
  ggtitle("Quantity Data Bar Plot Based on Segment and Sub-Category") +
  xlab("Customer Segment")+
  ylab("Total Item Purchased")+
  labs(fill = "Sub-Category")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  coord_flip()


#-------------------- Summary Statistic
stat.desc(numericdf)
summary(numericdf)

head(count(df, Sales, sort = TRUE))
count(df, Quantity, sort = TRUE)
count(df, Discount, sort = TRUE)
head(count(df, Profit, sort = TRUE))