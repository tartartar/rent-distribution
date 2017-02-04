library(rvest); library(ggplot2)

# Property to rent near King's Cross, all results (29th Jan 2017)
webpage <- read_html("http://www.zoopla.co.uk/to-rent/property/london/kings-cross/?include_shared_accommodation=false&price_frequency=per_month&q=Kings%20Cross%2C%20London&results_sort=newest_listings&search_source=home&page_size=500")

# getting prices. Element selected by using the following javascript:
# javascript:(function()%7Bvar%20s=document.createElement('div');s.innerHTML='Loading...';s.style.color='black';s.style.padding='20px';s.style.position='fixed';s.style.zIndex='9999';s.style.fontSize='3.0em';s.style.border='2px%20solid%20black';s.style.right='40px';s.style.top='40px';s.setAttribute('class','selector_gadget_loading');s.style.background='white';document.body.appendChild(s);s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src','https://dv0akt2986vzh.cloudfront.net/unstable/lib/selectorgadget.js');document.body.appendChild(s);%7D)();
prices <- webpage %>% html_nodes(".text-price") %>% html_text()

# getting addresses
addresses <- webpage %>% html_nodes(".listing-results-address") %>% html_text()

# getting type of property
type <- webpage %>% html_nodes("h2 a") %>% html_text()

clean.string <- NULL
for (i in 1:length(prices)) {
  clean.string <- append(clean.string, strsplit(prices, "£")[[i]][[2]])
}

prices <- as.numeric(gsub("[^0-9]", "", clean.string)); rm(clean.string); rm(i)

roads <- gsub("^\\s*", "", gsub("[0-9]*", "", gsub(",.*$", "", gsub("\n*(\\s)*$", "", gsub("^\n*(\\s)*", "", addresses))))); rm(addresses)

type <- gsub(" to rent", "", type)

# manual adjustment to remove extra result
prices <- prices[-1]
roads <- roads[-1]

properties <- data.frame(prices = prices,
                         roads = as.factor(roads),
                         type = as.factor(type))

rm(prices, roads, type, webpage)

head(properties)
# prices           roads               type
# 1   1950    Phoenix Road 1 bed flat to rent
# 2   1950   Knollys House 1 bed flat to rent
# 3    910  Milbrook Place     Studio to rent
# 4   2600      Gray's Inn     Studio to rent
# 5   4117 Gray's Inn Road 2 bed flat to rent
# 6   4117 Gray's Inn Road 2 bed flat to rent

saveRDS(properties, file = paste0(Sys.Date(), " King's Cross.rds"))

# properties <- readRDS(file = "2017-02-02 King's Cross.rds")

plot.h <- function(input) {
  ggplot(as.data.frame(input), aes(as.data.frame(input))) + 
    geom_histogram(binwidth = 0.1, fill = "black", col = "white") +
    scale_x_continuous("Price per month", 
                       breaks = c(seq(from = 0, to = max(input), by = 1500))) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
} 

new.prices <- log(properties$prices)

ggplot(as.data.frame(new.prices), aes(as.data.frame(new.prices))) + 
  geom_histogram(bins = 100, fill = "black", col = "white")

ggsave(plot, filename = paste0(Sys.Date(), " King's Cross.png"))

summary(properties$prices)[c(1, 4, 6)]
# Min.  Mean  Max. 
#  700  2120 22750 


#### density test ####
prop.subset <- properties[(properties$type == "1 bed flat" | properties$type == "2 bed duplex" | properties$type == "2 bed flat" | properties$type == "3 bed flat" | properties$type == "4 bed flat" | properties$type == "Studio") & properties$prices < 10000, ]

ggplot(prop.subset, aes(x = prices, fill = type)) +
  #geom_histogram(aes(y = ..density..), bins = 50) +
  #geom_density(col = "red")
  geom_histogram(aes(col = type), alpha = 0.3, binwidth = 100) +
  scale_x_continuous(breaks = c(seq(from = min(prop.subset$prices),
                                    to = max(prop.subset$prices), 
                                    by = 1000))) +
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0, 0)) +
  facet_wrap(~ type) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))