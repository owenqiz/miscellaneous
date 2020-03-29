# # http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
require(rvest)
require(dplyr)
require(rvest)

options(stringsAsFactors = FALSE)

url <- "https://forum.iask.ca/pages/COVID-19-TIMELINE/"
webpage <- read_html(url)

# tbls <- html_nodes(webpage, "table")

# check which table to use
# head(tbls)

tbls <- webpage %>%  html_nodes("table") %>% .[1] %>% html_table(fill = TRUE) %>% .[[1]]

colnames(tbls) <- c('region', 'confirmed', 'new_confirmed', 'death', 'new_death', 'death_rate',
                    'infected_per_million', 'tested',  'positive_rate', 'test_per_million',
                    'remainings', 'test_ability', 'test_period', 'recovered', 'active')

# removing icons
tbls <- data.frame(lapply(tbls, function(x) {gsub("\U0001f53c", "", x)}))
tbls$test_per_million <- gsub(',', '', tbls$test_per_million)

tbls$region <- recode(tbls$region, "加拿大" = 'Canada', '安省' = 'ON', "BC省" = 'BC', "阿省" = 'AB', '萨省' = 'SK',
       "魁省 \n                                        1" = 'QC', "曼省" = 'MB',
       "NS省" = 'NS', "NB省" = 'NB', "纽芬兰" = 'NL', "王子岛省" =  'PE', 
       "西北特区" = 'NT', "育空特区" = 'YT', "撤侨/其它" = 'Other')

colnames(tbls)

tbls <- tbls %>% select(-c(death_rate, positive_rate)) %>%
                 mutate_at(vars(-region), as.numeric) %>%
                 slice(-1)

