wbdata <- jsonlite::fromJSON(gzcon(url("https://github.com/cjbarrie/CS-ED/blob/main/data/web_historian_data.json?raw=true")))

wbdata_short <- wbdata %>% 
  select(domain, searchTerms, urlId)

wbdata %>% 
  group_by(domain) %>%
  count() 

wbdata %>% 
  group_by(domain) %>%
  count() %>%
  arrange(desc(n))

wbdata_mp <- wbdata %>% 
  group_by(domain) %>%
  count() %>%
  filter(n >5)

wbdata_mp %>%
  ggplot() +
  geom_bar(aes(domain, n), stat = "identity") +
  coord_flip()

