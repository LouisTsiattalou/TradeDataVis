library("maptools")
library("maps")
library("ggmap")
mapWorld <- map_data("world")

# Get map_data World names from iso codes using iso.expand
portsum_countries <- iso.expand(unique(portsum$source))

# Special Case - add serbia if XS is used - iso.expand only considers RS as Serbia
if ("XS" %in% unique(portsum$source)) {portsum_countries = c(portsum_countries, "Serbia")}
portsum_countries <- tibble(portsum_countries, iso.alpha(portsum_countries))
colnames(portsum_countries) <- c("name","code")
portsum_countries[portsum_countries$name == "Serbia","code"] = "XS"

# Aggregate by country
portsum_countrytotal <- portsum[,c("source","value")] %>% group_by(source) %>% summarise(value = sum(value))
# Match plot-compatible names to iso codes
portsum_countrytotal <- left_join(portsum_countrytotal,portsum_countries, by=c("source" = "code"))
# Join values to mapWorld for plotting
portsum_countrytotal <- tibble(portsum_countrytotal$name,portsum_countrytotal$value)
colnames(portsum_countrytotal) <- c("region","value")
mapWorld <- left_join(mapWorld,portsum_countrytotal)

#plot all states with ggplot
p <- ggplot()
p <- ggplot(data = mapWorld, aes(x=long,y=lat,group=group))
p <- p + geom_polygon(colour="grey20", fill="grey70")
p <- p + geom_polygon(aes(fill=value), colour = "grey20")
p <- p + scale_fill_gradient(trans = "log10")
p <- p + coord_fixed(1.3)
p
