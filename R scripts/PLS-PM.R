#plspm
sem=readxl::read_excel("plspm.xlsx")
sem.scale<-data.frame(scale(sem[-c(1:5)]))

sem$type<-gsub("MP","1",sem$type)
sem$type<-gsub("NP","0",sem$type)
sem$type<-as.numeric(sem$type)

# define vairable
comdata <- list(Temperature="Temperature" , 
                type = "type",
                diversity=c("div.taxa","div.func"), 
                stability = c("cohesion","verneralbility","robust.random","robustedness.tg"),  
                unique = c("indicator.taxa.rb","indicator.function.rb"),   
                unique.trans ="exchange.hgt")
##scaling
scaling1<-list( Temperature="NUM" , 
                type="NOMINAL",
                env = c("NUM","NUM","NUM","NUM","NUM","NUM","NUM"),  
                diversity=c("NUM","NUM"),  
                stability = c("NUM","NUM","NUM","NUM"),  
                unique = c("NUM","NUM"),   
                eco.role = c("NUM","NUM","NUM","NUM","NUM"),  
                unique.trans ="NUM")

# define latent_vars
latent_vars <- c("Temperature", "type",  "diversity","stability", "unique", "unique.trans")
# 将路径关系用公式列表表示
model_formulas <- list(  unique.trans= ~   Temperature+type +unique+stability+diversity, 
                         stability =~ Temperature + type+diversity,
                         unique =~  diversity+type+Temperature+stability,  
                         diversity =~  Temperature  + type)


# path
dat_path <- matrix(0, nrow = length(latent_vars), ncol = length(latent_vars),                   
                   dimnames = list(latent_vars, latent_vars))

# auto-fill in the path
for (end_var in names(model_formulas)) {  
  rhs_vars <- all.vars(model_formulas[[end_var]])  
  dat_path[end_var, rhs_vars] <- 1}

dat_path <- dat_path * lower.tri(dat_path)

print(dat_path)

library(plspm)
dat_modes <- rep("newA", 6)
dat_pls <- plspm(data.frame(sem[1:120,c(2,5)],sem.scale[1:120,]) , dat_path, comdata, modes = dat_modes,scaled = F,maxiter = 1000)
summary(dat_pls)
library(dplyr)
#plot
innerplot(dat_pls, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)


#plot effect size：
library(tidyr)
library(dplyr)
library(ggplot2)
dat_pls_effects <-dat_pls$effects
trad_effects <- dat_pls_effects %>%  filter(grepl("-> unique.trans$", relationships))
trad_effects <- trad_effects %>%  mutate(relationships = gsub(" -> unique.trans ", "", relationships))
trad_effects_long <- trad_effects %>%  pivot_longer(  cols = c(direct, indirect, total),    names_to = "effect_type",    values_to = "effect_size"  )

ggplot(trad_effects_long[which(trad_effects_long$effect_type!="total"),], aes(x = relationships, y = effect_size, fill = effect_type)) +  
  #geom_bar(stat = "identity", position = "dodge") +  
  geom_bar(stat = "identity", position = "stack") +  
  labs(    x = "Predictor",    y = "Effect Size (Standardized)",    title = "Effects on unique.trans "  ) +  # transmission
  scale_fill_manual(    values = c(      "direct" = "#EA7580FF",      "indirect" = "#088BBEFF",      "total" = "#F8CD9CFF"   ),    
                        labels = c(      "direct" = "Direct Effect",      "indirect" = "Indirect Effect",      "total" = "Total Effect"    )  ) +  
  theme_bw() +  theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank()) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14))+
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) 
