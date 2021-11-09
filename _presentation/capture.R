library('ggplot2')
library('rjags')
library('ggmcmc')

####################################################################################################
### model definition
####################################################################################################

model <- "
model{
  
  ## model 
  Y ~ dbinom(p, n)

  ## Prior distribution
  p ~ dbeta(1, 1)

}
"



data.list <- list( Y = 5,
              n = 20)


              
####################################################################################################
### Initial states of the MC
####################################################################################################
init.list <- list(p= 0.5 )


####################################################################################################
### Combine model data and intial values in jags
####################################################################################################
mjags <- jags.model(file = textConnection(model), 
                    data = data.list, 
                    inits = init.list, 
                    n.chains = 3)


####################################################################################################
## Sampling parameters
####################################################################################################
postSamples <- coda.samples(mjags, variable.names = c("p"), n.iter = 1000, thin = 10)  %>% ggs()



####################################################################################################
## Monitoring convergence
####################################################################################################
postSamples  %>% 
  ggplot() + geom_line(aes(x=Iteration, y = value, col = as.factor(Chain))) +
  ylim(c(0,1)) + labs(col = 'Chain', y = 'p') + 
  scale_color_manual(values= wesanderson::wes_palette('Darjeeling1'))


postSamples %>% filter(Parameter == 'p') %>% ggplot() + 
  geom_histogram(aes( x = value, fill = as.factor(Chain)), position = 'identity', alpha = 0.5) +
 labs(col = 'Chain', y = 'p')  + scale_fill_manual(values= wesanderson::wes_palette('Darjeeling1'))


postSamples %>% filter(Parameter == 'p') %>% ggplot() + facet_wrap(~ Chain) +
  geom_histogram(aes( x = value, fill = as.factor(Chain)), position = 'identity', alpha = 0.5) +
  labs(fill = 'Chain', x = 'p')   + scale_fill_manual(values= wesanderson::wes_palette('Darjeeling1'))



ggmcmc::ggmcmc(postSamples)


