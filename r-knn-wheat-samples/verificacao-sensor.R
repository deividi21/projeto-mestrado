library(tidyverse)

####Importando Dados####
leitura_11_07 <- read_delim("dados/teste_leitura_continua_11_07.csv",";", escape_double = FALSE, trim_ws = TRUE)

leitura_11_07.n <- leitura_11_07[20:37]

leitura_11_07.n <- mutate(leitura_11_07.n, id = leitura_11_07$`Sample,`)

leitura_11_07.n <- leitura_11_07.n %>% rename(R_610 = 'Cal. R (610nm)',
                                          S_680 = 'Cal. S (680nm)',
                                          T_730 = 'Cal. T (730nm)',
                                          U_760 = 'Cal. U (760nm)',
                                          V_810 = 'Cal. V (810nm)',
                                          W_860 = 'Cal. W (860nm)',
                                          G_560 = 'Cal. G (560nm)',
                                          H_585 = 'Cal. H (585nm)',
                                          I_645 = 'Cal. I (645nm)',
                                          J_705 = 'Cal. J (705nm)',
                                          K_900 = 'Cal. K (900nm)',
                                          L_940 = 'Cal. L (940nm)',
                                          A_410 = 'Cal. A (410nm)',
                                          B_435 = 'Cal. B (435nm)',
                                          C_460 = 'Cal. C (460nm)',
                                          D_485 = 'Cal. D (485nm)',
                                          E_510 = 'Cal. E (510nm)',
                                          F_535 = 'Cal. F (535nm)')




ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = R_610)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = T_730)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = V_810)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = W_860)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = K_900)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = A_410))
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = B_435))
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = D_485))


ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = C_460))
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = S_680)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = U_760)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = G_560)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = H_585)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = I_645)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = J_705)) 
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = E_510))
ggplot(leitura_11_07.n, aes(x=id)) + 
  geom_line(aes(y = F_535))



       