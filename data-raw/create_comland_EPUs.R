## Creates lazy data for EPUs

create_comland_EPUs <- function(){

  gom<-c(500, 510, 512:515)
  gb<-c(521:526, 551, 552, 561, 562)
  mab<-c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632)
  ss<-c(463:467, 511)

  EPUs <- list(GOM=gom, GB=gb,MAB=mab,SS=ss)

  usethis::use_data(EPUs)

}
