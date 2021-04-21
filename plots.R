
plots.change_province <- function(m.change.province,
                                folder="results/plots/",
                                width=10,
                                height=10){

  m.change.province %>%
    ungroup() %>%
    dplyr::select(province, change_Q1_rel, change_Q4_rel) %>%
    tidyr::gather("quarter","change", -province) %>%
    ungroup() %>%
    mutate(quarter=recode(quarter,
                          "change_Q1_rel"="2021Q1 Vs 2020Q1",
                          "change_Q4_rel"="2020Q4 Vs 2019Q4"
                          )) %>%
    mutate(quarter = as.factor(quarter),
           province = tidytext::reorder_within(province, change, quarter),
           label=paste0(round(change*100),"%")) %>%
  ggplot(aes(x=change,
             y=province)) +
    geom_col(aes(fill="1"), show.legend = F) +
    facet_wrap(~quarter, scales = "free_y") +
    tidytext::scale_y_reordered() +
    geom_text(aes(label=label,
                  hjust=ifelse(change>0,-0.1,1.1)),
              size=3) +
    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d()+
    labs(title="PM 2.5 level changes in Chinese provinces") +
    scale_x_continuous(labels=scales::percent,
                       limits=c(-0.35,0.6))


  ggsave(file.path(folder,"bar_change_province.png"), p1,
         width=width,
         height=height)

}



plots.hp_keyregion <- function(m.hp.keyregion,
                               folder="results/plots/",
                               width=8,
                               height=4,
                               ...){
  ggplot(m.hp.keyregion) +
    geom_bar(aes(quarter, round(count), fill="1"),
             stat="identity",
             show.legend = F) +
    facet_wrap(~keyregion2018, ...) +
    scale_y_continuous(
                       expand = expansion(mult=c(0,0.1),0)) +

    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d() +
    theme(axis.text.x = element_text(size=6)) +
    labs(title="Number of heavy pollution days in key regions",
         subtitle="Average number of heavy pollution days per city per quarter",
         caption="Source: CREA",
         x=NULL, y=NULL)

  ggsave("results/plots/hp_keyregion.png", width=width, height=height)
}



plots.sandstorm_keyregion <- function(m.storm.keyregion){
  ggplot(m.storm.keyregion) +
    geom_bar(aes(season, count, fill="1"),
             stat="identity",
             show.legend = F) +
    facet_wrap(~keyregion2018) +
    scale_y_continuous(limits=c(0,10),
                       breaks=seq(0,10,2),
                       expand = expansion(mult=c(0,0.1),0)) +

    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d() +
    labs(title="Occurence of sand storms in key regions",
         subtitle="Number of sand storm days in winter",
         caption="Source: CREA",
         x=NULL, y=NULL)

  ggsave("results/plots/storm_keyregion.png", width=8, height=4)
}
