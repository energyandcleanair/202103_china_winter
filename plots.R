
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
