library(fixedincome)
library(rb3)
library(bizdays)
library(dplyr)
library(ggplot2)

get_di1_curve <- function(refdate) {
  fut <- futures_get(refdate)
  yc <- yc_get(refdate)
  df <- yc_superset(yc, fut)
  
  df_curve <- bind_rows(
    df |> slice(1) |> select(biz_days, r_252),
    df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
  ) |>
    filter(!duplicated(biz_days))
  
  spotratecurve(
    df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = refdate
  )
}


dates <- bizseq("2022-08-01", "2022-08-16", "Brazil/ANBIMA")
curves <- lapply(seq_along(dates), function(ix) get_di1_curve(dates[ix]))

  {
    g <- autoplot(curves[[1]], curve.x.axis = "terms", colour = "red") +
      autolayer(curves[[1]], curve.geom = "point", curve.x.axis = "terms", colour = "red") +
      ylim(0.11, 0.14) +
      theme_bw() +
      theme(legend.position = "none") +
      labs(
        x = "Prazos", y = NULL, title = "Curvas de Juros Prefixados DI1",
        subtitle = "Entre as datas 2022-08-01 e 2022-08-16",
        caption = "Desenvolvido por wilsonfreitas / Fonte: B3"
      )
    print(g)
    
    for(curve in curves[-c(1, length(curves))]) {
      g <- g + autolayer(curve, curve.x.axis = "terms", colour = "grey") +
        autolayer(curve,
                  curve.geom = "point", curve.x.axis = "terms", colour = "grey"
        )
      print(g)
    }
    
    g <- g + autolayer(curves[[length(curves)]], curve.x.axis = "terms", colour = "blue") +
      autolayer(curves[[length(curves)]],
                curve.geom = "point", curve.x.axis = "terms", colour = "blue"
      )
    print(g)
}