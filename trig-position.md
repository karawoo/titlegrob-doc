Testing position and rotation
================
Kara Woo
15 August, 2017

Placement of strip text is incorrect when the text is at angles other than 0, 90, 180, 270. Testing out some ways to improve this.

``` r
library("ggplot2")
library("grid")
```

``` r
display_tg <- function(label = "pineapple", ...) {
  tg <- ggplot2:::titleGrob(
                    label,
                    x = NULL,
                    y = NULL,
                    ...,
                    debug = TRUE
                  )
  
  grid.newpage()
  grid.rect()
  ## Draw tg in viewport that is smaller than the page (add_margins doesn't work
  ## for this)
  pushViewport(viewport(w = 0.8, h = 0.8, clip = "on"))
  grid.draw(tg)
}
```

Using [this commit](https://github.com/karawoo/ggplot2/commit/c655de255247feb2bcebb16c421eed37a6f4ca10):

``` r
sapply(c(0, 90, 180, 270), function(x) display_tg(hjust = 0, vjust = 0, angle = x))
```

<img src="figs/trig/right-angles-just-0-1.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-just-0-2.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-just-0-3.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-just-0-4.png" width="50%" style="display: block; margin: auto;" />

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL

``` r
sapply(c(45, 135, 225, 315), function(x) display_tg(hjust = 0, vjust = 0, angle = x))
```

<img src="figs/trig/intermediate-angles-just-0-1.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-just-0-2.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-just-0-3.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-just-0-4.png" width="50%" style="display: block; margin: auto;" />

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL

``` r
sapply(c(45, 135, 225, 315), function(x) display_tg(hjust = 0.5, vjust = 0.5, angle = x))
```

<img src="figs/trig/right-angles-1.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-2.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-3.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/right-angles-4.png" width="50%" style="display: block; margin: auto;" />

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL

``` r
sapply(c(45, 135, 225, 315), function(x) display_tg(hjust = 1, vjust = 1, angle = x))
```

<img src="figs/trig/intermediate-angles-1.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-2.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-3.png" width="50%" style="display: block; margin: auto;" /><img src="figs/trig/intermediate-angles-4.png" width="50%" style="display: block; margin: auto;" />

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL

Some sample plots:

``` r
df <- data.frame(
  x = 1:2,
  y = 1:2,
  z = c("a", "aaaaaaabc"),
  g = c("b", "bbbbbbbcd")
)

base <- ggplot(df, aes(x, y)) + 
  geom_point() +
  facet_grid(g ~ z)

base + 
  theme(
    strip.text.x = element_text(hjust = 0, debug = TRUE),
    strip.text.y = element_text(angle = 45, hjust = 0, debug = TRUE)
  )
```

<img src="figs/trig/actual-plots-1.png" width="80%" style="display: block; margin: auto;" />

``` r
base + 
  theme(
    strip.text.x = element_text(angle = 315, debug = TRUE),
    axis.text.x = element_text(angle = -90)
  )
```

<img src="figs/trig/actual-plots-2.png" width="80%" style="display: block; margin: auto;" />
