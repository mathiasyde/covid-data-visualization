**2025-12-12 Mathias**

I've added the ability to toggle the annotations.

In the renderServer function, there is an ``Annotations`` object that ``geom`` objects can be added to.

```r
Annotations <- reactive({
    list(
        Omicron = if (input$enableAnnotations) {
            # geom_foo()
        } else NULL,
   )
})

```

The annotation can be added to a plot like the following
```r
ggplot() +
    Annotations$Omicron
```