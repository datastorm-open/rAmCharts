## Changes in the library

### Sanity check

Since we need to reduce as much as possible the size of the package (for CRAN submissions). We've removed all unecessary files like README examples or non minified javascript files.

They were present in all plugins sub-directories, so you need to explore them:

```
inst/htmlwidgets/lib/plugins
|_ animate
|_ dataloader
|_ responsive
|_ export
    |_ libs
        |_ blob.js
        |_ fabric.js
        |_ FilsSaver.js
        |_ jszip
        |_ pdfmake
        |_ xlsx
```

### Amcharts link

Update the font size:

```javascript
{
    var a = "http://www." + a,
    e = c = 0,
    d = this.realWidth,
    f = this.realHeight,
    h = this.type;
if ("serial" == h || "xy" == h || "gantt" == h) c = this.marginLeftReal, e = this.marginTopReal, d = c + this.plotAreaWidth, f = e + this.plotAreaHeight;
var h = a + "/javascript-charts/",
    k = "JavaScript charts",
    l = "JS chart by amCharts";
"ammap" == this.product && (h = a + "/javascript-maps/", k = "Interactive JavaScript maps", l = "JS map by amCharts");
a = document.createElement("a");
l = document.createTextNode(l);
a.setAttribute("href", h);
a.setAttribute("title", k);
this.urlTarget && a.setAttribute("target", this.urlTarget);
a.appendChild(l);
this.chartDiv.appendChild(a);
this.amLink = a;
h = a.style;
h.position = "absolute";
h.textDecoration = "none";
h.color = this.color;
h.fontFamily = this.fontFamily;
h.fontSize = "8px"; // changed from "11px"
h.opacity = .7; 
h.display = "block";
var k = a.offsetWidth,
    a = a.offsetHeight,
    l = 5 + c,
    m = e + 5;
"bottom-left" == b && (l = 5 + c, m = f - a - 3);
"bottom-right" == b && (l = d - k - 5, m = f - a - 3);
"top-right" == b && (l = d - k - 5, m = e + 5);
h.left = l + "px";
h.top = m + "px"
}
```
