#Particle Analysis inside every ROIs of an image#
[from](http://stackoverflow.com/questions/21292384/imagej-analyze-particles-in-different-roi-at-the-same-time)

```
id = getImageID();
setAutoThreshold("Default");
for (i=0 ; i<roiManager("count"); i++) {
     selectImage(id);
     roiManager("select", i);
     run("Analyze Particles...", "size=0-Infinity circularity=0.00-1.00 show=Masks clear display exclude include summarize");
                    }
```
