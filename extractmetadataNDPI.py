import pandas as pd
import numpy as np
import glob
from datetime import date
import openslide

inputfolder = ""
outputfolder = ""  
outputname = "metadata"

#list files in inputfolder
files = (glob.glob(inputfolder + "/*.ndpi"))

colID = ["filename","magnification", "width(px)", "height(px)", "mpp-x", "mpp-y"]
df = pd.DataFrame(columns= colID)

#read each file and extract metadata
for i in files:
    wsi_path2 = i
    t = openslide.open_slide(wsi_path2)
    width = (t.properties['openslide.level[0].width'])
    height = (t.properties['openslide.level[0].height'])
    Magn = (t.properties['hamamatsu.SourceLens'])
    mppY = (t.properties['openslide.mpp-y'])
    mppX = (t.properties['openslide.mpp-x'])
    filemetadata = [i, Magn, height, width, mppX, mppY]
    df.loc[i] = filemetadata
  
#create csv with metadata
df.to_csv((outputfolder + "/" + outputname + "_" + date.today().strftime("%Y%m%d") + ".csv"), index=False)