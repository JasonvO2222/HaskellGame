import os
import sys
from PIL import Image

def convertImage(file):
    img = Image.open("input/"+file)
    imagename = file.split(".", 1)[0]
    img.save("output/"+imagename+".bmp")


def iterateInput(directory):
        for file in os.listdir(directory):
            filename = os.fsdecode(file)
            if filename.endswith(".jpg") or filename.endswith(".png"):
                 convertImage(filename)
            else:
                 print("no can do: " + filename)


iterateInput("input/")












