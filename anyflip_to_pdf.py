import sys
from PIL import Image
import os
import requests

if(len(sys.argv) < 5):
    raise Exception("Requires 4 inputs. The first two are the name of the particular directory in anyflip which is identified by two unique strings, e.g. https://anyflip.com/osliz/fcwg/ has osliz and fcwg, the third is the name of the pdf you wish to produce - without .pdf, and the fourth is the number of images - which should be numbered 1.jpg...n.jpg")

anyflip_dir = sys.argv[1]+"/"+sys.argv[2]
str_pdf_name = sys.argv[3]+".pdf"
n = int(sys.argv[4])
current_dir = os.getcwd()+"\\"

for x in range(n):
    url = "http://online.anyflip.com/"+anyflip_dir+"/files/mobile/"+ str(x+1) +".jpg"
    r = requests.get(url, allow_redirects=True)
    open(str(x+1)+".jpg", "wb").write(r.content)



images = [Image.open(current_dir+str(f+1)+'.jpg') for f in range(n)]

pdf_path = current_dir+str_pdf_name

images[0].save(pdf_path, "PDF", resolution=100.0, save_all=True, append_images=images[1:])