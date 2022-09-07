# Small-Programs
Mostly fun programs, either implementing interesting algorithms or performing simple tasks  

Maze.c / Maze.exe : Using randomized Kruskal's algorithm to generate MAZES! Silly fun program, found it in an old linux laptop of mine. Compile however you wish, then run through cmd/terminal, but make sure to zoom out using ctrl+mouse wheel before you call or it'll be all ugly! Added a .exe version which you can run directly through the cmd on Windows if you don't wish to compile.  

anyflip_to_pdf.py : Given an Anyflip book, download it in pdf format.  
Instructions:  
Create a new folder. Copy anyflip_to_pdf.py to the folder.  
Open the cmd in that folder.  
Your book should have an address of the form https://anyflip.com/X/Y/  , where X & Y are some words.  
In the cmd run "python X Y newpdfname numpagestodownloadfrombook"  
in order to download from https://anyflip.com/X/Y/  the first numpagestodownloadfrombook pages from the book, and save the result into newpdfname.pdf  
E.g. "python osliz fcwg StarWarsBook 295" in order to download from https://anyflip.com/osliz/fcwg/ the first 295 pages (i.e. all of them) into a new file StarWarsBook.pdf  
