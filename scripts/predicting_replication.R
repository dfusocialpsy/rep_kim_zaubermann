library(dplyr)
getwd()
txt <- read.delim("C:/Users/Clemens Lindner/Downloads/test.txt")

stringr::str_replace_all(
  read.delim("C:/Users/Clemens Lindner/Downloads/test.txt"),
  "[\r\n]",""
) %>%
  stringr::str_extract(.,"t1(.*?\\)")



statcheck::checkPDF("C:/Users/Clemens Lindner/Downloads/Roelofs_2008_Tracing.pdf")

statcheck::statcheck("F(1, 23) = 2.77, p = .11")

pdftotext "C:/Users/Clemens Lindner/Downloads/Roelofs_2008_Tracing.pdf" test.txt
options(statcheck.xpdf.path = "C:/Path/To/pdftotext.exe")

options(statcheck.xpdf.path = "C:/Users/Clemens Lindner/Downloads/xpdf-tools-win-4.05/bin64/pdftotext.exe")

statcheck::checkPDF("C:/Users/Clemens Lindner/Downloads/Roelofs_2008_Tracing.pdf")
statcheck::statcheck("C:/Users/Clemens Lindner/Downloads/text.txt")



## S NNUM 1 Roelofs et al.  ----

### Experiment 1 ----

statcheck::statcheck("F(1, 23) = 12.14, p = .002")
F(1, 38)= 5.59,MSE= 1,436, p= .02
F(1, 23)= 6.86, MSE= 1,069, p= .02 
F(1, 38) = 1,MSE= 7,571, p= .40 

F(1,23)= 1.60, MSE= 1,401, p= .22 
F(1, 38)= 1.43, MSE= 1,436, p= .24

F(1, 23)  2.77, p  .11; 
F(1, 38)  4.98, p  .03.

t1(23)  1.0, p  .33; 
t2(19)  1.17, p .26, 
t1(23)  3.45, p  .002;
t2(19)  2.05, p  .05.

F1(1, 23)  23.14, MSE  709,p  .001; 
F2(1, 38)  5.72, MSE  2,512, p  .02
F1(1, 23)  6.58, MSE  1,206, p  .02;
F2(1, 38)  1, MSE 7,374, p  .40. 

F1(1, 23)  1,MSE  774, p  .88; 
F2(1, 38)  1, MSE  2,512, p  .80.

t1(23)  3.47, p  .002;
t2(19)  1.91, p  .07, 
t1(23)  3.21, p  .004;
t2(19)  1.60,p  .13.

F(9, 207) 1.18, MSE  3,266, p  .31, 
F(9, 207)  3.73, MSE  3,055, p  .001.


### Experiment 2 ----

F1(1, 17)  1, MSE 2,268, p  .62;
F2(1, 38)  1, MSE  2,640, p  .74, 
F1(1, 17)  18.57, MSE  2,039, p  .001;
F2(1, 38)  4.36, MSE  9,033, p  .04. 
F1(1, 17)  1.53, MSE  2,313, p  .23; 
F2(1, 38)  1.06, MSE  2,640, p  .31.

t1(17)  1.26,p  .23; 
t2(19)  1.24, p  .23, 
t1(17)  0.52, p  .61;
t2(19)  0.41, p  .68.

F1(1, 17)  1, MSE  10,018, p .83; 
F2(1, 38)  1, MSE  10,626, p  .84, 
F1(1, 17)  8.11, MSE  4,812, p  .01; 
F2(1, 38)  2.06, MSE  24,997, p  .16. 
F1(1, 17)  1, MSE  2,684, p  .65; 
F2(1, 38)  1,MSE  10,626, p  .73.

t1(17)  0.00, p  .99; 
t2(19) 0.10, p  .91, 
t1(17)  0.39, p  .70; 
t2(19)  0.35, p  .73.

F(9, 153)  0.19, MSE 10,814, p  .95, 
F(9, 153)  1.15, MSE  2,958, p  .33.


### Experiment 3 ----

F1(1, 13)  344.52, MSE 3,732, p  .001; 
F2(1, 76)  260.54, MSE  7,023, p  .001,
F1(1, 13)  6.51, MSE  3,176, p  .02; 
F2(1, 76)  4.15,MSE  7,023, p  .04, 
F1(1, 13)  5.92, MSE 1,537, p  .03; 
F2(1, 76)  3.53, MSE  3,071, p  .06.
F1(1, 13)  60.80, MSE  675, p  .001; 
F2(1, 76)  8.15, MSE  7,023, p .006,
F1(1, 13)  7.11, MSE  1,234, p  .02;
F2(1, 76)  3.72, MSE  3,071, p  .06, 
F1(1, 13)  1, MSE  2,509, p  .89; 
F2(1, 76)  1, MSE  3,071, p  .91.
F1(1, 13)  29.56, MSE  2,029, p  .001;
F2(1, 38)  8.15, MSE  10,318, p  .007, 
F1(1, 13)  10.63, MSE  1,680, p  .006; 
F2(1, 38)  6.14, MSE  3,623, p  .02. 
F1(1, 13)  1,MSE  1,758, p  .97; 
F2(1, 38)  1, MSE  3,623, p  .93.

t1(13)  2.39, p  .03;
t2(19)  1.59, p  .13, 
t1(13)  2.18, p  .05; 
t2(19)  1.94, p  .07.

t1(13)  0.22, p  .83; 
t2(19)  0.14, p  .88, 
t1(13)  0.20, p  .84; 
t2(19)  0.24, p  .81.

F1(1, 13)  172.86, MSE  6,750,p  .001; 
F2(1, 76)  171.52, MSE  9,780, p  .001, 
F1(1, 13)  5.37, MSE  3,257, p  .04; 
F2(1, 76)  5.45,MSE  4,381, p  .02, 
F1(1, 13)  1.01, MSE 3,459, p  .33; 
F2(1, 76)  1, MSE  9,780, p  .44. 
F1(1, 13)  17.42, MSE 2,144, p  .001; 
F2(1, 76)  5.55, MSE  9,780, p  .021, 
F1(1, 13)  7.66, MSE  1,731, p  .02; 
F2(1, 76)  4.54, MSE  4,381, p  .04, 
F1(1, 13)  1, MSE  2,061, p  .46; 
F2(1, 76)  1, MSE  4,381, p  .59. 
F1(1, 13)  11.57, MSE  2,751, p  .005;


F2(1, 38)  3.03, MSE  15,997, p  .09, 
F1(1, 13)  10.83, MSE  2,826, p  .006; 
F2(1, 38)  7.22, MSE  6,048, p  .011.
F1(1, 13)  1, MSE  2,331, p  .63; 
F2(1, 38)  1, MSE  6,048, p  .80. 
t1(13)  2.76, p  .02; 
t2(19)  1.59, p  .13,
t1(13)  2.32, p  .04;
t2(19)  2.29, p  .03. 
F1(1, 13)  3.16, MSE  2,851, p  .10; 
F2(1, 38)  3.31, MSE  3,664, p  .08, 

t1(13)  0.26, p  .80; 
t2(19)  0.26, p  .80, 
t1(13)  0.52, p  .61;
t2(19)  0.57, p  .58. 

F(9, 117)  5.82, MSE  4,862, p  .001, 
F(9, 117)  1.74, MSE  1,731, p  .09.

F(9, 117)  1.87, MSE  6,012, p  .06,
F(9, 117)  0.50, MSE  3,065, p  .87.



## SNUM#2 Morris et al. -----

### Experiment 1 ----

F1(1, 23)  66.22, p < .001
F2(1, 92)  201.99, p < .001



F1(1, 23)  4.63, p  .05, 
F2(1, 92)  33.18,p  .001.

F1(1, 23)  110.85, p < .001
F2(1, 92)  103.43, p < .001 

F1(1, 23)  7.58,  p < .05
F2(1, 92)  5.68,  p < .05 

F1(1, 23)  6.44,p < .05
F2(1, 92)  4.28,p < .05

F1(1, 23)  7.83, p < .05,
F2(1, 46)  102.54, p < .001.

F(1, 46)  12.75, p < .005; 
F(1, 23)  2.57, p = .12.

F1(1, 23)  236.19, p < .001
F2(1, 94)  318.49, p < .001

F1(1, 23)  62.33, p < .001
F2(1, 94)  62.85, p < 
  
F1(1, 23)  15.22, p < .005, 
F2(1, 94)  8.47, p < .01.

### Experiment 2 ----

F1(1, 23)  4.73, p < .05
F2(1, 88)  5.40, p < .05

F1(1, 23)  10.63, p < .005,
F2(1, 88)  37.92, p < .001. 
F1(1, 23)  8.57, p < .01,
F2(1, 88)  130.39, p < .001.

F1(1, 23)  6.52, p < .05
F2(1, 88)  6.90, p < .05

F1(1, 23)  5.22, p < .05, 
F2(1, 88)  16.09, p < .001.

F1(1, 23)  263.87, p < .001.
F2(1, 88)  243.99, p < .001.


F1(1, 23)  72.23,  p < .001
F2(1, 88)  316.15, p < .001 

F1(1, 23)  54.97,  p < .001
F2(1, 88)  94.05,  p < .001

F1(1, 23)  15.06, p < .005,
F2(1, 44) 35.63, p < .001;

F1(1, 23)  99.30,  p < .001
F2(1, 44) 348.54, p < .001

F1(1, 23)  6.76,p < .05
F2(1, 88)  4.60,p < .05

F1(1, 23)  14.42, p < .005
F2(1, 88)  10.52, p < .005

F1(1, 22)  405.84, 
F2(1, 92)  356.36, both ps 
.001. The main effect of condition was significant such that the
intervening item was reported at a higher rate when it appeared in
the context of repeated items, F1(1, 22)  74.48, F2(1, 92) 
110.70, both ps  .001. There was also a significant Task Order 
Block interaction, F1(1, 22)  7.15, p  .05, F2(1, 92)  15.60,
p  .001, which indicates that the intervening item was reported at
a higher rate when the identification task occurred first. This likely
reflects the effect of rapid forgetting of the intervening item when
the identification task was performed following the same/different
task. Finally, there was a significant Condition  Lexicality 
Block interaction, F1(1, 22)  8.95, p  .01, F2(1, 92)  9.57, p 
.005.