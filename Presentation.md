<style>

.reveal .state-background {
   background: #FEDDD8;
}

.section .reveal .state-background {
  background: #FCC9C5;
} 

.reveal h3 { 
  font-size: 50px;
  font-import: http://fonts.googleapis.com/css?family=Risque;
  font-family: 'Risque';
  color: #CC313D;  
}

.reveal p { 
  font-size: 35px;
  font-import: http://fonts.googleapis.com/css?family=Risque;
  font-family: 'Risque';
  color: black;  
}

.reveal p b{ 
  font-size: 38px;
  font-import: http://fonts.googleapis.com/css?family=Risque;
  font-family: 'Risque';
  color: #CC313D;  
}

.reveal .slides section .slideContent h2 {
   font-size: 30px;
   font-import: http://fonts.googleapis.com/css?family=Risque;
  font-family: 'Risque';
   font-weight: bold;
   color: black;
}

.reveal h1 {
    font-size: 80px;
    font-weight: bold;
    font-import: http://fonts.googleapis.com/css?family=Risque;
    font-family: 'Risque';
}

.reveal ul, 
.reveal ol {
    font-size: 30px;
    font-import: http://fonts.googleapis.com/css?family=Risque;
    font-family: 'Risque';
    color: green;
    list-style-type: square;
}

</style>


University Course Finder
========================================================
transition: rotate

### **WIA1007 Group L**

## TAN ZI AN (U2102755)  
## OOI JIA MING (U2102759)  
## TAN XU YANG (U2102862)  
## MUHAMMAD ADAM MALIQUE BIN ZAINAL HABSAHRI (U2102866)


Why this application?
========================================================

**How can I get all universities information in 1 place?**

This is the question all of us had have during our high schools, and perhaps still having now!

**So we decided to develop this application to help our juniors out.**

The University Course Finder App is developed to enable university applicants to have easy access to all the Malaysian public university course official information, including <b>course overview</b>, <b>fee</b>, <b>duration</b> and <b>direct link</b> to the official course webpage.  

**Skateholders:** Malaysian university applicants


How?
========================================================


## To solve this problem, our team applied data science processes such as:
- Data acquisition ( Web scraping Malaysian public university websites )
- Cleaning data ( Dealing with inconsistent formats and missing values )
- Analysing data ( How the data can help future university applicants? )  

## Dataset description
- **10** attributes with **784** entries
- Character attributes: course, Link, Introduction, Address, Contact, uni_name, Fac_link
- Integer attributes: X, Duration, Fee

## *More detailed descriptions are in the codebook:*
## <https://rpubs.com/TanZiAn/MalaysianUniversityCodebook>

University Course Finder
========================================================

![Exe](presentation02.gif)  

## *Link to the shiny app:*
## <https://tanzian.shinyapps.io/UniversityCoursesFinder/>


Conclusion
========================================================

This project provides our team a great opportunity to acquire hands-on experiences in developing a data product. Since we built the Malaysian university dataset from scratch, we learnt how to apply the data science knowledge that we have learnt in this course such as web scraping and data cleaning, which are the essential skills of a good data scientist. It was definitely a fascinating experience to kick start our data scientist carrer!


## *Link to our project development GitHub repository:*
## <https://github.com/mjia0730/WIA1007>

