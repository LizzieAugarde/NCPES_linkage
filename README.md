# NCPES_linkage

This project completes the linkage of NCPES England results to NDRS cancer registry data. The project generates a table of linkage fields for each survey 
respondent, for both the adult and under-16 survey. The linkage is completed annually. The raw NCPES data and linkage fields are uploaded to CAS for use 
by NDRS analysts and as a requestable dataset. 

Project status: 
This project is in development. The code will be actively maintained annually by the analyst(s) completing the linkage. 


Point of contact: 
Lizzie Augarde elizabeth.augarde2@nhs.net


Data requirements: 
- raw anonymised NCPES England data. This is supplied to the NHS England Insight and Voice Team by the survey provider, Picker. The Insight and Voice Team 
QA the data and send it to NDRS. The data are uploaded to CAS by NDRS developers. 
- cancer registry data. The project uses a CAS snapshot, with the timing to be determined by the analyst(s) completing the linkage. 


Outputs: 
The project produces a table of linkage fields which should be uploaded into the CPES schema on CAS by NDRS developers, along with the raw NCPES data. All analysts with Level2 CAS access should be given access to both tables. 


Prerequisites:
- access to the raw NCPES data uploaded to CAS by NDRS developers. This requires being added to the CPESTEST user group.


How to install and use:
Clone the Github repo and run the scripts in numerical order TBC


License:
MIT (see license file)


Other legal or regulatory requirements:
TBC
