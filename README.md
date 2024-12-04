# NCPES_linkage

This project completes the linkage of NCPES England results to NDRS cancer registry data. The project generates a table of linkage fields for each survey respondent, for both the adult and under-16 survey. The linkage is completed annually. The raw NCPES data and linkage fields are uploaded to CAS for use by NDRS analysts and as a requestable dataset. 

Project status: 
This project is in development. The code will be actively maintained annually by the analyst(s) completing the linkage. 


Point of contact: 
Claire Welsh claire.welsh8@nhs.net and Sophie Jose sophie.jose@nhs.net


Data requirements: 
- raw anonymised NCPES England data. This is supplied to the NHS England Insight and Voice Team by the survey provider, Picker. The Insight and Voice Team QA the data and send it to NDRS. The data are uploaded to CAS by NDRS developers. 
- cancer registry data. The project uses a CAS snapshot, with the timing to be determined by the analyst(s) completing the linkage. 


Outputs: 
The project produces a table of linkage fields which should be uploaded into the CPES schema on CAS by NDRS developers, along with the raw NCPES data. All analysts with Level2 CAS access should be given access to both tables once they are uploaded and the upload has been checked. 


Prerequisites:
Access to the raw NCPES data uploaded to CAS by NDRS developers. This requires being added to the CPESTEST user group.


How to install and use:
Clone the Github repo and run the scripts in numerical order


License:
MIT (see license file)


Other legal or regulatory requirements:
None

Other notes:
There are some patients that appeared to have been diagnosed after their discharge date. Many of these patients are diagnosed within 1-30 days after their discharge. For this reason, patients who are diagnosed over 30 days after their discharge date in the CPES dataset are not indicated as a match. This helps to remove any records that are deemed ‘duplicates’ (by duplicate we mean where a patient had two or more records that were a match on ICD-10 4-digit or 3-digit), but also to remove any record that matched, but had a date of diagnosis more than 30 days after discharge to minimise the possibility of error. 

For certain tumours a more stringent rule in terms of time between diagnosis and discharge is necessary given the large variance across this group. These cases are considered a match if the time between diagnosis and discharge was within the mean time frame for the most relevant tumour.  So for instance, the average length of time between diagnosis and discharge among matched breast cancer patients was 546 days in 2010 (considering just the positive time interval values). Therefore, cases that were coded ‘C50’ in CPES and ‘D05’ in the AT_TUMOUR_ENGLAND, and vice-versa, with a time difference from diagnosis and discharge date between -30 days and 546 days were considered a related match and flagged. This flag variable is called ‘FLAG_RELATED_MATCH’ and this process is applied in 5_non_matched.R.

The same applies to colorectal tumours, but these are grouped together before the mean difference between diagnosis and discharge is identified, also in 5_non_matched.R.