--pulls enrollment records for the past 5 years
SELECT DISTINCT
  BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.STUDENT_ID,
  BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.INST_ATT_CRED + BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.DEV_ATT_CRED as tot_att_cred,
  BI_DIM_TERMS_COHORTS.TERM_REPORTING_YEAR,
  BI_DIM_SNAPSHOT_DATES.TERM_ID,
  BI_FACT_COURSE_SECTION_SNAPSHOTS.CRED_TYPE_DESC,
  BI_DIM_EDUC_GOALS.EDUC_GOAL_ID,
  BI_DIM_EDUC_GOALS.EDUC_GOAL_DESC,
  BI_DIM_GENDERS.GENDER,
  case when BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.IPEDS_RACE_ETHNIC_DESC = 'Black or African American' then 'Black/African American'
        when BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.IPEDS_RACE_ETHNIC_DESC = 'Hispanic' then 'Latinx'
       else BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.IPEDS_RACE_ETHNIC_DESC
end  as race,
  CASE 
WHEN ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) < 18 THEN '17 & Below' 
WHEN ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) >= 18 And ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) <= 24 THEN '18-24'
WHEN ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) >= 25 And ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) <= 39 THEN '25-39'
WHEN ( Cast(Round(DATEDIFF(DD,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.BIRTH_DATE,BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE) / 365.25,0,1) AS INT) ) >= 40 THEN '40 & Over'
ELSE 'Unknown' END as age,
  BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SB15_TERM_STATUS,
  upper(BI_FACT_COURSE_SECTION_SNAPSHOTS.SEC_SUBJECT)+'-'+BI_FACT_COURSE_SECTION_SNAPSHOTS.SEC_COURSE_NO as crs_name
FROM
  BI_DIM_TERMS_COHORTS INNER JOIN BI_DIM_SNAPSHOT_DATES ON (BI_DIM_SNAPSHOT_DATES.TERM_ID=BI_DIM_TERMS_COHORTS.TERMS_ID)
   INNER JOIN BI_FACT_TERM_ENROLLMENT_SNAPSHOTS ON ( BI_DIM_SNAPSHOT_DATES.DATE_RANGE = BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE_DESC
and BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE_TERM=BI_DIM_SNAPSHOT_DATES.TERM_ID)
   INNER JOIN BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS ON ( BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.SELECT_DATE_DESC  = BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE_DESC
and BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE_TERM=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.SELECT_DATE_TERM
and BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.STUDENT_TERMS_ID=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.STUDENT_TERMS_ID)
   INNER JOIN BI_DIM_STC_STATUSES ON (BI_DIM_STC_STATUSES.STC_STATUS_DIM_ID=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.STC_STATUS_DIM_ID)
   INNER JOIN BI_FACT_COURSE_SECTION_SNAPSHOTS ON (BI_FACT_COURSE_SECTION_SNAPSHOTS.COURSE_SECTIONS_ID=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.COURSE_SECTIONS_ID and BI_FACT_COURSE_SECTION_SNAPSHOTS.SNAPSHOT_SELECT_DATE=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.SELECT_DATE and BI_FACT_COURSE_SECTION_SNAPSHOTS.SNAPSHOT_NAME=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.SELECT_DATE_DESC and BI_FACT_COURSE_SECTION_SNAPSHOTS.TERM_ID=BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.SELECT_DATE_TERM)
   INNER JOIN BI_DIM_GENDERS ON (BI_DIM_GENDERS.GENDER_DIM_ID=BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.GENDER_DIM_ID)
   INNER JOIN BI_DIM_STTR_STATUSES ON (BI_DIM_STTR_STATUSES.TERM_STATUS_DIM_ID=BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.TERM_STATUS_DIM_ID)
   INNER JOIN BI_FACT_TERM_ENROLLMENT_EDUC_GOALS_SNAPSHOTS ON (BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE=BI_FACT_TERM_ENROLLMENT_EDUC_GOALS_SNAPSHOTS.SELECT_DATE and BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.SELECT_DATE_DESC=BI_FACT_TERM_ENROLLMENT_EDUC_GOALS_SNAPSHOTS.SELECT_DATE_DESC and BI_FACT_TERM_ENROLLMENT_SNAPSHOTS.STUDENT_TERMS_ID=BI_FACT_TERM_ENROLLMENT_EDUC_GOALS_SNAPSHOTS.STUDENT_TERM_ID)
   INNER JOIN BI_DIM_EDUC_GOALS ON (BI_FACT_TERM_ENROLLMENT_EDUC_GOALS_SNAPSHOTS.EDUC_GOAL_DIM_ID=BI_DIM_EDUC_GOALS.EDUC_GOAL_DIM_ID)
  
WHERE
  (
   BI_DIM_TERMS_COHORTS.TERM_REPORTING_YEAR  BETWEEN  2017  AND  2021
   AND
   CASE
  WHEN BI_FACT_COURSE_SECTION_SNAPSHOTS.SEC_NO like '1%' THEN 'San Jose City College'
  WHEN BI_FACT_COURSE_SECTION_SNAPSHOTS.SEC_NO like '2%' THEN 'Evergreen College'
  WHEN BI_FACT_COURSE_SECTION_SNAPSHOTS.SEC_NO is null THEN 'Unknown'
  ELSE 'Other Location' END  IN  ( 'San Jose City College'  )
   AND
   ( ( BI_DIM_STTR_STATUSES.TERM_STATUS ) IN ('P','R','S','T')
AND
(
(( BI_DIM_STC_STATUSES.STC_STATUS ) IN ('A','N'))
OR
(( BI_DIM_STC_STATUSES.STC_STATUS ) = 'D' AND ( BI_FACT_SECTION_ENROLLMENT_SNAPSHOTS.FINAL_GRADE ) IN ('W', 'CW'))
)  )
   AND
   ( BI_DIM_SNAPSHOT_DATES.DATE_RANGE = '1 Month After End'  )
  )
