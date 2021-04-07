-- Get trends by month/year for all cohorts
SELECT 
  cohort_definition_id, 
  MONTH(cohort_start_date) as month, 
  YEAR(cohort_start_date) as year, 
  COUNT(DISTINCT subject_id) as subject_count,
  COUNT(*) as event_count
FROM @cohort_database_schema.@cohort_table
GROUP BY
  cohort_definition_id, 
  MONTH(cohort_start_date),
  YEAR(cohort_start_date)
ORDER BY
  cohort_definition_id, 
  YEAR(cohort_start_date),
  MONTH(cohort_start_date)
;
