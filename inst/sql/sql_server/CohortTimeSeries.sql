SELECT
  cohort_definition_id,
  DATEFROMPARTS(
    YEAR(cohort_start_date), 
    {@time_interval == 'month' | @time_interval == 'day'}?{MONTH(cohort_start_date)}:{CAST(1 AS int)},
    {@time_interval == 'day'}?{DAY(cohort_start_date)}:{CAST(1 AS int)}
   ) cohort_start_date,
  CAST('@time_interval' AS VARCHAR) time_interval,
  COUNT(DISTINCT subject_id) as subject_count,
  COUNT(*) as event_count
FROM @cohort_database_schema.@cohort_table cohort
{@cohort_definition_id != -1} ? {WHERE cohort.cohort_definition_id IN (@cohort_definition_id)}
GROUP BY
  cohort_definition_id,
  DATEFROMPARTS(
    YEAR(cohort_start_date), 
    {@time_interval == 'month' | @time_interval == 'day'}?{MONTH(cohort_start_date)}:{CAST(1 AS int)},
    {@time_interval == 'day'}?{DAY(cohort_start_date)}:{CAST(1 AS int)}
   )
ORDER BY
  cohort_definition_id, 
  DATEFROMPARTS(
    YEAR(cohort_start_date), 
    {@time_interval == 'month' | @time_interval == 'day'}?{MONTH(cohort_start_date)}:{CAST(1 AS int)},
    {@time_interval == 'day'}?{DAY(cohort_start_date)}:{CAST(1 AS int)}
   ) ASC
;