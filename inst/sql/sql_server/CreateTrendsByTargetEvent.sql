@event_time_window_table_create

@target_event_xref_table_create

CREATE TABLE #trends (
  cohort_definition_id BIGINT NOT NULL, 
  event_cohort_definition_id BIGINT NOT NULL,
  window_id INT NOT NULL,
  month INT NOT NULL,
  year INT NOT NULL,
	event_count BIGINT NOT NULL
);

INSERT INTO #trends (
  cohort_definition_id, 
  event_cohort_definition_id,
  window_id,
  month,
  year,
	event_count
)
SELECT 
  a.cohort_definition_id,
  a.event_cohort_definition_id,
  a.window_id,
  MONTH(a.cohort_start_date) as month, 
  YEAR(a.cohort_start_date) as year,   
  COUNT(DISTINCT a.subject_id) event_count
FROM (
  SELECT DISTINCT
  	ts.cohort_definition_id, 
  	ts.subject_id,
  	ts.cohort_start_date,
  	ts.cohort_end_date,
  	o.cohort_definition_id event_cohort_definition_id, 
  	ts.window_id
  from (
    SELECT * 
    FROM @cohort_database_schema.@cohort_table c
    INNER JOIN (SELECT DISTINCT target_cohort_id FROM #target_event_xref) te ON te.target_cohort_id = c.cohort_definition_id
    CROSS JOIN #event_windows e  
  ) ts 
  inner join (
  	SELECT *
  	FROM @cohort_database_schema.@cohort_table c
    INNER JOIN (SELECT DISTINCT event_cohort_id FROM #target_event_xref) te ON te.event_cohort_id = c.cohort_definition_id
  ) o ON o.subject_id = ts.subject_id
  INNER JOIN #target_event_xref te ON te.target_cohort_id = ts.target_cohort_id AND te.event_cohort_id = o.event_cohort_id
  WHERE DATEADD(day,CAST(ts.window_start as int),ts.cohort_start_date) <=  CASE WHEN ts.window_type = 'start' THEN o.cohort_start_date ELSE o.cohort_end_date END
  AND DATEADD(day,CAST(ts.window_end as int),ts.cohort_start_date) >= o.cohort_start_date 
) a
GROUP BY
  a.cohort_definition_id,
  a.event_cohort_definition_id,
  a.window_id,
  MONTH(a.cohort_start_date), 
  YEAR(a.cohort_start_date)  
ORDER BY
  a.cohort_definition_id,
  a.event_cohort_definition_id,
  a.window_id,
  YEAR(a.cohort_start_date), 
  MONTH(a.cohort_start_date)
;


@target_event_xref_table_drop

@event_time_window_table_drop
