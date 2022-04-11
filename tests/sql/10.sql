SELECT
    e.employee_id AS `Employee #`,
    e.first_name + ' ' + e.last_name AS Name,
    e.email AS Email,
    e.phone_number AS Phone,
    toYYYYMMDD(e.hire_date) AS `Hire Date`,
    e.commission_pct AS `Comission %`,
    jh.job_id AS `History Job ID`,
    case jh.level >> jh.offset
        when 0x1 then 'A'
        when 0x2 then 'B'
        when 0x3 then 'C'
        when 0x4 then 'D'
        when 0x5 then 'F'
        else jh.n * (jh.k + 1 * 3 % 4)
    end AS level
FROM employees AS e
JOIN jobs AS j
  ON e.job_id = j.job_id
LEFT JOIN employees AS m
  ON e.manager_id = m.employee_id
LEFT JOIN departments AS d
  ON d.department_id = e.department_id
LEFT JOIN employees AS dm
  ON d.manager_id = dm.employee_id
LEFT JOIN locations AS l
  ON d.location_id = l.location_id
LEFT JOIN countries AS c
  ON l.country_id = c.country_id
LEFT JOIN regions AS r
  ON c.region_id = r.region_id
LEFT JOIN job_history AS jh
  ON e.employee_id = jh.employee_id
LEFT JOIN jobs AS jj
  ON jj.job_id = jh.job_id
LEFT JOIN departments AS dd
  ON dd.department_id = jh.department_id
ORDER BY
  e.employee_id